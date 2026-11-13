source("scripts/00_config.R")


raw_root <- path.expand(Sys.getenv("RTI_RAW"))
if (!nzchar(raw_root) || !dir.exists(raw_root)) stop("Set RTI_RAW to the shared Drive rti/ folder.")
freeze_dir <- file.path(raw_root, raw_freeze)

emails <- readr::read_rds(here::here("data", "private", "emails.rds"))
register <- readr::read_rds(here::here("data", "private", "ra_register.rds"))
assignments <- read_assignments()
overrides <- readr::read_csv(file.path(freeze_dir, "overrides.csv"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE
) |>
  dplyr::filter(!is.na(message_id), nzchar(message_id)) |>
  dplyr::mutate(application_id = stringr::str_to_upper(stringr::str_trim(application_id)))
private_stop <- normalize_name(paste(readLines(file.path(freeze_dir, "stop_terms.txt"), warn = FALSE),
  collapse = " "
))
stop_words <- c(match_stop_words, stringr::str_split(private_stop, " ")[[1]])

# Attachment text ----
## pdftotext first, English OCR when the text layer is empty. Cached in data/private/.
text_cache <- here::here("data", "private", "attachment_text")
dir.create(text_cache, recursive = TRUE, showWarnings = FALSE)
attachment_files <- list.files(
  file.path(raw_root, "email_tracker", "attachments"),
  recursive = TRUE, full.names = TRUE
)

attachment_text <- function(a) {
  cache <- file.path(text_cache, paste0(a$drive_id, ".txt"))
  if (file.exists(cache)) {
    return(paste(readLines(cache, warn = FALSE), collapse = "\n"))
  }
  path <- attachment_files[basename(attachment_files) == dplyr::coalesce(a$saved_as, "\001")]
  if (length(path) == 0) {
    return("")
  }
  path <- path[1]
  text <- ""
  if (stringr::str_detect(path, stringr::regex("\\.pdf$", ignore_case = TRUE))) {
    pdf_args <- c("-layout", shQuote(path), "-")
    text <- paste(system2("pdftotext", pdf_args, stdout = TRUE, stderr = FALSE), collapse = "\n")
    if (nchar(stringr::str_remove_all(text, "\\s")) < 50 && nzchar(Sys.which("tesseract"))) {
      tmp <- tempfile("ocr")
      dir.create(tmp)
      system2("pdftoppm", c("-r", "150", "-l", "3", "-png", shQuote(path), file.path(tmp, "p")),
        stdout = FALSE, stderr = FALSE
      )
      pages <- sort(list.files(tmp, "p.*\\.png$", full.names = TRUE))
      text <- paste(unlist(purrr::map(pages, function(p) {
        system2("tesseract", c(shQuote(p), "stdout", "-l", "eng"), stdout = TRUE, stderr = FALSE)
      })), collapse = "\n")
      unlink(tmp, recursive = TRUE)
    }
  }
  writeLines(text, cache)
  text
}

# Indexes ----
links <- new.env(parent = emptyenv())
links$app_id <- rep("", nrow(emails))
links$method <- rep("", nrow(emails))
links$confidence <- rep("", nrow(emails))

reg_index <- new.env(parent = emptyenv())
set_reg <- function(reg, aid) {
  if (nzchar(reg) && !exists(reg, envir = reg_index, inherits = FALSE)) {
    assign(reg, aid, envir = reg_index)
  }
}
get_reg <- function(reg) {
  if (nzchar(reg) && exists(reg, envir = reg_index, inherits = FALSE)) {
    get(reg, envir = reg_index)
  } else {
    ""
  }
}
for (i in seq_len(nrow(register))) set_reg(register$reg_no[i], register$app_id[i])

pio_index <- new.env(parent = emptyenv())
add_pio <- function(email, aid) {
  if (nzchar(email)) assign(email, dplyr::union(get_pio(email), aid), envir = pio_index)
}
get_pio <- function(email) {
  if (nzchar(email) && exists(email, envir = pio_index, inherits = FALSE)) {
    get(email, envir = pio_index)
  } else {
    character()
  }
}
for (i in seq_len(nrow(register))) {
  for (e in register$pio_emails[[i]]) add_pio(e, register$app_id[i])
}

assign_event <- function(i, aid, how, conf) {
  links$app_id[i] <- aid
  links$method[i] <- how
  links$confidence[i] <- conf
}

reg_by_app <- setNames(register$reg_no, register$app_id)
person_by_app <- setNames(register$person, register$app_id)
people <- unique(register$person)

# 1. Acknowledgments: registration number, else the authority name the portal printed ----
for (i in which(emails$kind == "filing_ack")) {
  f <- emails$fields[[i]]
  reg <- dplyr::coalesce(f$reg_no, "")
  hit <- get_reg(reg)
  if (nzchar(hit)) {
    assign_event(i, hit, "reg_no", "high")
  } else {
    st <- emails$portal_state[i]
    pool_states <- if (nzchar(st) && st != "CENTRAL") st else c("DL", "MH")
    pool <- assignments |> dplyr::filter(state %in% pool_states)
    filer <- stringr::str_to_lower(dplyr::coalesce(f$filer_name, ""))
    named <- function(p) nzchar(filer) && stringr::str_detect(filer, stringr::fixed(p))
    person <- people[vapply(stringr::str_to_lower(people), named, TRUE)]
    if (length(person) > 0) {
      mine <- pool |> dplyr::filter(dplyr::coalesce(person_by_app[app_id], "") == person[1])
      if (nrow(mine) > 0) pool <- mine
    }
    open_rows <- pool |> dplyr::filter(!nzchar(dplyr::coalesce(reg_by_app[app_id], "")))
    if (nrow(open_rows) > 0) pool <- open_rows
    target <- normalize_name(f$authority)
    scores <- vapply(normalize_name(pool$office_name), \(o) compute_name_similarity(target, o), 0)
    ord <- order(-scores)
    clear_winner <- length(ord) == 1 || scores[ord[2]] < scores[ord[1]] - 0.05
    if (length(ord) > 0 && scores[ord[1]] >= 0.9 && clear_winner) {
      assign_event(i, pool$app_id[ord[1]], "authority_name", "medium")
    }
  }
  if (nzchar(links$app_id[i])) {
    set_reg(reg, links$app_id[i])
    add_pio(dplyr::coalesce(f$pio_email, ""), links$app_id[i])
  }
}

# 2. Transfers carry the old and new registration numbers ----
for (i in which(emails$kind == "transferred")) {
  f <- emails$fields[[i]]
  hit <- get_reg(dplyr::coalesce(f$reg_no, ""))
  if (nzchar(hit)) {
    assign_event(i, hit, "reg_no", "high")
    for (new_reg in unlist(f$new_reg_nos)) set_reg(new_reg, hit)
  }
}

# 3. Portal status mail by registration number ----
status_kinds <- c(
  "disposed", "returned", "rejected", "appeal_ack", "fee_demand", "document_required",
  "portal_other"
)
for (i in which(emails$kind %in% status_kinds & !nzchar(links$app_id))) {
  f <- emails$fields[[i]]
  hits <- vapply(c(dplyr::coalesce(f$reg_no, ""), unlist(f$reg_nos)), get_reg, "")
  hits <- hits[nzchar(hits)]
  if (length(hits) > 0) assign_event(i, hits[[1]], "reg_no", "high")
}

# 4. Telangana acknowledgments carry no number: match on the filing time the RA recorded ----
tg_rows <- register |> dplyr::filter(stringr::str_starts(app_id, "TG"), !is.na(sheet_filing_at))
used <- character()
for (i in which(emails$kind == "filing_ack_tg")) {
  open_tg <- tg_rows |> dplyr::filter(!app_id %in% used)
  if (nrow(open_tg) == 0) next
  gaps <- abs(as.numeric(difftime(open_tg$sheet_filing_at, emails$at[i], units = "secs")))
  ord <- order(gaps, open_tg$app_id)
  ord <- ord[gaps[ord] <= 180]
  if (length(ord) >= 1 && (length(ord) == 1 || gaps[ord[2]] - gaps[ord[1]] > 60)) {
    assign_event(i, open_tg$app_id[ord[1]], "tg_filing_time", "medium")
    used <- c(used, open_tg$app_id[ord[1]])
  }
}

# 5. Mail from people and offices ----
filed_info <- list()
for (i in seq_len(nrow(register))) {
  if (nzchar(register$reg_no[i])) {
    aid <- register$app_id[i]
    office <- assignments$office_name[match(aid, assignments$app_id)]
    filed_info[[aid]] <- list(
      office = dplyr::coalesce(office, ""),
      dates = as.character(na.omit(register$sheet_filing_date[i]))
    )
  }
}
for (i in which(emails$kind == "filing_ack" & nzchar(links$app_id))) {
  aid <- links$app_id[i]
  if (is.null(filed_info[[aid]])) {
    office <- assignments$office_name[match(aid, assignments$app_id)]
    filed_info[[aid]] <- list(office = dplyr::coalesce(office, ""), dates = character())
  }
  filed_info[[aid]]$dates <- dplyr::union(filed_info[[aid]]$dates, as.character(emails$date[i]))
}

for (i in which(emails$kind %in% c("pio_email", "sent_by_us") & !nzchar(links$app_id))) {
  f <- emails$fields[[i]]
  atts <- emails$attachments[[i]]
  texts <- vapply(atts, attachment_text, "")
  regs <- c(unlist(f$reg_nos), unlist(purrr::map(texts, extract_registration_numbers)))
  hits <- vapply(as.character(regs), get_reg, "")
  hits <- hits[nzchar(hits)]
  if (length(hits) > 0) {
    assign_event(i, hits[[1]], "reg_no_in_text", "high")
    next
  }
  cands <- get_pio(emails$sender[i])
  if (length(cands) == 1) {
    assign_event(i, cands, "pio_email", "high")
    next
  }
  blob <- paste(
    c(emails$sender_name[i], stringr::str_extract(emails$sender[i], "^[^@]*"), emails$subject[i], texts),
    collapse = " "
  )
  guess <- match_office_and_date(blob, filed_info, stop_words, restrict = cands)
  if (nzchar(guess)) assign_event(i, guess, "office_and_date", "low")
}

# 6. Hand overrides have the last word ----
hit <- match(emails$msg_id, overrides$message_id)
for (i in which(!is.na(hit))) {
  assign_event(
    i, overrides$application_id[hit[i]], "override",
    "manual"
  )
}

# 7. Fixes from reading unmatched mail during the data audit ----
fixes <- readr::read_csv(here::here("data", "hand", "match_fixes.csv"),
  col_types = readr::cols(.default = "c"), show_col_types = FALSE
)
hit <- match(emails$msg_id, fixes$message_id)
for (i in which(!is.na(hit))) assign_event(i, fixes$app_id[hit[i]], "audit_fix", "manual")
stopifnot(all(fixes$message_id %in% emails$msg_id))

# Save ----
matched <- emails |> dplyr::mutate(
  app_id = links$app_id, method = links$method, confidence = links$confidence
)
readr::write_rds(matched, here::here("data", "private", "events_matched.rds"))

derived_kinds <- c(
  "filing_ack", "filing_ack_tg", "transferred", "disposed", "returned", "rejected", "pio_email",
  "fee_demand", "appeal_ack", "document_required"
)
events_out <- matched |>
  dplyr::filter(kind %in% derived_kinds) |>
  dplyr::transmute(
    msg_id, date, kind, app_id, method, confidence,
    has_attachment = lengths(attachments) > 0,
    mentions_fee = purrr::map_lgl(fields, \(x) isTRUE(x$mentions_fee)),
    fee_amount_inr = purrr::map_chr(fields, \(x) dplyr::coalesce(x$amount_inr, ""))
  )
readr::write_csv(events_out, here::here("data", "derived", "events.csv"), na = "")

print(events_out |> dplyr::mutate(matched = nzchar(dplyr::coalesce(app_id, ""))) |> dplyr::count(kind, matched))
print(dplyr::count(dplyr::filter(events_out, nzchar(dplyr::coalesce(app_id, ""))), method, confidence))
