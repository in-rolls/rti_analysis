original_folders <- c(
  "email_tracker/raw", "email_tracker/attachments", "rti responses",
  "Filed_RTI"
)

## Paths can hold inbox and filer names, so the manifest in git stores a hash of each path
hash_path <- function(path) {
  vapply(path, digest::digest, "", algo = "md5", serialize = FALSE, USE.NAMES = FALSE)
}

list_originals <- function(raw_root, folder) {
  files <- list.files(file.path(raw_root, folder), recursive = TRUE, full.names = TRUE)
  rel <- stringr::str_remove(files, stringr::fixed(paste0(raw_root, "/")))
  dplyr::tibble(folder = folder, path = rel, path_md5 = hash_path(rel), bytes = file.size(files))
}

## The files in a folder that the manifest pins, and a stop if any pinned file has gone
find_pinned_originals <- function(raw_root, folder, manifest) {
  pins <- manifest |> dplyr::filter(folder == !!folder)
  found <- list_originals(raw_root, folder) |>
    dplyr::select(path, path_md5) |>
    dplyr::inner_join(dplyr::select(pins, path_md5, md5), by = "path_md5")
  if (nrow(found) != nrow(pins)) stop("Originals missing from ", folder)
  found
}

# Portal senders ----
## Registration numbers decide the application; the sender only says which template to expect.
portal_senders <- c(
  "rtisupport@tn.gov.in" = "TN", "rtionline-kar@nic.in" = "KA", "tsrtinoreply@gmail.com" = "TG",
  "noreply-rtimh@mah.gov.in" = "MH", "noreply-dopt@gov.in" = "CENTRAL"
)
portal_domains <- c(".gov.in", ".nic.in")
system_domains <- c("google.com", "accounts.google.com", "googlemail.com")

# Patterns ----
## No word boundaries at the ends: portal text glues the next label on ("/60009Name:").
## Office codes can hold an ampersand (DD&DC, HG&CD), written &amp; in some mail
reg_re <- paste0(
  "(?<![A-Z&])([A-Z&]{4,9}|[A-Z]{2}&amp;[A-Z]{2})\\s*/\\s*([RA])\\s*/\\s*(20\\d\\d)\\s*/\\s*",
  "(\\d{4,6})(?!\\d)"
)
tg_reg_re <- "\\bTGRTI/APP/[A-Z&]+/\\d{2}/\\d{2}/\\d{2}/[A-Z]+/\\d+\\b"
email_re <- "[\\w.+-]+@[\\w-]+(?:\\.[\\w-]+)+"
date_re <- "\\b(\\d{1,2})[./-](\\d{1,2})[./-](20\\d\\d)\\b"
app_re <- "^(TN|TG|KA|DL|MH)-\\d{3,4}$"
fee_re <- stringr::regex(paste0(
  "\\bfees?\\b|additional (?:fee|cost|amount)|postal order|\\bIPO\\b|demand draft|",
  "\\bD\\.?D\\.?\\b|challan|remit|per page|photocopy charges"
), ignore_case = TRUE)
## An office writing about an RTI, in English, Marathi, Hindi, Kannada or Tamil
rti_words_re <- paste0(
  "\\brti\\b|right to information|information sought|petition|",
  "\u092e\u093e\u0939\u093f\u0924\u0940(?:\u091a\u093e)? \u0905\u0927\u093f\u0915\u093e\u0930|",
  "\u0938\u0942\u091a\u0928\u093e \u0915\u093e \u0905\u0927\u093f\u0915\u093e\u0930|",
  "\u0cae\u0cbe\u0cb9\u0cbf\u0ca4\u0cbf \u0cb9\u0c95\u0ccd\u0c95\u0cc1|",
  "\u0ba4\u0b95\u0bb5\u0bb2\u0bcd \u0b85\u0bb1\u0bbf\u0baf\u0bc1\u0bae\u0bcd ",
  "\u0b89\u0bb0\u0bbf\u0bae\u0bc8"
)

# Text helpers ----
## Undo the portals' non-breaking-space damage and normalize whitespace within lines
clean_text <- function(x) {
  x <- ifelse(is.null(x) || is.na(x), "", x)
  x <- stringr::str_replace_all(x, "\r\n?", "\n")
  x <- stringr::str_replace_all(x, "\u00c2\u00a0", " ")
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, "\u00c2", " ")
  stringr::str_replace_all(x, "[ \t]+", " ")
}

extract_email_sender <- function(from) {
  from <- ifelse(is.null(from) || is.na(from), "", from)
  matches <- stringr::str_match(from, "<([^>]+)>")[, 2]
  stringr::str_to_lower(stringr::str_trim(ifelse(is.na(matches), from, matches)))
}

## Prefer the HTML rendering, where cell separators survive; else plain text
extract_email_body <- function(rec) {
  h <- clean_text(rec$body_html_text)
  p <- clean_text(rec$body_text)
  if (nchar(h) >= nchar(p) * 0.8 && nzchar(stringr::str_trim(h))) h else p
}

extract_registration_numbers <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(text)) {
    return(character())
  }
  matches <- stringr::str_match_all(text, reg_re)[[1]]
  if (nrow(matches) == 0) {
    return(character())
  }
  codes <- stringr::str_replace_all(matches[, 2], stringr::fixed("&amp;"), "&")
  unique(paste(codes, matches[, 3], matches[, 4], matches[, 5], sep = "/"))
}

## A registration number as typed into a sheet: tabs, entities, spaces, case
normalize_registration <- function(x) {
  x <- ifelse(is.null(x) || is.na(x), "", x)
  x <- stringr::str_replace_all(x, stringr::fixed("&#9;"), " ")
  x <- stringr::str_replace_all(x, stringr::fixed("&amp;"), "&")
  x <- stringr::str_replace_all(stringr::str_to_upper(stringr::str_trim(x)), "\\s+", "")
  found <- extract_registration_numbers(x)
  if (length(found) > 0) {
    return(found[1])
  }
  tg <- stringr::str_extract(x, tg_reg_re)
  if (is.na(tg)) x else tg
}

## A real registration number has a digit and a slash; RAs sometimes type a note in that cell
is_real_reg <- function(x) {
  stringr::str_detect(dplyr::coalesce(x, ""), "\\d") &
    stringr::str_detect(dplyr::coalesce(x, ""), "/")
}

## PIO addresses as typed into the portal: "x @ gmail.com", "x@.domain.net"
fix_email <- function(x) {
  x <- stringr::str_replace_all(stringr::str_trim(dplyr::coalesce(x, "")), "\\s*@\\s*", "@")
  x <- stringr::str_replace_all(x, "\\s*\\.\\s*", ".")
  x <- stringr::str_replace_all(x, stringr::fixed("@."), "@")
  matches <- stringr::str_extract(x, email_re)
  if (is.na(matches)) "" else stringr::str_remove(stringr::str_to_lower(matches), "\\.$")
}

extract_email_field <- function(text, label, value = "(.+?)", stop = "(?=\\s*(?:\\||\\n|$))") {
  pattern <- stringr::regex(paste0(label, "\\s*[:|]*\\s*[:|]?\\s*", value, stop), ignore_case = TRUE)
  matches <- stringr::str_match(text, pattern)[, 2]
  if (is.na(matches)) "" else stringr::str_remove_all(stringr::str_remove_all(matches, "^[ |:-]+"), "[ |:-]+$")
}

parse_ack <- function(text) {
  nxt <- paste0(
    "(?=\\s*(?:\\||\\n|Date of Filing|Request filed with|Public Authority|Contact Details|",
    "Nodal Officer|Telephone|Email Id|Payment Details|RTI Fee|MAHA Online|Online Reference|",
    "Transaction Status|Note:|$))"
  )
  regs <- extract_registration_numbers(text)
  authority <- extract_email_field(text, "Request filed with", stop = nxt)
  if (!nzchar(authority)) authority <- extract_email_field(text, "Public Authority", stop = nxt)
  list(
    reg_no = if (length(regs) > 0) regs[1] else "",
    filer_name = extract_email_field(text, "\\bName", stop = nxt),
    filing_date = extract_email_field(text, "Date of Filing", "(\\d{1,2}/\\d{1,2}/\\d{4})", stop = ""),
    authority = authority,
    pio_email = fix_email(extract_email_field(text, "Email Id", stop = nxt))
  )
}

extract_first_registration <- function(text) {
  regs <- extract_registration_numbers(text)
  if (length(regs) > 0) regs[1] else ""
}

# Classify one message ----
## Returns kind, portal state and the fields that kind carries. Anything unrecognized is kept as
## pio_email or other rather than dropped.
classify_email <- function(rec) {
  from <- extract_email_sender(rec$from)
  subject <- clean_text(rec$subject)
  s <- stringr::str_to_lower(subject)
  text <- extract_email_body(rec)
  labels <- unlist(rec$labels)
  domain <- stringr::str_extract(from, "[^@]*$")
  state <- if (from %in% names(portal_senders)) unname(portal_senders[from]) else ""
  out <- function(kind, st = state, ...) list(kind = kind, state = st, fields = list(...))
  has_system_domain <- any(endsWith(from, system_domains)) || domain %in% system_domains
  lead <- stringr::str_sub(stringr::str_to_lower(text), 1, 400)

  if (!is.null(rec$error)) {
    return(out("collector_error"))
  }
  if ("SENT" %in% labels && !nzchar(state)) {
    return(out("sent_by_us", "", reg_nos = extract_registration_numbers(paste0(subject, "\n", text))))
  }
  if (has_system_domain) {
    return(out("system", ""))
  }
  if (stringr::str_detect(s, "\\botp\\b|one time password")) {
    return(out("otp"))
  }
  if (nzchar(state) || any(endsWith(domain, portal_domains))) {
    if (state == "TG" && stringr::str_detect(s, stringr::fixed("submission"))) {
      ## The Telangana mail prints "$regno" (a template bug): proof of filing and its time only
      name <- stringr::str_trim(stringr::str_match(text, "Dear\\s+([^,\\n]+)")[, 2])
      tg <- stringr::str_extract(text, tg_reg_re)
      return(out("filing_ack_tg", "TG",
        filer_name = dplyr::coalesce(name, ""), reg_no = dplyr::coalesce(tg, "")
      ))
    }
    filed_mail <- stringr::str_detect(s, "filed successfully|successfully (submitted|filed)")
    if (filed_mail && !stringr::str_detect(s, stringr::fixed("appeal"))) {
      return(list(kind = "filing_ack", state = state, fields = parse_ack(text)))
    }
    if (stringr::str_detect(s, stringr::fixed("appeal")) && stringr::str_detect(s, "filed|registered")) {
      return(out("appeal_ack", reg_no = extract_first_registration(text), reg_nos = extract_registration_numbers(text)))
    }
    if (stringr::str_detect(s, stringr::fixed("transferred"))) {
      regs <- extract_registration_numbers(text)
      return(out("transferred",
        reg_no = if (length(regs) > 0) regs[1] else "", new_reg_nos = regs[-1]
      ))
    }
    if (stringr::str_detect(s, stringr::fixed("returned back"))) {
      return(out("returned", reg_no = extract_first_registration(text)))
    }
    if (stringr::str_detect(s, stringr::fixed("disposed"))) {
      return(out("disposed", reg_no = extract_first_registration(text)))
    }
    if (stringr::str_detect(s, stringr::fixed("payment processing"))) {
      return(out("payment_pending"))
    }
    fee_pattern <- "additional (fee|payment)|fee.*(due|pay)|payment.*(due|request|required)"
    if (stringr::str_detect(paste(s, lead), fee_pattern)) {
      amount <- stringr::str_match(text, "Rs\\.?\\s*([\\d,]+(?:\\.\\d+)?)")[, 2]
      return(out("fee_demand",
        reg_no = extract_first_registration(text),
        amount_inr = stringr::str_remove_all(dplyr::coalesce(amount, ""), ",")
      ))
    }
    if (stringr::str_detect(s, stringr::fixed("document required"))) {
      return(out("document_required", reg_no = extract_first_registration(text)))
    }
    if (stringr::str_detect(s, stringr::fixed("reject"))) {
      return(out("rejected", reg_no = extract_first_registration(text)))
    }
    if (nzchar(state)) {
      return(out("portal_other", reg_nos = extract_registration_numbers(paste0(subject, "\n", text))))
    }
    ## A government office on a .gov.in or .nic.in address writing directly falls through
  }
  mentions_rti <- stringr::str_detect(paste(s, stringr::str_sub(stringr::str_to_lower(text), 1, 600)), rti_words_re)
  if (length(rec$attachments) > 0 || mentions_rti) {
    fee_text <- paste(subject, stringr::str_sub(text, 1, 1500))
    return(out("pio_email", "",
      reg_nos = extract_registration_numbers(paste0(subject, "\n", text)),
      mentions_fee = stringr::str_detect(fee_text, fee_re)
    ))
  }
  out("other", "")
}

# RA sheet values ----
## The sheets use 2026-09-26, 28-08-2026, 28/08/2026 and "04-09-2026 20:27:58"
parse_sheet_date <- function(x) {
  x <- stringr::str_trim(dplyr::coalesce(x, ""))
  iso <- stringr::str_match(x, "^(\\d{4})-(\\d{1,2})-(\\d{1,2})")
  dmy <- stringr::str_match(x, "^(\\d{1,2})[-/.](\\d{1,2})[-/.](\\d{4})")
  parts <- if (!is.na(iso[1, 1])) {
    as.integer(iso[1, 2:4])
  } else if (!is.na(dmy[1, 1])) {
    as.integer(dmy[1, c(4, 3, 2)])
  } else {
    return(list(day = as.Date(NA), at = as.POSIXct(NA)))
  }
  day <- as.Date(sprintf("%04d-%02d-%02d", parts[1], parts[2], parts[3]), format = "%Y-%m-%d")
  if (is.na(day)) {
    return(list(day = as.Date(NA), at = as.POSIXct(NA)))
  }
  rest <- stringr::str_sub(x, nchar(dplyr::coalesce(iso[1, 1], dmy[1, 1])) + 1)
  tm <- stringr::str_match(rest, "(\\d{1,2}):(\\d{2})(?::(\\d{2}))?")
  at <- if (is.na(tm[1, 1])) {
    as.POSIXct(NA)
  } else {
    as.POSIXct(
      sprintf(
        "%s %s:%s:%s", day, tm[1, 2], tm[1, 3], ifelse(is.na(tm[1, 4]), "00", tm[1, 4])
      ),
      tz = "Asia/Kolkata"
    )
  }
  list(day = day, at = at)
}

# Matching helpers ----
normalize_name <- function(x) {
  x <- stringr::str_replace_all(stringr::str_to_lower(dplyr::coalesce(x, "")), stringr::fixed("&amp;"), "&")
  x <- stringr::str_replace_all(x, stringr::fixed("husbadry"), "husbandry")
  stringr::str_trim(stringr::str_replace_all(stringr::str_replace_all(x, "[^a-z0-9]+", " "), "\\s+", " "))
}

compute_name_similarity <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) {
    return(0)
  }
  1 - as.numeric(utils::adist(a, b)) / max(nchar(a), nchar(b))
}

match_stop_words <- stringr::str_split(paste(
  "department office officer officers district government public information application",
  "petition online reply regarding details requested under sent dated secretariat section rti",
  "the and for sir madam tamil nadu karnataka telangana maharashtra delhi from your letter reg",
  "thiru attachment kindly download please find below with regards chennai directorate director",
  "commissioner commissionerate joint deputy assistant state head headquarters main road street",
  "nagar near copy dear this that which will have been reference cited following given directed",
  "invite attention question questions answer act 2005 annual return register"
), " ")[[1]]

extract_name_tokens <- function(x, stop_words = match_stop_words) {
  tokens <- stringr::str_split(normalize_name(x), " ")[[1]]
  unique(tokens[nchar(tokens) >= 4 & !tokens %in% stop_words & !stringr::str_detect(tokens, "^\\d+$")])
}

## Place names that tie a letter to a state. Maharashtra is left out: the applicant's own address,
## printed on every reply, is there.
state_hints <- c(
  TN = paste0(
    "tamil ?nadu|chennai|madurai|coimbatore|salem|tiruchirappalli|tirunelveli|vellore|",
    "dharmapuri"
  ),
  KA = "karnataka|bengaluru|bangalore|mysuru|mysore|belagavi|hubballi|kalaburagi",
  TG = "telangana|hyderabad|warangal|karimnagar|nizamabad",
  DL = "\\bdelhi\\b"
)

## Last-resort match for a letter quoting neither a registration number nor a known PIO address:
## the office's distinctive words plus the filing date the letter cites. Low confidence.
match_office_and_date <- function(blob, info, stop_words = match_stop_words, restrict = character()) {
  have <- extract_name_tokens(blob, stop_words)
  matches <- stringr::str_match_all(blob, date_re)[[1]]
  dates <- if (nrow(matches) > 0) {
    as.Date(
      sprintf("%s-%02d-%02d", matches[, 4], as.integer(matches[, 3]), as.integer(matches[, 2])),
      format = "%Y-%m-%d"
    )
  } else {
    as.Date(character())
  }
  hinted <- names(state_hints)[vapply(
    state_hints, function(p) stringr::str_detect(blob, stringr::regex(p, ignore_case = TRUE)), logical(1)
  )]
  scored <- list()
  for (aid in names(info)) {
    if (length(restrict) > 0 && !aid %in% restrict) next
    want <- extract_name_tokens(info[[aid]]$office, stop_words)
    if (length(want) == 0) next
    overlap <- sum(want %in% have) / length(want)
    if (overlap < 0.5) next
    date_hit <- any(as.character(dates) %in% as.character(info[[aid]]$dates))
    score <- overlap + (if (date_hit) 0.5 else 0) + (if (stringr::str_sub(aid, 1, 2) %in% hinted) 0.3 else 0)
    scored[[aid]] <- list(score = score, overlap = overlap, date_hit = date_hit, aid = aid)
  }
  if (length(scored) == 0) {
    return("")
  }
  ## Highest score first, ties broken by the larger id, as a descending tuple sort would
  ids <- names(scored)
  ord <- order(
    -vapply(scored, `[[`, 0, "score"), -vapply(scored, `[[`, 0, "overlap"),
    -vapply(scored, `[[`, 0, "date_hit"), xtfrm(ids) * -1
  )
  scored <- scored[ord]
  best <- scored[[1]]
  clear <- length(scored) == 1 || scored[[2]]$score < best$score - 0.25
  ok <- clear && ((best$date_hit && best$overlap >= 0.5) || best$overlap >= 0.99)
  if (ok) best$aid else ""
}
