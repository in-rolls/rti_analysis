source("scripts/00_config.R")


raw_root <- path.expand(Sys.getenv("RTI_RAW"))
if (!nzchar(raw_root) || !dir.exists(raw_root)) stop("Set RTI_RAW to the shared Drive rti/ folder.")

events <- readr::read_csv(here::here("data", "derived", "events.csv"),
  col_types = readr::cols(date = "D", .default = "c"), show_col_types = FALSE, na = ""
) |>
  dplyr::filter(!is.na(app_id), nzchar(app_id))
register <- readr::read_rds(here::here("data", "private", "ra_register.rds"))
manifest <- readr::read_csv(here::here("data", "raw_manifest.csv"), show_col_types = FALSE)
assignments <- read_assignments()

response_kinds <- c("pio_email", "disposed", "returned", "rejected")

first_date <- function(kinds) {
  picked <- events |> dplyr::filter(kind %in% kinds)
  if (nrow(picked) == 0) {
    return(dplyr::tibble(app_id = character(), value = as.Date(character())))
  }
  picked |>
    dplyr::group_by(app_id) |>
    dplyr::summarise(value = min(date), .groups = "drop")
}
last_date <- function(kinds) {
  picked <- events |> dplyr::filter(kind %in% kinds)
  if (nrow(picked) == 0) {
    return(dplyr::tibble(app_id = character(), value = as.Date(character())))
  }
  picked |>
    dplyr::group_by(app_id) |>
    dplyr::summarise(value = max(date), .groups = "drop")
}
add_date <- function(applications, ev, name) {
  applications |>
    dplyr::left_join(ev, by = "app_id") |>
    dplyr::rename(!!name := value)
}

## The first acknowledgment, and which message gave the first response
ack <- events |>
  dplyr::filter(kind %in% c("filing_ack", "filing_ack_tg")) |>
  dplyr::group_by(app_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::transmute(app_id, ack_date = date, ack_method = method)

response_events <- events |> dplyr::filter(kind %in% response_kinds)
first_any <- response_events |>
  dplyr::arrange(app_id, date) |>
  dplyr::group_by(app_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::transmute(app_id,
    first_email_response = date, first_response_kind = kind,
    first_response_method = method, first_response_confidence = confidence
  )
first_strict <- response_events |>
  dplyr::filter(confidence != "low") |>
  dplyr::group_by(app_id) |>
  dplyr::summarise(first_email_response_strict = min(date), .groups = "drop")

fee_amount <- events |>
  dplyr::filter(kind == "fee_demand") |>
  dplyr::group_by(app_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::transmute(app_id, fee_amount_inr)

# Documents in the archive ----
loose_id <- "\\b(TN|KA|TG|DL|MH)\\b[^0-9]{0,24}?(\\d{3,4})\\b"
id_from_name <- function(path) {
  model <- stringr::str_match(stringr::str_replace_all(stringr::str_to_upper(basename(path)), "_", "-"), loose_id)
  dplyr::if_else(is.na(model[, 1]), NA_character_, sprintf("%s-%03d", model[, 2], as.integer(model[, 3])))
}
count_docs <- function(folder, name) {
  find_pinned_originals(raw_root, folder, manifest) |>
    dplyr::mutate(app_id = id_from_name(path)) |>
    dplyr::filter(!is.na(app_id)) |>
    dplyr::count(app_id, name = name)
}

# One row per application ----
status <- assignments |>
  dplyr::transmute(app_id, state) |>
  dplyr::left_join(
    register |>
      dplyr::transmute(app_id, group,
        reg_cell = dplyr::case_when(real_reg ~ "number", has_reg ~ "text", TRUE ~ "empty"),
        sheet_filing_date, sheet_response_date
      ),
    by = "app_id"
  ) |>
  dplyr::left_join(ack, by = "app_id") |>
  add_date(first_date("transferred"), "first_transfer_on") |>
  add_date(last_date("transferred"), "transferred_on") |>
  add_date(first_date("fee_demand"), "first_fee_on") |>
  add_date(last_date("fee_demand"), "fee_demanded_on") |>
  add_date(first_date("disposed"), "disposed_on") |>
  add_date(first_date("returned"), "returned_on") |>
  add_date(first_date("rejected"), "rejected_on") |>
  add_date(first_date("pio_email"), "email_response_on") |>
  add_date(first_date("appeal_ack"), "appeal_filed_on") |>
  dplyr::left_join(first_any, by = "app_id") |>
  dplyr::left_join(first_strict, by = "app_id") |>
  dplyr::left_join(fee_amount, by = "app_id") |>
  dplyr::left_join(count_docs("rti responses", "n_reply_pdfs"), by = "app_id") |>
  dplyr::left_join(count_docs("Filed_RTI", "n_receipts"), by = "app_id") |>
  dplyr::mutate(
    ## Filed means a real registration number on the sheet or a portal acknowledgment.
    ## A note typed into the number cell does not count.
    filed = as.integer(dplyr::coalesce(reg_cell == "number", FALSE) | !is.na(ack_date)),
    filing_date = dplyr::if_else(filed == 1L, dplyr::coalesce(ack_date, sheet_filing_date), as.Date(NA)),
    filing_date_source = dplyr::case_when(
      filed == 0L ~ "",
      !is.na(ack_date) ~ "portal acknowledgment",
      !is.na(sheet_filing_date) ~ "RA sheet",
      TRUE ~ "missing"
    ),
    first_response = pmin(first_email_response, sheet_response_date, na.rm = TRUE),
    first_response_strict = pmin(first_email_response_strict, sheet_response_date, na.rm = TRUE),
    n_reply_pdfs = dplyr::coalesce(n_reply_pdfs, 0L),
    n_receipts = dplyr::coalesce(n_receipts, 0L)
  ) |>
  dplyr::arrange(app_id)

stopifnot(nrow(status) == nrow(assignments), !anyDuplicated(status$app_id))
readr::write_csv(status, here::here("data", "derived", "status.csv"), na = "")

# Frame sizes ----
read_frame <- function(name) {
  readr::read_csv(find_upstream_file(name), col_types = readr::cols(.default = "c"), show_col_types = FALSE)
}
## Gaurav's frame spans six states; only its Tamil Nadu rows are used
tn_frame <- read_frame("tn_frame") |> dplyr::filter(state == "Tamil Nadu")
tn_frame_tiers <- dplyr::count(tn_frame, tier = level_label, name = "frame_n")
dm <- read_frame("dm_frame")
state_col <- dplyr::intersect(c("State", "state"), names(dm))[1]
frame_sizes <- dplyr::tibble(
  state = state_order,
  frame_n = c(
    sum(tn_frame_tiers$frame_n),
    nrow(read_frame("tg_frame")),
    nrow(read_frame("ka_frame")),
    sum(dm[[state_col]] %in% c("Delhi", "DL")),
    sum(dm[[state_col]] %in% c("Maharashtra", "MH"))
  )
)
readr::write_csv(tn_frame_tiers, here::here("data", "derived", "tn_frame_tiers.csv"))
readr::write_csv(frame_sizes, here::here("data", "derived", "frame_sizes.csv"))

print(dplyr::count(status, state, filed))
print(dplyr::count(dplyr::filter(status, filed == 1L), filing_date_source))
print(dplyr::count(status, first_response_confidence))
