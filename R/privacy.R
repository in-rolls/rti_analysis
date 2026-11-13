text_files <- "\\.(csv|R|md|in|tex|yml|yaml|json|cff)$|^(DESCRIPTION|Makefile|\\.gitignore)$"
## These hold the patterns themselves, public portal addresses and made-up examples
skip_files <- paste0(
  "^renv/|^renv\\.lock$|^R/privacy\\.R$|^R/raw\\.R$|",
  "^tests/testthat/test-privacy\\.R$|^tests/testthat/test-raw\\.R$"
)

privacy_patterns <- c(
  email = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
  phone = "(^|[^0-9A-Za-z.])([6-9][0-9]{9}|0[0-9]{10})([^0-9A-Za-z]|$)",
  registration = "TGRTI/|/R/20[0-9]{2}/|RTI/APP"
)

## A column may exist if it is empty: the drawn assignment lists carry blank tracking columns
forbidden_columns <- c(
  "reg_no", "new_reg_nos", "registration_number", "sheet_person", "filed_by", "pio_name",
  "pio_email", "pio_phone", "payment_ref", "payment_reference"
)

## Public senders of the portals' own mail, needed to recognize their templates
allowed_addresses <- c(
  "rtisupport@tn.gov.in", "rtionline-kar@nic.in", "tsrtinoreply@gmail.com",
  "noreply-rtimh@mah.gov.in", "noreply-dopt@gov.in"
)

## Which pattern labels match these lines
scan_text <- function(lines, patterns = privacy_patterns) {
  names(patterns)[vapply(patterns, function(p) any(grepl(p, lines, perl = TRUE)), logical(1))]
}

find_forbidden_columns <- function(header) {
  intersect(tolower(trimws(strsplit(header, ",", fixed = TRUE)[[1]])), forbidden_columns)
}

## Forbidden columns that actually hold values
find_filled_columns <- function(path) {
  applications <- utils::read.csv(path,
    colClasses = "character", check.names = FALSE,
    na.strings = character()
  )
  names(applications) <- tolower(trimws(names(applications)))
  Filter(function(col) any(nzchar(trimws(applications[[col]]))), intersect(names(applications), forbidden_columns))
}

find_privacy_issues <- function(root = ".") {
  old <- setwd(root)
  on.exit(setwd(old))
  files <- system2("git", c("ls-files", "--cached", "--others", "--exclude-standard"),
    stdout = TRUE
  )
  files <- files[grepl(text_files, files) & !grepl(skip_files, files) & file.exists(files)]
  terms <- if (file.exists(".private_terms")) {
    trimws(setdiff(readLines(".private_terms", warn = FALSE), ""))
  } else {
    character()
  }
  patterns <- c(privacy_patterns, setNames(sprintf("(?i)\\Q%s\\E", terms), rep(
    "private term",
    length(terms)
  )))
  findings <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (allowed in allowed_addresses) lines <- gsub(allowed, "", lines, fixed = TRUE)
    hits <- unique(scan_text(lines, patterns))
    if (grepl("\\.csv$", f) && length(lines) > 0 && length(find_forbidden_columns(lines[1])) > 0) {
      hits <- c(hits, sprintf("column %s", find_filled_columns(f)))
    }
    if (length(hits) > 0) {
      found <- paste(hits, collapse = ", ")
      findings <- c(findings, sprintf("%s: %s", f, found))
    }
  }
  findings
}
