# Shared utilities for the used-car data pipeline.
# This file is sourced by scraping, cleaning, merging, database, and realtime scripts.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

# Canonical 18-column schema required by rule/scrap_rule.md.
CAR_SCHEMA <- c(
  "brand", "model", "trim", "year", "body_type", "fuel_type", "transmission",
  "engine_size", "seat_count", "drivetrain", "price", "mileage", "origin",
  "color", "city", "posted_date", "source", "url"
)

# Resolve the project root from scripts sourced inside script/ or from the root runner.
project_root <- function() {
  normalizePath(file.path(dirname(sys.frame(1)$ofile %||% getwd()), ".."), mustWork = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

root_path <- function(...) {
  # getwd() is expected to be the project root when run_pipeline.R is used.
  file.path(getwd(), ...)
}

log_message <- function(script_name, message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] - %s: %s", timestamp, script_name, level, message)
  cat(line, "\n", file = root_path("log.txt"), append = TRUE)
}

ensure_directories <- function() {
  dir.create(root_path("script"), showWarnings = FALSE, recursive = TRUE)
  dir.create(root_path("data"), showWarnings = FALSE, recursive = TRUE)
  # Create subdirectories for raw and clean data as per process_rule.md
  dir.create(root_path("data", "raw"), showWarnings = FALSE, recursive = TRUE)
  dir.create(root_path("data", "clean"), showWarnings = FALSE, recursive = TRUE)
}

empty_car_data <- function(n = 0) {
  tibble(
    brand = rep(NA_character_, n),
    model = rep(NA_character_, n),
    trim = rep(NA_character_, n),
    year = rep(NA_integer_, n),
    body_type = rep(NA_character_, n),
    fuel_type = rep(NA_character_, n),
    transmission = rep(NA_character_, n),
    engine_size = rep(NA_real_, n),
    seat_count = rep(NA_integer_, n),
    drivetrain = rep(NA_character_, n),
    price = rep(NA_real_, n),
    mileage = rep(NA_integer_, n),
    origin = rep(NA_character_, n),
    color = rep(NA_character_, n),
    city = rep(NA_character_, n),
    posted_date = rep(as.Date(NA), n),
    source = rep(NA_character_, n),
    url = rep(NA_character_, n)
  )
}

align_schema <- function(df) {
  missing_cols <- setdiff(CAR_SCHEMA, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }

  df %>%
    select(all_of(CAR_SCHEMA))
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- str_squish(x)
  x[x == "" | str_to_lower(x) %in% c("na", "n/a", "null", "unknown", "không rõ")] <- NA_character_
  x
}

parse_price_vnd <- function(x) {
  # Guard against NA/empty input
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- clean_text(x)
  if (all(is.na(x))) return(NA_real_)
  x <- str_to_lower(x)
  multiplier <- case_when(
    str_detect(x, "tỷ|ty") ~ 1000000000,
    str_detect(x, "triệu|trieu") ~ 1000000,
    TRUE ~ 1
  )

  number <- str_extract(x, "[0-9]+([,.][0-9]+)?")
  number <- str_replace(number, ",", ".")
  value <- suppressWarnings(as.numeric(number))

  plain_digits <- str_remove_all(x, "[^0-9]")
  result <- ifelse(multiplier == 1 & !is.na(plain_digits) & plain_digits != "",
                   suppressWarnings(as.numeric(plain_digits)),
                   value * multiplier)
  as.numeric(round(result))
}

parse_integer_value <- function(x) {
  x <- clean_text(x)
  digits <- str_remove_all(x, "[^0-9]")
  suppressWarnings(as.integer(ifelse(digits == "", NA, digits)))
}

parse_engine_size <- function(x) {
  x <- str_replace(clean_text(x), ",", ".")
  suppressWarnings(as.numeric(str_extract(x, "[0-9]+(\\.[0-9]+)?")))
}

parse_posted_date <- function(x) {
  # Safety wrapper – any parsing failure returns NA instead of throwing.
  tryCatch({
    x <- clean_text(x)
    # Handle Vietnamese relative time strings like "4 giờ trước" or "11 phút trước"
    rel_match <- stringr::str_match(x, "(?i)(\\d+)\\s*(giờ|phút|phut|giay|giây)\\s*trước")
    if (!is.na(rel_match[1,1])) {
      value <- as.numeric(rel_match[1,2])
      unit <- tolower(rel_match[1,3])
      now <- Sys.time()
      dt <- switch(unit,
                   "giờ" = now - lubridate::hours(value),
                   "giây" = now - lubridate::seconds(value),
                   "giay" = now - lubridate::seconds(value),
                   "phút" = now - lubridate::minutes(value),
                   "phut" = now - lubridate::minutes(value),
                   now)
      return(as.Date(dt))
    }
    # Fallback to absolute date parsing
    parsed <- suppressWarnings(parse_date_time(x, orders = c("dmy", "ymd", "mdy", "dmy HMS", "ymd HMS")))
    as.Date(parsed)
  }, error = function(e) {
    # Log the warning for debugging (optional)
    log_message("utils.R", sprintf("parse_posted_date failed for input '%s': %s", x, e$message), "WARN")
    NA
  })
}

standardize_transmission <- function(x) {
  x <- clean_text(x)
  case_when(
    str_detect(str_to_lower(x), "tự động|tu dong|automatic|at") ~ "Tự động",
    str_detect(str_to_lower(x), "số sàn|so san|manual|mt") ~ "Số sàn",
    str_detect(str_to_lower(x), "cvt") ~ "CVT",
    TRUE ~ x
  )
}

standardize_fuel_type <- function(x) {
  x <- clean_text(x)
  case_when(
    str_detect(str_to_lower(x), "xăng|xang|petrol|gasoline") ~ "Xăng",
    str_detect(str_to_lower(x), "dầu|dau|diesel") ~ "Dầu",
    str_detect(str_to_lower(x), "hybrid") ~ "Hybrid",
    str_detect(str_to_lower(x), "điện|dien|electric|ev") ~ "Điện",
    TRUE ~ x
  )
}

standardize_car_data <- function(df) {
  df <- align_schema(df)

  df %>%
    mutate(across(where(is.character), clean_text)) %>%
    mutate(
      brand = str_to_upper(brand),
      model = str_to_upper(model)
    ) %>%
    mutate(
      # Split combined brand/model if model missing
      brand = ifelse(is.na(model) | model == "", str_extract(brand, "^[^\\s]+"), brand),
      model = ifelse(is.na(model) | model == "", str_trim(str_remove(str_to_upper(brand), "^[^\\s]+")), model),
      price = parse_price_vnd(price),
      mileage = parse_integer_value(mileage),
      engine_size = parse_engine_size(engine_size),
      year = parse_integer_value(year),
      seat_count = parse_integer_value(seat_count),
      posted_date = parse_posted_date(posted_date),
      fuel_type = standardize_fuel_type(fuel_type),
      transmission = standardize_transmission(transmission),
      body_type = clean_text(body_type),
      drivetrain = clean_text(drivetrain)
    ) %>%
    align_schema()
}

safe_write_csv <- function(df, path) {
  readr::write_excel_csv(align_schema(df), path, na = "")
}
