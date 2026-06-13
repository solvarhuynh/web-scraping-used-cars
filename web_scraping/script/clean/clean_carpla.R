# ==============================================================================
# Script: clean_carpla.R
# Purpose: Clean & standardise scraped raw data from carpla.vn
# Input : web_scraping/data/raw/data_carpla_raw.csv
# Output: web_scraping/data/clean/data_carpla_clean.csv
# Requires: web_scraping/script/utils.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/clean/clean_carpla.R"
INPUT_FILE  <- "web_scraping/data/raw/data_carpla_raw.csv"
OUTPUT_FILE <- "web_scraping/data/clean/data_carpla_clean.csv"

infer_carpla_title_fields <- function(df) {
  title <- normalize_na(df$brand)
  words <- str_split(str_squish(ifelse(is.na(title), "", title)), "\\s+")

  inferred_brand <- vapply(words, function(w) {
    if (!length(w) || w[1] == "") return(NA_character_)
    w[1]
  }, character(1))

  inferred_model <- vapply(words, function(w) {
    if (length(w) < 2) return(NA_character_)
    w[2]
  }, character(1))

  inferred_year <- str_extract(title, "(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])")

  inferred_trim <- mapply(function(w, yr) {
    if (length(w) < 3) return(NA_character_)
    tail_words <- w[3:length(w)]
    tail_text <- str_squish(paste(tail_words, collapse = " "))
    if (!is.na(yr)) tail_text <- str_squish(str_remove(tail_text, fixed(yr)))
    tail_text <- str_remove(tail_text, regex("\\b(màu|color)\\b.*$", ignore_case = TRUE))
    ifelse(tail_text == "", NA_character_, tail_text)
  }, words, inferred_year, USE.NAMES = FALSE)

  df %>%
    mutate(
      trim = coalesce(normalize_na(trim), inferred_trim),
      year = coalesce(normalize_na(year), inferred_year),
      model = coalesce(normalize_na(model), inferred_model),
      brand = inferred_brand
    )
}

clean_carpla <- function() {
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, "=== Bắt đầu cleaning dữ liệu Carpla ===")

  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, paste("Input file not found:", INPUT_FILE), "ERROR")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(
    INPUT_FILE,
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data rỗng.", "WARN")
    empty_df <- align_schema(data.frame())
    safe_write_csv(empty_df, OUTPUT_FILE)
    return(invisible(empty_df))
  }

  log_message(SCRIPT_NAME, sprintf("Đọc được %d dòng từ %s", nrow(raw), INPUT_FILE))

  df_clean <- raw %>%
    infer_carpla_title_fields() %>%
    standardize_car_data() %>%
    apply_business_rules()

  safe_write_csv(df_clean, OUTPUT_FILE)

  log_message(SCRIPT_NAME, sprintf(
    "=== Hoàn thành. %d dòng đã được lưu tại: %s ===",
    nrow(df_clean), OUTPUT_FILE
  ))

  invisible(df_clean)
}

clean_carpla()
