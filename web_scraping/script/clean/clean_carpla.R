# Clean Carpla raw data.
# Output: data/clean/data_carpla_clean.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("script/utils.R")

SCRIPT_NAME <- "clean_carpla.R"
INPUT_FILE <- "web_scraping/data/raw/data_carpla_raw.csv"
OUTPUT_FILE <- "web_scraping/data/clean/data_carpla_clean.csv"
DISPLAY_NAME <- "Carpla"

clean_carpla <- function() {
  dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, sprintf("Starting %s cleaning.", DISPLAY_NAME))

  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, "Input file not found.", "ERROR")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(INPUT_FILE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data is empty.", "WARN")
    return(invisible(NULL))
  }

  # -------------------------------------------------------------------
  # Basic sanitisation & numeric conversion according to clean_rule.md
  # -------------------------------------------------------------------
  df_cleaned <- raw %>%
    # Trim whitespace and normalise empty strings to NA
    mutate(across(everything(), ~ ifelse(is.na(.x) || str_trim(.x) == "" || str_to_lower(.x) %in% c("na", "n/a", "null", "unknown", "không rõ"), NA_character_, str_squish(.x)))) %>%
    # Parse price (VND), mileage (km), engine size (L), year, seat count, posted date
    mutate(
      # Xử lý giá tiền (Tỷ/Triệu)
      price = {
        p <- str_replace_all(price, ",", ".")
        val_ty <- as.numeric(str_extract(p, "[0-9.]+(?=\\s*t[ỷỉ])"))
        val_tr <- as.numeric(str_extract(p, "[0-9.]+(?=\\s*triệu)"))
        coalesce(val_ty, 0) * 1e9 + coalesce(val_tr, 0) * 1e6
      },
      mileage = parse_integer_value(mileage),
      engine_size = parse_engine_size(engine_size),
      year = parse_integer_value(year),
      seat_count = parse_integer_value(seat_count),
      posted_date = parse_posted_date(posted_date)
    ) %>%
    # Apply the global canonical schema helper (adds missing columns, reorders)
    align_schema()

  # Use the higher‑level standardiser to ensure brand/model uppercase and other text cleaning
  df_final <- standardize_car_data(df_cleaned)

  safe_write_csv(df_final, OUTPUT_FILE)
  log_message(SCRIPT_NAME, sprintf("Finished %s cleaning with %s rows.", DISPLAY_NAME, nrow(df_final)))
  return(df_final)
}

# Execute automatically when sourced
carpla_clean <- clean_carpla()
