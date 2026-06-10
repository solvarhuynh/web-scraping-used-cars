# Clean Bán Xe Hơi Cũ raw data according to rule/clean_rule.md.
# Output: data/clean/data_banxehoicu_clean.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("script/utils.R")

SCRIPT_NAME <- "clean_banxehoicu.R"
INPUT_FILE <- "data/raw/data_banxehoicu_raw.csv"
OUTPUT_FILE <- "data/clean/data_banxehoicu_clean.csv"
DISPLAY_NAME <- "Bán Xe Hơi Cũ"

clean_banxehoicu <- function() {
  cat(sprintf("
Starting %s data cleaning...
", DISPLAY_NAME))
  log_message(SCRIPT_NAME, sprintf("Starting %s cleaning.", DISPLAY_NAME))

  tryCatch({
    cat(sprintf("Reading raw file: %s
", INPUT_FILE))
    raw <- if (file.exists(INPUT_FILE)) {
      readr::read_csv(INPUT_FILE, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
    } else {
      cat(sprintf("Raw file not found: %s. Creating empty cleaned dataset.
", INPUT_FILE))
      empty_car_data()
    }

    row_count <- nrow(raw)
    cat(sprintf("Loaded %s raw rows from %s.
", row_count, DISPLAY_NAME))

    if (row_count > 0) {
      cat("Applying standard cleaning transformations...
")
      pb <- txtProgressBar(min = 0, max = 4, style = 3)
      setTxtProgressBar(pb, 1)
      raw <- align_schema(raw)
      setTxtProgressBar(pb, 2)
      clean <- standardize_car_data(raw)
      setTxtProgressBar(pb, 3)
      safe_write_csv(clean, OUTPUT_FILE)
      setTxtProgressBar(pb, 4)
      close(pb)
      cat("
")
    } else {
      cat("No rows to clean. Writing an empty cleaned CSV with the required schema.
")
      clean <- empty_car_data()
      safe_write_csv(clean, OUTPUT_FILE)
    }

    log_message(SCRIPT_NAME, sprintf("Finished %s cleaning with %s rows.", DISPLAY_NAME, nrow(clean)))
    cat(sprintf("Successfully cleaned %s rows for %s. Output saved to %s.
", nrow(clean), DISPLAY_NAME, OUTPUT_FILE))
    clean
  }, error = function(e) {
    cat(sprintf("Cleaning failed for %s: %s
", DISPLAY_NAME, e$message))
    log_message(SCRIPT_NAME, e$message, "ERROR")
    clean <- empty_car_data()
    safe_write_csv(clean, OUTPUT_FILE)
    cat(sprintf("Wrote empty fallback cleaned file to %s.
", OUTPUT_FILE))
    clean
  })
}

banxehoicu_clean <- clean_banxehoicu()
