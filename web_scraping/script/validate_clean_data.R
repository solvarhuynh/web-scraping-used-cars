# ==============================================================================
# Script: validate_clean_data.R
# Purpose: Validate cleaned CSV files before database initialization and merging
# Output: web_scraping/data/quality_report/*.csv and *.txt
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/validate_clean_data.R"
CLEAN_DIR <- "web_scraping/data/clean"
REPORT_DIR <- "web_scraping/data/quality_report"
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

validate_clean_data <- function() {
  dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
  log_message(SCRIPT_NAME, "=== Bắt đầu validation dữ liệu clean ===")

  clean_files <- list.files(CLEAN_DIR, pattern = "^data_.*_clean\\.csv$", full.names = TRUE)
  if (!length(clean_files)) {
    stop("No cleaned CSV files found in web_scraping/data/clean.")
  }

  per_file <- lapply(clean_files, function(path) {
    df <- readr::read_csv(
      path,
      col_types = cols(.default = "c"),
      locale = locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )

    missing_cols <- setdiff(CANONICAL_COLS, names(df))
    extra_cols <- setdiff(names(df), CANONICAL_COLS)
    df_aligned <- align_schema(df)

    numeric_df <- df_aligned %>%
      mutate(
        year = suppressWarnings(as.integer(year)),
        price = suppressWarnings(as.numeric(price)),
        mileage = suppressWarnings(as.numeric(mileage))
      )

    tibble(
      file = path,
      source_name = str_remove_all(basename(path), "^data_|_clean\\.csv$"),
      rows = nrow(df_aligned),
      schema_ok = length(missing_cols) == 0 && length(extra_cols) == 0 &&
        identical(names(df_aligned), CANONICAL_COLS),
      missing_cols = paste(missing_cols, collapse = ";"),
      extra_cols = paste(extra_cols, collapse = ";"),
      duplicate_url = sum(duplicated(df_aligned$url[!is.na(df_aligned$url) & df_aligned$url != ""])),
      missing_brand = sum(is.na(df_aligned$brand) | df_aligned$brand == ""),
      missing_model = sum(is.na(df_aligned$model) | df_aligned$model == ""),
      missing_url = sum(is.na(df_aligned$url) | df_aligned$url == ""),
      bad_year = sum(is.na(numeric_df$year) | numeric_df$year < 1990 | numeric_df$year > CURRENT_YEAR),
      bad_price = sum(is.na(numeric_df$price) | numeric_df$price < 5e7 | numeric_df$price > 1.5e10),
      bad_mileage = sum(!is.na(numeric_df$mileage) & (numeric_df$mileage < 0 | numeric_df$mileage > 1e6))
    )
  }) %>%
    bind_rows()

  all_data <- bind_rows(lapply(clean_files, read_clean_csv))
  cross_duplicate_urls <- all_data %>%
    filter(!is.na(url), url != "") %>%
    count(url, sort = TRUE) %>%
    filter(n > 1)

  issue_summary <- per_file %>%
    mutate(
      total_issues = (!schema_ok) + duplicate_url + missing_brand + missing_model +
        missing_url + bad_year + bad_price + bad_mileage,
      status = ifelse(total_issues == 0, "OK", "CHECK")
    )

  readr::write_csv(issue_summary, file.path(REPORT_DIR, "clean_validation_summary.csv"), na = "")
  readr::write_csv(cross_duplicate_urls, file.path(REPORT_DIR, "duplicate_urls_cross_source.csv"), na = "")

  report_path <- file.path(REPORT_DIR, "clean_validation_report.txt")
  report_lines <- capture.output({
    cat("CLEAN DATA VALIDATION REPORT\n")
    cat("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
    cat("Current year rule: <= ", CURRENT_YEAR, "\n\n", sep = "")
    print(issue_summary)
    cat("\nCross-source duplicate URLs: ", nrow(cross_duplicate_urls), "\n", sep = "")
    cat("Total clean rows: ", sum(issue_summary$rows), "\n", sep = "")
    cat("Schema OK for all files: ", all(issue_summary$schema_ok), "\n", sep = "")
    cat("Files needing check: ", paste(issue_summary$source_name[issue_summary$status == "CHECK"], collapse = ", "), "\n", sep = "")
  })
  writeLines(report_lines, report_path, useBytes = TRUE)

  log_message(SCRIPT_NAME, sprintf(
    "=== Validation hoàn thành. %d dòng clean, report tại %s ===",
    sum(issue_summary$rows), REPORT_DIR
  ))

  invisible(issue_summary)
}

validate_clean_data()
