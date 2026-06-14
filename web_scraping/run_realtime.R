# ==============================================================================
# Script: run_realtime.R
# Purpose: Fetch page-1 deltas and append valid new records to master SQLite DB
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(readr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/run_realtime.R"
DB_FILE <- "web_scraping/data/master_data.db"
OUTPUT_CSV <- "web_scraping/data/master_data.csv"

log_message(SCRIPT_NAME, "Starting real-time delta fetch cycle.")
cat("\n========================================\n")
cat("   STARTING REAL-TIME UPDATE CYCLE\n")
cat("========================================\n")

if (!file.exists(DB_FILE)) {
  stop("Master database not found. Run web_scraping/run_pipeline.R first.")
}

# Danh sách đầy đủ 3 nguồn — script tự bỏ qua nếu file chưa có
realtime_scripts <- list(
  list(file = "web_scraping/script/realtime/realtime_chotot.R",     fn = "run_realtime_chotot",     enabled = TRUE),
  list(file = "web_scraping/script/realtime/realtime_banxehoicu.R", fn = "run_realtime_banxehoicu", enabled = FALSE),
  list(file = "web_scraping/script/realtime/realtime_bonbanh.R",    fn = "run_realtime_bonbanh",    enabled = TRUE)
)

con <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

inserted_total <- 0L

for (task in realtime_scripts) {
  if (isFALSE(task$enabled)) {
    log_message(SCRIPT_NAME, sprintf("Task disabled: %s — skipping.", task$file), "INFO")
    next
  }

  if (!file.exists(task$file)) {
    log_message(SCRIPT_NAME, sprintf("Script not found: %s — skipping.", task$file), "WARN")
    next
  }

  env <- new.env(parent = globalenv())
  source(task$file, local = env)

  if (!exists(task$fn, envir = env)) {
    log_message(SCRIPT_NAME, sprintf("Function not found: %s in %s", task$fn, task$file), "ERROR")
    next
  }

  inserted <- tryCatch(
    get(task$fn, envir = env)(con),
    error = function(e) {
      log_message(SCRIPT_NAME, sprintf("%s failed: %s", task$fn, e$message), "ERROR")
      0L
    }
  )

  inserted_total <- inserted_total + as.integer(inserted)
}

log_message(SCRIPT_NAME, sprintf("Real-time update cycle completed with %d new rows.", inserted_total))

# Dong bo lai master_data.csv tu master_data.db de app Shiny doc duoc du lieu moi
if (inserted_total > 0) {
  master_df <- DBI::dbReadTable(con, "car_listings") %>%
    align_schema() %>%
    dplyr::arrange(source, brand, model, year)

  readr::write_csv(master_df, OUTPUT_CSV, na = "")
  log_message(SCRIPT_NAME, sprintf("Da cap nhat %s (%d dong).", OUTPUT_CSV, nrow(master_df)))
} else {
  log_message(SCRIPT_NAME, sprintf("Khong co dong moi, giu nguyen %s.", OUTPUT_CSV))
}

cat("\n========================================\n")
cat("   REAL-TIME UPDATE COMPLETED\n")
cat("========================================\n")