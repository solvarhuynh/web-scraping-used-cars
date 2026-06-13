# ==============================================================================
# Script: run_realtime.R
# Purpose: Fetch page-1 deltas and append valid new records to master SQLite DB
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/run_realtime.R"
DB_FILE <- "web_scraping/data/master_data.db"

run_realtime <- function() {
  log_message(SCRIPT_NAME, "Starting real-time delta fetch cycle.")
  cat("\n========================================\n")
  cat("   STARTING REAL-TIME UPDATE CYCLE\n")
  cat("========================================\n")

  if (!file.exists(DB_FILE)) {
    stop("Master database not found. Run web_scraping/run_pipeline.R first.")
  }

  realtime_scripts <- list(
    list(file = "web_scraping/script/realtime/realtime_chotot.R", fn = "run_realtime_chotot"),
    list(file = "web_scraping/script/realtime/realtime_carpla.R", fn = "run_realtime_carpla"),
    list(file = "web_scraping/script/realtime/realtime_banxehoicu.R", fn = "run_realtime_banxehoicu"),
    list(file = "web_scraping/script/realtime/realtime_bonbanh.R", fn = "run_realtime_bonbanh")
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  inserted_total <- 0L

  for (task in realtime_scripts) {
    if (!file.exists(task$file)) {
      log_message(SCRIPT_NAME, sprintf("Script not found: %s", task$file), "WARN")
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
  cat("\n========================================\n")
  cat("   REAL-TIME UPDATE COMPLETED\n")
  cat("========================================\n")

  invisible(inserted_total)
}

run_realtime()
