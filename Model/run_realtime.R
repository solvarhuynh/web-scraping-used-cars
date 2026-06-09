# Orchestrator for Real-time Delta Fetching
# Execute with: Rscript run_realtime.R
# Purpose: Fetch only new records from Page 1 and append to SQLite database.

source("script/utils.R")

SCRIPT_NAME <- "run_realtime.R"

run_realtime <- function() {
  log_message(SCRIPT_NAME, "Starting real-time delta fetch cycle.")
  cat("\n========================================\n")
  cat("   STARTING REAL-TIME UPDATE CYCLE\n")
  cat("========================================\n")

  tryCatch({
    # Danh sách các script realtime cần chạy
    scripts <- c(
      "script/realtime/realtime_chotot.R",
      "script/realtime/realtime_carpla.R",
      "script/realtime/realtime_banxehoicu.R",
      "script/realtime/realtime_oto.R"
    )

    for (script in scripts) {
      if (file.exists(script)) {
        source(script)
      } else {
        cat(sprintf("\n[WARN] Script not found: %s\n", script))
        log_message(SCRIPT_NAME, sprintf("Script not found: %s", script), "WARN")
      }
    }

    log_message(SCRIPT_NAME, "Real-time update cycle completed.")
    cat("\n========================================\n")
    cat("   REAL-TIME UPDATE COMPLETED! \n")
    cat("========================================\n")
  }, error = function(e) {
    log_message(SCRIPT_NAME, e$message, "ERROR")
    cat(sprintf("\n[ERROR] Real-time cycle failed: %s\n", e$message))
  })
}

run_realtime()