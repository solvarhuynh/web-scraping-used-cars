# Main batch orchestrator for the used-car data pipeline.
# Execute with: Rscript run_pipeline.R

source("script/utils.R")

SCRIPT_NAME <- "run_pipeline.R"

run_pipeline <- function() {
  ensure_directories()
  log_message(SCRIPT_NAME, "Starting full batch pipeline.")
  cat("\n========================================\n")
  cat("   STARTING USED-CAR DATA PIPELINE\n")
  cat("========================================\n")

  tryCatch({
    # Define all pipeline tasks
    tasks <- list(
      list(file = "script/scrap_chotot.R", desc = "Scraping raw data from Chotot"),
      list(file = "script/scrap_carpla.R", desc = "Scraping raw data from Carpla"),
      list(file = "script/scrap_banxehoicu.R", desc = "Scraping raw data from Banxehoicu"),
      list(file = "script/scrap_oto.R", desc = "Scraping raw data from Oto.com.vn"),
      list(file = "script/clean_chotot.R", desc = "Cleaning Chotot data"),
      list(file = "script/clean_carpla.R", desc = "Cleaning Carpla data"),
      list(file = "script/clean_banxehoicu.R", desc = "Cleaning Banxehoicu data"),
      list(file = "script/clean_oto.R", desc = "Cleaning Oto.com.vn data"),
      list(file = "script/merge_data.R", desc = "Merging all cleaned data"),
      list(file = "script/init_database.R", desc = "Initializing SQLite database")
    )

    total_tasks <- length(tasks)
    
    # Initialize overall progress bar
    cat("\nOverall Pipeline Progress:\n")
    pb <- txtProgressBar(min = 0, max = total_tasks, style = 3)

    for (i in seq_along(tasks)) {
      task <- tasks[[i]]
      cat(sprintf("\n\n---> Step [%d/%d]: %s...\n", i, total_tasks, task$desc))
      source(task$file)
      setTxtProgressBar(pb, i)
    }
    close(pb)

    log_message(SCRIPT_NAME, "Full batch pipeline completed successfully.")
    cat("\n\n========================================\n")
    cat(" PIPELINE COMPLETED SUCCESSFULLY! \n")
    cat("========================================\n")
  }, error = function(e) {
    log_message(SCRIPT_NAME, e$message, "ERROR")
    cat(sprintf("\n\n[ERROR] Pipeline failed: %s\n", e$message))
    stop(e)
  })
}

run_pipeline()
