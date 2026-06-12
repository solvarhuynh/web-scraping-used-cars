setwd("d:/R program/project")
source("d:/R program/project/web_scraping/script/utils.R")

SCRIPT_NAME <- "run_pipeline.R"

ensure_directories()
log_message(SCRIPT_NAME, "Starting full batch pipeline.")
cat("\n========================================\n")
cat("   STARTING USED-CAR DATA PIPELINE\n")
cat("========================================\n")

tryCatch({
  # Define all pipeline tasks - Bạn hãy kiểm tra lại các đường dẫn này có đúng folder web_scraping không nhé
  tasks <- list(
    list(file = "d:/R program/project/web_scraping/script/scrap/scrap_chotot.R", desc = "Scraping raw data from Chotot"),
    list(file = "d:/R program/project/web_scraping/script/scrap/scrap_carpla.R", desc = "Scraping raw data from Carpla"),
    list(file = "d:/R program/project/web_scraping/script/scrap/scrap_banxehoicu.R", desc = "Scraping raw data from Banxehoicu")
  )

  missing_files <- vapply(tasks, function(task) !file.exists(task$file), logical(1))
  if (any(missing_files)) {
    stop(sprintf(
      "Missing pipeline task file(s): %s",
      paste(vapply(tasks[missing_files], `[[`, character(1), "file"), collapse = ", ")
    ))
  }

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
