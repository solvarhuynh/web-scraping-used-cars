# ==============================================================================
# Script: run_pipeline.R
# Purpose: Reproducible used-car data pipeline
# Usage:
#   Rscript web_scraping/run_pipeline.R
#   RUN_SCRAPE=true Rscript web_scraping/run_pipeline.R
# ==============================================================================

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/run_pipeline.R"
RUN_SCRAPE <- tolower(Sys.getenv("RUN_SCRAPE", "false")) %in% c("true", "1", "yes", "y")

ensure_directories()
log_message(SCRIPT_NAME, sprintf("Starting full pipeline. RUN_SCRAPE=%s", RUN_SCRAPE))

cat("\n========================================\n")
cat("   STARTING USED-CAR DATA PIPELINE\n")
cat("========================================\n")

source_task <- function(path, desc) {
  if (!file.exists(path)) {
    stop(sprintf("Missing pipeline task file: %s", path))
  }
  cat(sprintf("\n---> %s\n", desc))
  source(path, local = new.env(parent = globalenv()))
}

scrape_tasks <- list(
  list(file = "web_scraping/script/scrap/scrap_chotot.R", desc = "Scrape raw data from Chợ Tốt"),
  list(file = "web_scraping/script/scrap/scrap_carpla.R", desc = "Scrape raw data from Carpla"),
  list(file = "web_scraping/script/scrap/scrap_banxehoicu.R", desc = "Scrape raw data from Bán Xe Hơi Cũ"),
  list(file = "web_scraping/script/scrap/scrap_bonbanh.R", desc = "Scrape raw data from BonBanh")
)

core_tasks <- list(
  list(file = "web_scraping/script/clean/clean_chotot.R", desc = "Clean Chợ Tốt data"),
  list(file = "web_scraping/script/clean/clean_carpla.R", desc = "Clean Carpla data"),
  list(file = "web_scraping/script/clean/clean_banxehoicu.R", desc = "Clean Bán Xe Hơi Cũ data"),
  list(file = "web_scraping/script/clean/clean_bonbanh.R", desc = "Clean BonBanh data"),
  list(file = "web_scraping/script/validate_clean_data.R", desc = "Validate clean data quality"),
  list(file = "web_scraping/script/init_database.R", desc = "Initialize per-source SQLite databases"),
  list(file = "web_scraping/script/merge_data.R", desc = "Merge master database and CSV")
)

tasks <- if (RUN_SCRAPE) c(scrape_tasks, core_tasks) else core_tasks

tryCatch({
  total_tasks <- length(tasks)
  cat("\nOverall Pipeline Progress:\n")
  pb <- txtProgressBar(min = 0, max = total_tasks, style = 3)

  for (i in seq_along(tasks)) {
    task <- tasks[[i]]
    cat(sprintf("\n\nStep [%d/%d]", i, total_tasks))
    source_task(task$file, task$desc)
    setTxtProgressBar(pb, i)
  }

  close(pb)
  log_message(SCRIPT_NAME, "Full pipeline completed successfully.")

  cat("\n\n========================================\n")
  cat(" PIPELINE COMPLETED SUCCESSFULLY\n")
  cat("========================================\n")
}, error = function(e) {
  log_message(SCRIPT_NAME, e$message, "ERROR")
  cat(sprintf("\n\n[ERROR] Pipeline failed: %s\n", e$message))
  stop(e)
})
