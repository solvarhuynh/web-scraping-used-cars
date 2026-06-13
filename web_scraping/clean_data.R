# ==============================================================================
# Script: clean_data.R
# Purpose: Convenience wrapper to clean all raw source files
# ==============================================================================

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/clean_data.R"
ensure_directories()
log_message(SCRIPT_NAME, "Starting all clean scripts.")

clean_scripts <- c(
  "web_scraping/script/clean/clean_chotot.R",
  "web_scraping/script/clean/clean_carpla.R",
  "web_scraping/script/clean/clean_banxehoicu.R",
  "web_scraping/script/clean/clean_bonbanh.R"
)

for (script in clean_scripts) {
  if (!file.exists(script)) stop("Missing clean script: ", script)
  source(script, local = new.env(parent = globalenv()))
}

log_message(SCRIPT_NAME, "All clean scripts completed.")
