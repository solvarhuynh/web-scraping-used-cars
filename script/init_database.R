library(DBI)
library(RSQLite)
library(tidyverse)
library(cli)

source("script/utils.R")

log_file <- "log.txt"
clean_dir <- "data/clean/"
output_dir <- "data/init_db/"

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] [init_database.R] - INFO: ", msg, "\n"), file = log_file, append = TRUE)
}

log_msg("Starting per-source SQLite database initialization.")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  log_msg(paste("Created output directory", output_dir))
}

# Find all cleaned CSV files
clean_files <- list.files(clean_dir, pattern = "^data_.*_clean\\.csv$", full.names = TRUE)

if (length(clean_files) == 0) {
  log_msg("No cleaned CSV files found in data/clean/; nothing to process.")
} else {
  pb <- cli_progress_bar(total = length(clean_files), format = "[:bar] :current/:total (:percent) :msg")
  for (csv_path in clean_files) {
    website_name <- sub("^data_(.*)_clean\\.csv$", "\\1", basename(csv_path))
    db_path <- file.path(output_dir, paste0("data_", website_name, ".db"))

    # Read CSV
    data_df <- read_csv(csv_path, show_col_types = FALSE)

    # Connect and write to SQLite
    con <- dbConnect(SQLite(), dbname = db_path)
    dbWriteTable(con, "car_listings", data_df, overwrite = TRUE, row.names = FALSE)
    # Ensure url column is primary key unique
    dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_url ON car_listings(url);")
    dbDisconnect(con)

    log_msg(paste0("Imported ", nrow(data_df), " records from ", csv_path, " into ", db_path))
    cli_progress_update(id = pb, set = 1, message = website_name)
  }
  cli_progress_done(pb)
}
