library(DBI)
library(RSQLite)
library(tidyverse)
library(cli)

source("script/utils.R")

log_file <- "log.txt"
input_dir <- "data/init_db/"
output_db <- "data/master_data.db"

log_msg <- function(msg) {
  cat(paste0("[", Sys.time(), "] [merge_data.R] - INFO: ", msg, "\n"), file = log_file, append = TRUE)
}

log_msg("Starting merge of individual SQLite databases into master database.")

# Ensure output directory exists
output_dir <- dirname(output_db)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  log_msg(paste("Created output directory", output_dir))
}

# Find all individual DB files
db_files <- list.files(input_dir, pattern = "^data_.*\\.db$", full.names = TRUE)

if (length(db_files) == 0) {
  log_msg("No individual SQLite databases found; creating empty master database.")
  # Create empty master DB with proper schema
  con_master <- dbConnect(SQLite(), dbname = output_db)
  dbExecute(con_master, "CREATE TABLE IF NOT EXISTS car_listings ("\
                "url TEXT PRIMARY KEY, "\
                "brand TEXT, model TEXT, year INTEGER, price REAL, mileage REAL, "\
                "location TEXT, color TEXT, fuel TEXT, transmission TEXT, "\
                "engine TEXT, doors INTEGER, seats INTEGER, "\
                "description TEXT, image_url TEXT, source TEXT, scraped_at TEXT);")
  dbDisconnect(con_master)
} else {
  pb <- cli_progress_bar(total = length(db_files), format = "[:bar] :current/:total (:percent) :msg")
  # Create master DB and ensure schema
  con_master <- dbConnect(SQLite(), dbname = output_db)
  dbExecute(con_master, "DROP TABLE IF EXISTS car_listings;")
  dbExecute(con_master, "CREATE TABLE car_listings ("\
                "url TEXT PRIMARY KEY, "\
                "brand TEXT, model TEXT, year INTEGER, price REAL, mileage REAL, "\
                "location TEXT, color TEXT, fuel TEXT, transmission TEXT, "\
                "engine TEXT, doors INTEGER, seats INTEGER, "\
                "description TEXT, image_url TEXT, source TEXT, scraped_at TEXT);")

  total_records <- 0
  for (db_path in db_files) {
    website_name <- sub("^data_(.*)\\.db$", "\\1", basename(db_path))
    con_ind <- dbConnect(SQLite(), dbname = db_path)
    df <- dbReadTable(con_ind, "car_listings")
    dbDisconnect(con_ind)
    # Insert into master DB, ignoring duplicates via primary key constraint
    dbWriteTable(con_master, "car_listings", df, append = TRUE, row.names = FALSE)
    total_records <- total_records + nrow(df)
    log_msg(paste0("Merged ", nrow(df), " records from ", db_path))
    cli_progress_update(id = pb, set = 1, message = website_name)
  }
  cli_progress_done(pb)
  dbDisconnect(con_master)
  log_msg(paste0("Merging completed. Total records in master DB: ", total_records))
}
