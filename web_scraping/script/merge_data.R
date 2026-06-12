# ==============================================================================
# Script: merge_data.R
# Purpose: Merge all per-source SQLite databases into one master database
# Input : web_scraping/data/init_db/data_*.db
# Output: web_scraping/data/raw/master_data.db
# Requires: web_scraping/script/utils.R
# ==============================================================================

library(DBI)
library(RSQLite)
library(cli)

source("web_scraping/script/utils.R")

INPUT_DIR <- "web_scraping/data/init_db/"
OUTPUT_DB <- "web_scraping/data/master_data.db"

# SQL tạo bảng đúng theo 18-cột canonical schema (clean_rule.md)
CREATE_TABLE_SQL <- "
  CREATE TABLE IF NOT EXISTS car_listings (
    brand        TEXT,
    model        TEXT,
    trim         TEXT,
    year         INTEGER,
    body_type    TEXT,
    fuel_type    TEXT,
    transmission TEXT,
    engine_size  REAL,
    seat_count   INTEGER,
    drivetrain   TEXT,
    price        INTEGER,
    mileage      INTEGER,
    origin       TEXT,
    color        TEXT,
    city         TEXT,
    posted_date  DATE,
    source       TEXT,
    url          TEXT PRIMARY KEY
  );
"

log_message("merge_data.R", "Starting merge of individual SQLite databases into master database.")

# Đảm bảo thư mục output tồn tại
output_dir_path <- dirname(OUTPUT_DB)
if (!dir.exists(output_dir_path)) {
  dir.create(output_dir_path, recursive = TRUE)
  log_message("merge_data.R", paste("Created output directory:", output_dir_path))
}

# Tìm tất cả file DB riêng lẻ
db_files <- list.files(INPUT_DIR, pattern = "^data_.*\\.db$", full.names = TRUE)

if (length(db_files) == 0) {
  log_message("merge_data.R", "No individual SQLite databases found; creating empty master database.", "WARN")
  con_master <- dbConnect(SQLite(), dbname = OUTPUT_DB)
  dbExecute(con_master, CREATE_TABLE_SQL)
  dbDisconnect(con_master)
} else {
  pb <- cli_progress_bar(
    total  = length(db_files),
    format = "[:bar] :current/:total (:percent)"
  )

  # Tạo/mở master DB và reset bảng
  con_master <- dbConnect(SQLite(), dbname = OUTPUT_DB)
  dbExecute(con_master, "DROP TABLE IF EXISTS car_listings;")
  dbExecute(con_master, CREATE_TABLE_SQL)

  total_records <- 0L

  for (db_path in db_files) {
    website_name <- sub("^data_(.*)\\.db$", "\\1", basename(db_path))

    tryCatch({
      con_src <- dbConnect(SQLite(), dbname = db_path)
      df      <- dbReadTable(con_src, "car_listings")
      dbDisconnect(con_src)

      # Chèn vào master, bỏ qua duplicate theo PRIMARY KEY (url)
      dbWriteTable(con_master, "car_listings", df, append = TRUE, row.names = FALSE)
      total_records <- total_records + nrow(df)

      log_message("merge_data.R", sprintf(
        "Merged %d records from %s", nrow(df), db_path
      ))
    }, error = function(e) {
      log_message("merge_data.R", sprintf("Failed for %s: %s", website_name, e$message), "ERROR")
    })

    cli_progress_update(id = pb, set = which(db_files == db_path))
  }

  cli_progress_done(pb)
  dbDisconnect(con_master)

  log_message("merge_data.R", sprintf(
    "=== Merge complete. Total records in master DB: %d → %s ===",
    total_records, OUTPUT_DB
  ))
}