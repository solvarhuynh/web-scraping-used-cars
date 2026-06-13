# ==============================================================================
# Script: merge_data.R
# Purpose: Merge all per-source SQLite databases into master DB and CSV
# Input : web_scraping/data/init_db/data_*.db
# Output: web_scraping/data/master_data.db, web_scraping/data/master_data.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(readr)
  library(dplyr)
  library(cli)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/merge_data.R"
INPUT_DIR <- "web_scraping/data/init_db"
OUTPUT_DB <- "web_scraping/data/master_data.db"
OUTPUT_CSV <- "web_scraping/data/master_data.csv"
TABLE_NAME <- "car_listings"

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
    price        REAL,
    mileage      INTEGER,
    origin       TEXT,
    color        TEXT,
    city         TEXT,
    posted_date  DATE,
    source       TEXT,
    url          TEXT PRIMARY KEY
  );
"

coerce_master_types <- function(df) {
  align_schema(df) %>%
    mutate(
      year = suppressWarnings(as.integer(year)),
      engine_size = suppressWarnings(as.numeric(engine_size)),
      seat_count = suppressWarnings(as.integer(seat_count)),
      price = suppressWarnings(as.numeric(price)),
      mileage = suppressWarnings(as.integer(mileage)),
      posted_date = as.character(posted_date)
    )
}

merge_data <- function() {
  dir.create(dirname(OUTPUT_DB), recursive = TRUE, showWarnings = FALSE)
  log_message(SCRIPT_NAME, "Starting merge of individual SQLite databases into master outputs.")

  db_files <- list.files(INPUT_DIR, pattern = "^data_.*\\.db$", full.names = TRUE)

  con_master <- dbConnect(SQLite(), dbname = OUTPUT_DB)
  on.exit(dbDisconnect(con_master), add = TRUE)

  dbExecute(con_master, "DROP TABLE IF EXISTS car_listings;")
  dbExecute(con_master, CREATE_TABLE_SQL)

  if (!length(db_files)) {
    log_message(SCRIPT_NAME, "No individual SQLite databases found; creating empty master outputs.", "WARN")
  } else {
    pb <- cli_progress_bar(total = length(db_files), format = "[:bar] :current/:total (:percent)")

    for (i in seq_along(db_files)) {
      db_path <- db_files[[i]]
      source_name <- sub("^data_(.*)\\.db$", "\\1", basename(db_path))

      tryCatch({
        con_src <- dbConnect(SQLite(), dbname = db_path)
        df <- dbReadTable(con_src, TABLE_NAME)
        dbDisconnect(con_src)

        df <- coerce_master_types(df)

        tmp_table <- paste0("tmp_", source_name)
        dbWriteTable(con_master, tmp_table, df, overwrite = TRUE, row.names = FALSE)

        cols <- paste(CANONICAL_COLS, collapse = ", ")
        insert_sql <- sprintf(
          "INSERT OR IGNORE INTO %s (%s) SELECT %s FROM %s;",
          TABLE_NAME, cols, cols, tmp_table
        )
        inserted <- dbExecute(con_master, insert_sql)
        dbExecute(con_master, sprintf("DROP TABLE IF EXISTS %s;", tmp_table))

        log_message(SCRIPT_NAME, sprintf(
          "Merged %d/%d records from %s", inserted, nrow(df), db_path
        ))
      }, error = function(e) {
        log_message(SCRIPT_NAME, sprintf("Failed for %s: %s", source_name, e$message), "ERROR")
      })

      cli_progress_update(id = pb, set = i)
    }

    cli_progress_done(pb)
  }

  master_df <- dbReadTable(con_master, TABLE_NAME) %>%
    align_schema() %>%
    arrange(source, brand, model, year)

  readr::write_csv(master_df, OUTPUT_CSV, na = "")

  log_message(SCRIPT_NAME, sprintf(
    "=== Merge complete. %d unique records -> %s and %s ===",
    nrow(master_df), OUTPUT_DB, OUTPUT_CSV
  ))

  invisible(master_df)
}

merge_data()
