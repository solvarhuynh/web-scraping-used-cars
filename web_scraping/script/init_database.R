# ==============================================================================
# Script: init_database.R
# Purpose: Import each cleaned CSV into its own SQLite database (per-source)
# Input : web_scraping/data/clean/data_*_clean.csv
# Output: web_scraping/data/init_db/data_*.db
# Requires: web_scraping/script/utils.R
# ==============================================================================

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)
library(cli)

source("web_scraping/script/utils.R")

CLEAN_DIR  <- "web_scraping/data/clean/"
OUTPUT_DIR <- "web_scraping/data/init_db/"

log_message("init_database.R", "Starting per-source SQLite database initialization.")

# Đảm bảo thư mục output tồn tại
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  log_message("init_database.R", paste("Created output directory:", OUTPUT_DIR))
}

# Tìm tất cả file cleaned CSV
clean_files <- list.files(CLEAN_DIR, pattern = "^data_.*_clean\\.csv$", full.names = TRUE)

if (length(clean_files) == 0) {
  log_message("init_database.R", "No cleaned CSV files found in data/clean/; nothing to process.", "WARN")
} else {
  pb <- cli_progress_bar(
    total  = length(clean_files),
    format = "[:bar] :current/:total (:percent)"
  )

  for (csv_path in clean_files) {
    website_name <- sub("^data_(.*)_clean\\.csv$", "\\1", basename(csv_path))
    db_path      <- file.path(OUTPUT_DIR, paste0("data_", website_name, ".db"))

    tryCatch({
      # Đọc CSV bằng dạng chuỗi để tránh lỗi đoán kiểu, đặc biệt với file rỗng
      data_df <- read_clean_csv(csv_path) %>%
        mutate(
          year = as.integer(year),
          engine_size = as.numeric(engine_size),
          seat_count = as.integer(seat_count),
          price = as.numeric(price), # Dùng numeric vì giá VND có thể vượt giới hạn 32-bit integer của R
          mileage = as.integer(mileage),
          # Chuyển đổi posted_date về định dạng YYYY-MM-DD chuẩn của SQLite DATE
          posted_date = as.character(as.Date(posted_date, tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%d/%m/%Y")))
        )

      # Xoá file db cũ nếu có để tránh lỗi "file is not a database" do file bị hỏng
      if (file.exists(db_path)) file.remove(db_path)

      # Định nghĩa chính xác kiểu dữ liệu cho SQLite theo clean_rule.md
      schema_types <- c(
        brand        = "TEXT",
        model        = "TEXT",
        trim         = "TEXT",
        year         = "INTEGER",
        body_type    = "TEXT",
        fuel_type    = "TEXT",
        transmission = "TEXT",
        engine_size  = "REAL",
        seat_count   = "INTEGER",
        drivetrain   = "TEXT",
        price        = "REAL",
        mileage      = "INTEGER",
        origin       = "TEXT",
        color        = "TEXT",
        city         = "TEXT",
        posted_date  = "DATE",
        source       = "TEXT",
        url          = "TEXT"
      )
      
      data_df <- data_df %>% align_schema() %>% select(all_of(names(schema_types)))

      # Ghi vào SQLite và ép kiểu (field.types)
      con <- dbConnect(SQLite(), dbname = db_path)
      dbWriteTable(con, "car_listings", data_df, overwrite = TRUE, row.names = FALSE, field.types = schema_types)
      # Tạo unique index trên cột url
      dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_url ON car_listings(url);")
      dbDisconnect(con)

      log_message("init_database.R", sprintf(
        "Imported %d records from %s into %s", nrow(data_df), csv_path, db_path
      ))
    }, error = function(e) {
      log_message("init_database.R", sprintf("Failed for %s: %s", website_name, e$message), "ERROR")
    })

    cli_progress_update(id = pb, set = which(clean_files == csv_path))
  }

  cli_progress_done(pb)
  log_message("init_database.R", "=== Per-source database initialization complete. ===")
}
