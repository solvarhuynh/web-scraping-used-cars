# ==============================================================================
# Script: clean_banxehoicu.R
# Purpose: Clean & standardise scraped raw data from banxehoicu.vn
# Input : web_scraping/data/raw/data_banxehoicu_raw.csv
# Output: web_scraping/data/clean/data_banxehoicu_clean.csv
# Requires: script/utils.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/clean/clean_banxehoicu.R"
INPUT_FILE  <- "web_scraping/data/raw/data_banxehoicu_raw.csv"
OUTPUT_FILE <- "web_scraping/data/clean/data_banxehoicu_clean.csv"

# ==============================================================================
clean_banxehoicu <- function() {
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, "=== Bắt đầu cleaning dữ liệu Bán Xe Hơi Cũ ===")

  # ── Đọc file raw ─────────────────────────────────────────────────────────────
  # File banxehoicu_raw.csv không có dòng header — dùng CANONICAL_COLS làm tên cột
  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, paste("Input file not found:", INPUT_FILE), "ERROR")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(
    INPUT_FILE,
    col_names    = CANONICAL_COLS,
    col_types    = cols(.default = "c"),
    show_col_types = FALSE,
    locale       = locale(encoding = "UTF-8")
  )

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data rỗng.", "WARN")
    return(invisible(NULL))
  }

  log_message(SCRIPT_NAME, sprintf("Đọc được %d dòng từ %s", nrow(raw), INPUT_FILE))

  # ── BƯỚC 1: Loại bỏ dòng header nếu crawler lỡ cào trúng dòng tiêu đề ───────
  df <- raw %>%
    filter(!brand %in% c("brand", "Brand"))

  # ── BƯỚC 2: Áp dụng toàn bộ cleaning chung từ utils.R ───────────────────────
  # Bao gồm:
  #   - normalize_na: empty/"NA"/"Đang cập nhật"/"-" → NA
  #   - clean_price: "849 triệu" → 849000000L
  #   - clean_mileage: loại "km", dấu phân cách → integer
  #   - clean_engine_size: "1.5L" / cc → float lít
  #   - clean_year / clean_seat_count → integer
  #   - clean_posted_date: "Hôm nay" / "1 ngày trước" → "DD-MM-YYYY"
  #   - clean_city: "Quận/Huyện: Quận 7, Hồ Chí Minh" → "Hồ Chí Minh"
  #   - clean_fuel_type, clean_transmission, clean_body_type
  #   - clean_drivetrain, clean_origin
  #   - brand/model → UPPERCASE
  df_clean <- standardize_car_data(df) %>%
    # Xóa các dòng lỗi (không có brand)
    filter(!is.na(brand) & brand != "") %>%
    # Xóa các hãng xe tải (brand đã được standardize thành UPPERCASE)
    filter(!brand %in% c("DONGFENG", "HINO", "ISUZU", "JAC", "FAW")) %>%
    # Áp business rule chung: year/price/mileage/url/model hợp lệ và URL duy nhất
    apply_business_rules()

  # ── BƯỚC 3: Ghi output ───────────────────────────────────────────────────────
  safe_write_csv(df_clean, OUTPUT_FILE)

  log_message(SCRIPT_NAME, sprintf(
    "=== Hoàn thành. %d dòng đã được lưu tại: %s ===",
    nrow(df_clean), OUTPUT_FILE
  ))

  invisible(df_clean)
}

# Chạy khi được source
clean_banxehoicu()
