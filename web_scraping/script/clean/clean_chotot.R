# ==============================================================================
# Script: clean_chotot.R
# Purpose: Clean & standardise scraped raw data from xe.chotot.com
# Input : web_scraping/data/raw/data_chotot_raw.csv
# Output: web_scraping/data/clean/data_chotot_clean.csv
# Requires: script/utils.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/clean/clean_chotot.R"
INPUT_FILE  <- "web_scraping/data/raw/data_chotot_raw.csv"
OUTPUT_FILE <- "web_scraping/data/clean/data_chotot_clean.csv"

# ── Bảng mapping mã số brand của Chợ Tốt → tên thương hiệu ───────────────────
# (Chợ Tốt đôi khi trả về mã số thay vì tên trong cột brand)
CHOTOT_BRAND_MAP <- c(
  "1"  = "Kia",          "2"  = "Toyota",      "3"  = "Ford",
  "4"  = "Chevrolet",    "5"  = "Hyundai",      "6"  = "Honda",
  "7"  = "Mazda",        "8"  = "Audi",         "9"  = "BMW",
  "10" = "Daewoo",       "13" = "Isuzu",        "14" = "Jeep",
  "15" = "Lexus",        "16" = "Mercedes-Benz","18" = "Mitsubishi",
  "19" = "Nissan",       "20" = "Peugeot",      "21" = "Smart",
  "22" = "Suzuki",       "23" = "Volkswagen",   "24" = "Jaecoo",
  "27" = "Asia",         "32" = "BYD",          "35" = "Omoda",
  "37" = "Citroen",      "42" = "Geely",        "48" = "Jaguar",
  "51" = "Land Rover",   "60" = "MG",           "63" = "Porsche",
  "68" = "Samsung",      "71" = "Subaru",       "76" = "Volvo",
  "80" = "VinFast",      "83" = "Haval",        "84" = "Skoda",
  "85" = "Lynk & Co",    "87" = "Wuling",       "88" = "GAC",
  "92" = "Dongfeng"
)

# ── Bảng mapping mã số body_type, fuel_type, transmission của Chợ Tốt ─────────
# (Chợ Tốt đôi khi trả về mã số nguyên trong các cột này)
CHOTOT_BODY_MAP <- c(
  "1" = "Sedan", "2" = "Coupe",      "3" = "SUV",
  "4" = "Hatchback", "6" = "Bán tải", "7" = "Mui trần",
  "8" = "MPV",   "9" = "Van/Minibus"
)

CHOTOT_FUEL_MAP <- c(
  "1" = "Xăng", "2" = "Dầu", "3" = "Hybrid", "4" = "Điện"
)

CHOTOT_TRANS_MAP <- c(
  "1" = "Tự động", "2" = "Số sàn", "3" = "CVT"
)

# ==============================================================================
clean_chotot <- function() {
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, "=== Bắt đầu cleaning dữ liệu Chợ Tốt ===")

  # ── Đọc file raw ─────────────────────────────────────────────────────────────
  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, paste("Input file not found:", INPUT_FILE), "ERROR")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(
    INPUT_FILE,
    col_types = cols(.default = "c"),
    locale    = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data rỗng.", "WARN")
    return(invisible(NULL))
  }

  log_message(SCRIPT_NAME, sprintf("Đọc được %d dòng từ %s", nrow(raw), INPUT_FILE))

  # ── BƯỚC 1: Giải mã mã số Chợ Tốt → giá trị text (chỉ khi là mã số) ────────
  df <- raw %>%
    mutate(
      # Brand: nếu là mã số thì map, không thì giữ nguyên
      brand = ifelse(
        !is.na(brand) & str_detect(brand, "^[0-9]+$"),
        CHOTOT_BRAND_MAP[brand],
        brand
      ),

      # Body type: nếu là mã số thì map, nếu là "Kiểu dáng khác" thì chuyển sang NA
      body_type = case_when(
        !is.na(body_type) & str_detect(body_type, "^[0-9]+$") ~ CHOTOT_BODY_MAP[body_type],
        str_detect(tolower(body_type), "kiểu dáng khác") ~ NA_character_,
        TRUE ~ body_type
      ),

      # Fuel type: nếu là mã số thì map
      fuel_type = ifelse(
        !is.na(fuel_type) & str_detect(fuel_type, "^[0-9]+$"),
        CHOTOT_FUEL_MAP[fuel_type],
        fuel_type
      ),

      # Transmission: nếu là mã số thì map
      transmission = ifelse(
        !is.na(transmission) & str_detect(transmission, "^[0-9]+$"),
        CHOTOT_TRANS_MAP[transmission],
        transmission
      )
    )

  # ── BƯỚC 2: Áp dụng toàn bộ cleaning chung từ utils.R ───────────────────────
  # Bao gồm:
  #   - normalize_na: empty/"NA"/"Đang cập nhật"/"-" → NA
  #   - clean_price: "340.000.000 đ" → 340000000L
  #   - clean_mileage: loại "km", dấu phân cách → integer
  #   - clean_engine_size: "1.5L" / cc → float lít
  #   - clean_year / clean_seat_count → integer
  #   - clean_posted_date: "12 giờ trước" / "1 tháng trước" → "DD-MM-YYYY"
  #   - clean_city: lấy tỉnh/thành cuối cùng trong chuỗi phân cách bởi dấu phẩy
  #   - clean_fuel_type: "Petrol"/"Electric"/... → "Xăng"/"Điện"/...
  #   - clean_transmission: "Automatic"/"Manual" → "Tự động"/"Số sàn"
  #   - clean_body_type, clean_drivetrain, clean_origin
  #   - brand/model → UPPERCASE
  df_final <- standardize_car_data(df) %>%
    apply_business_rules()

  # ── BƯỚC 3: Ghi output ───────────────────────────────────────────────────────
  safe_write_csv(df_final, OUTPUT_FILE)

  log_message(SCRIPT_NAME, sprintf(
    "=== Hoàn thành. %d dòng đã được lưu tại: %s ===",
    nrow(df_final), OUTPUT_FILE
  ))

  invisible(df_final)
}

# Chạy khi được source
clean_chotot()
