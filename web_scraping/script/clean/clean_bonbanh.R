# ==============================================================================
# Script: clean_bonbanh.R
# Purpose: Clean & standardise scraped raw data from bonbanh.com
# Input : web_scraping/data/raw/data_bonbanh_raw.csv
# Output: web_scraping/data/clean/data_bonbanh_clean.csv
# Requires: script/utils.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/clean/clean_bonbanh.R"
INPUT_FILE  <- "web_scraping/data/raw/data_bonbanh_raw.csv"
OUTPUT_FILE <- "web_scraping/data/clean/data_bonbanh_clean.csv"

# ==============================================================================
clean_bonbanh <- function() {
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, "=== Bắt đầu cleaning dữ liệu BonBanh ===")

  # ── Đọc file raw ─────────────────────────────────────────────────────────────
  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, paste("Input file not found:", INPUT_FILE), "ERROR")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(
    INPUT_FILE,
    col_types      = cols(.default = "c"),
    show_col_types = FALSE,
    locale         = locale(encoding = "UTF-8")
  )

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data rỗng.", "WARN")
    return(invisible(NULL))
  }

  log_message(SCRIPT_NAME, sprintf("Đọc được %d dòng từ %s", nrow(raw), INPUT_FILE))

  # ── BƯỚC 1: Pre-clean đặc thù BonBanh trước khi standardize ─────────────────

  df <- raw %>%
    mutate(
      # Sửa lỗi: Chuyển "Số tay" thành "Số sàn" theo yêu cầu
      transmission = ifelse(str_detect(tolower(transmission), "tay"), "Số sàn", transmission),

      # Drivetrain: bỏ phần mô tả dài, chỉ giữ mã chuẩn (FWD / RWD / 4WD / AWD)
      # VD: "FWD - Dẫn động cầu trước"  → "FWD"
      #     "RFD - Dẫn động cầu sau"     → "RWD"  (note: BonBanh viết "RFD", sửa lại)
      #     "4WD - Dẫn động 4 bánh"      → "4WD"
      #     "AWD - 4 bánh toàn thời gian"→ "AWD"
      drivetrain = case_when(
        str_detect(drivetrain, regex("^FWD", ignore_case = TRUE)) ~ "FWD",
        str_detect(drivetrain, regex("^RFD|^RWD|cầu sau", ignore_case = TRUE)) ~ "RWD",
        str_detect(drivetrain, regex("^4WD|4 bánh", ignore_case = TRUE)) ~ "4WD",
        str_detect(drivetrain, regex("^AWD|toàn thời gian", ignore_case = TRUE)) ~ "AWD",
        TRUE ~ drivetrain
      ),

      # Origin: chuẩn hóa về 2 giá trị theo schema
      # VD: "Lắp ráp trong nước" → "Trong nước"
      #     "Nhập khẩu"          → "Nhập khẩu" (giữ nguyên)
      origin = case_when(
        str_detect(origin, regex("lắp ráp|trong nước", ignore_case = TRUE)) ~ "Trong nước",
        str_detect(origin, regex("nhập khẩu|nhập", ignore_case = TRUE))     ~ "Nhập khẩu",
        TRUE ~ origin
      ),

      # Sửa lỗi đặc thù: Peugeot 5008 bị lấy nhầm năm sản xuất là 5008 và trim chứa năm
      year = ifelse(toupper(brand) == "PEUGEOT" & model == "5008" & year == "5008", "2022", year),
      trim = ifelse(toupper(brand) == "PEUGEOT" & model == "5008" & trim == "GT 2022", "GT", trim),

      # Sửa lỗi đặc thù: Peugeot 3008 (và bất kỳ model nào) bị lấy nhầm year = tên model,
      # trong khi năm thực tế nằm cuối cột trim (VD: "ALLURE 2020", "GT 2022", "2019").
      # Logic: nếu year == model → trích năm 4 chữ số từ cuối trim, xóa năm đó khỏi trim.
      temp_peugeot_fix = toupper(brand) == "PEUGEOT" & (year == model),
      temp_year_from_trim = ifelse(
        temp_peugeot_fix,
        str_extract(trim, "(?<![0-9])\\d{4}(?![0-9])"),   # năm 4 chữ số trong trim
        NA_character_
      ),
      year = ifelse(temp_peugeot_fix & !is.na(temp_year_from_trim), temp_year_from_trim, year),
      trim = ifelse(
        temp_peugeot_fix,
        {
          t <- str_trim(str_remove(trim, "(?<![0-9])\\d{4}(?![0-9])"))
          ifelse(t == "" | is.na(t), NA_character_, t)   # trim chỉ có năm → NA
        },
        trim
      )
    ) %>%
    select(-temp_peugeot_fix, -temp_year_from_trim)

  # ── BƯỚC 2: Áp dụng toàn bộ cleaning chung từ utils.R ───────────────────────
  # Bao gồm:
  #   - normalize_na: empty/"NA"/"Đang cập nhật"/"-" → NA
  #   - clean_price: "385000000" → 385000000L
  #   - clean_mileage: loại "km", dấu phân cách → integer
  #   - clean_engine_size: "1.5" → 1.5
  #   - clean_year / clean_seat_count → integer
  #   - clean_posted_date: "05-06-2026" → "05-06-2026"
  #   - clean_city: giữ tỉnh/thành cuối cùng
  #   - clean_fuel_type, clean_transmission, clean_body_type
  #   - clean_drivetrain (đã pre-clean ở Bước 1)
  #   - clean_origin (đã pre-clean ở Bước 1)
  #   - brand/model → UPPERCASE
  df_clean <- standardize_car_data(df) %>%
    # Xóa các dòng lỗi (không có brand)
    filter(!is.na(brand) & brand != "") %>%
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
clean_bonbanh()
