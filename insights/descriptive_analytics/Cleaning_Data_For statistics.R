# ============================================================
# FILE: Cleaning_Data_For statistics.R
# Purpose: Prepare clean data for Probability_statistics.R
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

OUTPUT_DIR <- "insights/descriptive_analytics/output_probability_statistics"
CLEAN_FILE <- file.path(OUTPUT_DIR, "00_data_da_lam_sach.csv")
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

to_number <- function(x) suppressWarnings(as.numeric(as.character(x)))

clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == "" | toupper(x) %in% c("NA", "N/A", "NULL")] <- NA
  x
}

normalize_transmission <- function(x) {
  y <- str_to_lower(clean_text(x))
  case_when(
    y %in% c("automatic", "auto", "at", "số tự động", "so tu dong", "tự động", "tu dong") ~ "Tự động",
    y %in% c("manual", "mt", "số sàn", "so san", "sàn", "san", "số tay") ~ "Số sàn",
    y == "cvt" ~ "CVT",
    TRUE ~ NA_character_
  )
}

data_raw <- read_master_data()

cat("Đã nạp dữ liệu master cho thống kê:", nrow(data_raw), "dòng.\n")

required_cols <- c("brand", "year", "price", "mileage", "transmission", "source", "fuel_type")
missing_cols <- setdiff(required_cols, names(data_raw))
if (length(missing_cols) > 0) {
  stop("Thiếu cột bắt buộc trong tập dữ liệu master: ", paste(missing_cols, collapse = ", "))
}

data_clean <- data_raw %>%
  mutate(
    brand = clean_text(brand),
    year = to_number(year),
    price_raw = to_number(price),
    price = price_raw,
    mileage = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(mileage)))),
    transmission = normalize_transmission(transmission),
    fuel_type = clean_text(fuel_type),
    source = clean_text(source),
    price_scale = "Original scale"
  ) %>%
  filter(
    !is.na(brand),
    !is.na(year), year >= 1990, year <= CURRENT_YEAR,
    !is.na(price), price >= 5e7, price <= 1.5e10,
    !is.na(mileage), mileage >= 0, mileage <= 1e6,
    !is.na(transmission)
  )

if (nrow(data_clean) == 0) stop("Không có dữ liệu hợp lệ sau khi làm sạch.")

data_clean <- data_clean %>%
  mutate(
    age = CURRENT_YEAR - year,
    age_group = cut(
      age,
      breaks = c(-Inf, 3, 7, 12, Inf),
      labels = c("0-3 nam", "4-7 nam", "8-12 nam", "Tren 12 nam"),
      right = TRUE
    )
  )

price_q75 <- quantile(data_clean$price, 0.75, na.rm = TRUE)
mileage_q75 <- quantile(data_clean$mileage, 0.75, na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(
    is_high_price = price >= price_q75,
    is_high_mileage = mileage >= mileage_q75
  )

overview <- data.frame(
  metric = c("Rows before cleaning", "Rows after cleaning", "Rows removed", "Unique brands", "Min year", "Max year"),
  value = c(
    nrow(data_raw),
    nrow(data_clean),
    nrow(data_raw) - nrow(data_clean),
    length(unique(data_clean$brand)),
    min(data_clean$year, na.rm = TRUE),
    max(data_clean$year, na.rm = TRUE)
  )
)

write.csv(data_clean, CLEAN_FILE, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(overview, file.path(OUTPUT_DIR, "00_tong_quan_lam_sach.csv"), row.names = FALSE)

cat("\n=== TIỀN XỬ LÝ DỮ LIỆU THỐNG KÊ HOÀN TẤT ===\n")
print(overview, row.names = FALSE)
cat("Tập dữ liệu sạch tổng hợp đã lưu tại: ", CLEAN_FILE, "\n", sep = "")
