# ============================================================
# FILE: Cleaning_Data.R
# File này dùng để lọc dữ liệu cho file Probability_statistics.R
# ============================================================

library(dplyr)
library(readr)

setwd("D:/Trung Khang/Documents/R_programming/Project_cuoiky/web-scraping-used-cars/Xác suất thống kê mô tả")

OUTPUT_DIR <- "output_probability_statistics"
CLEAN_FILE <- file.path(OUTPUT_DIR, "00_data_da_lam_sach.csv")
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# CÁC HÀM TIỆN ÍCH TIỀN XỬ LÝ
to_number <- function(x) suppressWarnings(as.numeric(as.character(x)))

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) == "NA" | toupper(x) == "N/A"] <- NA
  x
}

normalize_transmission <- function(x) {
  x <- tolower(clean_text(x))
  out <- rep(NA_character_, length(x))

  out[x %in% c("manual", "mt", "số sàn")] <- "Manual"
  out[x %in% c("automatic", "auto", "at", "số tự động")] <- "Automatic"
  out[x %in% c("cvt")] <- "CVT"
  out[x %in% c("robot", "robotic")] <- "Robot"
  out
}

# ĐỌC ĐỒNG THỜI VÀ GỘP TOÀN BỘ FILE DỮ LIỆU ĐỘNG TỪ BỘ PHẬN CÀO DATA
danh_sach_file <- list.files(
  path = "D:/Trung Khang/Documents/R_programming/Project_cuoiky/web-scraping-used-cars/data", 
  pattern = ".*_clean\\.csv$", 
  full.names = TRUE
)

data_raw <- lapply(danh_sach_file, function(file) {
  read.csv(file, stringsAsFactors = FALSE, colClasses = "character") 
}) %>% bind_rows()

cat("Đã nạp thành công", length(danh_sach_file), "file dữ liệu!\n")
cat("Tổng số dòng dữ liệu thô thu được:", nrow(data_raw), "\n")

# CHUẨN HÓA THÔNG TIN CỐT LÕI
required_cols <- c("brand", "year", "price", "mileage", "transmission")
missing_cols <- setdiff(required_cols, names(data_raw))
if (length(missing_cols) > 0) {
  stop("Thiếu cột bắt buộc trong tập dữ liệu gộp: ", paste(missing_cols, collapse = ", "))
}

data_clean <- data_raw

data_clean$brand <- clean_text(data_clean$brand)
data_clean$year <- to_number(data_clean$year)
data_clean$price_raw <- to_number(data_clean$price)
# Loại bỏ hoàn toàn các ký tự chữ lạ dính trong cột số Km (Odo)
data_clean$mileage <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(data_clean$mileage))))
data_clean$transmission <- normalize_transmission(data_clean$transmission)

data_clean$price_scale <- ifelse(data_clean$price_raw < 1000, "Small scale x100000", "Original scale")
data_clean$price <- ifelse(data_clean$price_raw < 1000, data_clean$price_raw * 100000, data_clean$price_raw)

# LỌC OUTLIERS VÀ FEATURE ENGINEERING
valid_rows <-
  !is.na(data_clean$brand) &
  !is.na(data_clean$year) & data_clean$year >= 1980 & data_clean$year <= CURRENT_YEAR + 1 &
  !is.na(data_clean$price) & data_clean$price > 0 &
  !is.na(data_clean$mileage) & data_clean$mileage >= 0 & data_clean$mileage < 500000 &
  !is.na(data_clean$transmission)

data_clean <- data_clean[valid_rows, ]

if (nrow(data_clean) == 0) stop("Không có dữ liệu hợp lệ sau khi làm sạch.")

data_clean$age <- CURRENT_YEAR - data_clean$year
data_clean$age_group <- cut(data_clean$age, breaks = c(-Inf, 3, 7, 12, Inf), labels = c("0-3 nam", "4-7 nam", "8-12 nam", "Tren 12 nam"), right = TRUE)

price_q75 <- quantile(data_clean$price, 0.75, na.rm = TRUE)
mileage_q75 <- quantile(data_clean$mileage, 0.75, na.rm = TRUE)

data_clean$is_high_price <- data_clean$price >= price_q75
data_clean$is_high_mileage <- data_clean$mileage >= mileage_q75

# KẾT XUẤT FILE SẠCH SANG THƯ MỤC ĐÚNG YÊU CẦU
overview <- data.frame(
  metric = c("Rows before cleaning", "Rows after cleaning", "Rows removed", "Unique brands", "Min year", "Max year"),
  value = c(nrow(data_raw), nrow(data_clean), nrow(data_raw) - nrow(data_clean), length(unique(data_clean$brand)), min(data_clean$year, na.rm = TRUE), max(data_clean$year, na.rm = TRUE))
)

write.csv(data_clean, CLEAN_FILE, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(overview, file.path(OUTPUT_DIR, "00_tong_quan_lam_sach.csv"), row.names = FALSE)

cat("\n=== TIỀN XỬ LÝ DỮ LIỆU HOÀN TẤT ===\n")
print(overview, row.names = FALSE)
cat("Tập dữ liệu sạch tổng hợp đã lưu tại: ", CLEAN_FILE, "\n", sep = "")
