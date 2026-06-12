# Load các thư viện cần thiết
library(dplyr)
library(readr)

# ---------------------------------------------------------
# CẤU HÌNH ĐƯỜNG DẪN TẬP TIN
# ---------------------------------------------------------
input_file <- "d:/R program/project/web_scraping/data/raw/data_chotot_raw.csv"
output_file <- "d:/R program/project/web_scraping/data/raw/data_chotot_raw.csv"

# Đọc dữ liệu thô (do file raw không có header nên gán col_names = FALSE)
df <- read_csv(input_file, col_names = FALSE, show_col_types = FALSE)

# ---------------------------------------------------------
# YÊU CẦU 4: THÊM TÊN CÁC CỘT (Dựa theo 18 cột dữ liệu thực tế)
# ---------------------------------------------------------
colnames(df) <- c(
  "Brand", "Model", "Version", "Year", "Body_Type", 
  "Fuel", "Transmission", "Engine", "Seats", "Drivetrain", 
  "Price", "Mileage", "Origin", "Condition", "Location", 
  "Posted_Time", "Source", "URL"
)

# ---------------------------------------------------------
# THỰC HIỆN LÀM SẠCH DỮ LIỆU (CLEANING)
# ---------------------------------------------------------
df_clean <- df %>%
  # Yêu cầu 3: Xóa các dòng trống không (nếu tất cả các giá trị trong dòng đều là NA)
  filter(!if_all(everything(), is.na)) %>%
  
  # Yêu cầu 1: Xóa các dòng lỗi không cào được dữ liệu (chỉ có Source và URL)
  # Dấu hiệu nhận biết: Cột "Posted_Time" (thời gian) hoặc "Brand" bị NA/trống
  filter(!is.na(Posted_Time) & Posted_Time != "") %>%
  filter(!is.na(Brand) & Brand != "") %>%
  
  # Yêu cầu 2: Xóa dòng có URL trùng lặp (chỉ giữ lại dòng xuất hiện đầu tiên/cũ nhất)
  distinct(URL, .keep_all = TRUE)

# ---------------------------------------------------------
# XUẤT FILE SAU KHI CLEAN
# ---------------------------------------------------------
# Lưu file đã làm sạch (ghi đè hoặc tạo mới trong folder clean)
write_csv(df_clean, output_file)

message(sprintf("[THÀNH CÔNG] Dữ liệu ban đầu: %d dòng. Dữ liệu sau khi làm sạch: %d dòng.", nrow(df), nrow(df_clean)))