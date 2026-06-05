library(dplyr)
library(stringr)

# 1. Đọc data thô (Đổi đường dẫn nếu bạn chạy trên máy cá nhân)
df_raw <- read.csv("D:/Huy/DAHO/Nam2/Hk2/Dot_2/LT_R/project/data_bonbanh_raw.csv", stringsAsFactors = FALSE)

# 2. Thực thi Data Cleaning Rules
df_clean <- df_raw %>%
  # Xóa khoảng trắng thừa và ép chữ HOA cho brand, model
  mutate(
    brand = toupper(str_squish(brand)),
    model = toupper(str_squish(model))
  ) %>%
  # Biến tất cả các ô trống ("") hoặc chuỗi không xác định thành NA
  mutate(across(where(is.character), ~str_squish(.))) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.character), ~ifelse(. %in% c("Chưa rõ", "NA", "null"), NA_character_, .))) %>%
  
  # Chuẩn hóa danh mục (transmission, fuel_type) theo format chung
  mutate(
    transmission = case_when(
      str_detect(transmission, "(?i)tự động") ~ "Số tự động",
      str_detect(transmission, "(?i)sàn|tay") ~ "Số sàn",
      TRUE ~ transmission
    ),
    fuel_type = case_when(
      str_detect(fuel_type, "(?i)xăng") ~ "Xăng",
      str_detect(fuel_type, "(?i)dầu") ~ "Dầu",
      str_detect(fuel_type, "(?i)điện") ~ "Điện",
      str_detect(fuel_type, "(?i)hybrid") ~ "Hybrid",
      TRUE ~ fuel_type
    )
  ) %>%
  
  # Ép chuẩn kiểu dữ liệu lần cuối theo mục 2
  mutate(
    year = as.integer(year),
    price = as.numeric(price),
    mileage = as.integer(mileage),
    engine_size = as.numeric(engine_size),
    seat_count = as.integer(seat_count)
  )

# LƯU FILE
duong_dan <- "D:/Huy/DAHO/Nam2/Hk2/Dot_2/LT_R/project/data_bonbanh_clean.csv"
write.csv(df_clean, duong_dan, row.names = FALSE, fileEncoding = "UTF-8")
cat("\nHOÀN TẤT! Dữ liệu đã lưu tại:", duong_dan, "\n")