# Tắt ký hiệu khoa học
options(scipen = 999)
library(rvest)
library(dplyr)
library(stringr)

# THU THẬP LINK XE VÀ NƠI BÁN TỪ TRANG TỔNG
so_trang <- 300 
df_links <- data.frame(url = character(), city = character(), stringsAsFactors = FALSE)

cat("Đang thu thập link xe và Nơi bán...\n")
for (i in 1:so_trang) {
  url_page <- paste0("https://bonbanh.com/oto/page,", i)
  page_html <- tryCatch(read_html(url_page), error = function(e) NULL)
  
  if (!is.null(page_html)) {
    car_nodes <- page_html %>% html_nodes(".car-item")
    
    for(node in car_nodes) {
      link_node <- node %>% html_node("a") %>% html_attr("href")
      if(is.na(link_node) || link_node == "") next
      
      # Lấy URL
      url_val <- paste0("https://bonbanh.com/", str_replace(link_node, "^/", ""))
      
      # Lấy City
      city_raw <- node %>% html_node(".cb4") %>% html_text(trim = TRUE)
      city_val <- ifelse(!is.na(city_raw), str_trim(str_replace_all(city_raw, "(?i)nơi bán:", "")), "Chưa rõ")
      
      df_links <- bind_rows(df_links, data.frame(url = url_val, city = city_val, stringsAsFactors = FALSE))
    }
  }
  cat("Đã quét trang", i, "- Tổng link lấy được:", nrow(df_links), "\n")
  Sys.sleep(1) # Nghỉ 1s chống block
}

# Lọc trùng lặp link để tránh cào lại
df_links <- distinct(df_links, url, .keep_all = TRUE)
cat("\n\BẮT ĐẦU VÀO TỪNG XE ĐỂ CÀO...\n")

# Hàm phụ trợ tìm thông số
get_spec <- function(text, keyword) {
  pattern <- paste0("(?i)", keyword, "\\s*[:\\n]*\\s*([^\\n]+)")
  val <- str_match(text, pattern)[,2]
  return(ifelse(is.na(val), NA_character_, str_squish(val)))
}

data_18_cols <- data.frame()

for (j in 1:nrow(df_links)) {
  car_url <- df_links$url[j]
  txt_city <- df_links$city[j] # Dùng City chuẩn
  
  car_page <- tryCatch(read_html(car_url), error = function(e) NULL)
  if (is.null(car_page)) next
  
  # Dùng html_text2() để giữ nguyên cấu trúc xuống dòng
  full_text <- car_page %>% html_text2()
  
  # XỬ LÝ TIÊU ĐỀ & GIÁ BÁN 
  raw_h1 <- car_page %>% html_node("h1") %>% html_text(trim = TRUE)
  parts <- str_split(raw_h1, "-")[[1]]
  car_name_part <- str_squish(parts[1])
  price_part <- ifelse(length(parts) > 1, str_squish(parts[length(parts)]), full_text)
  
  clean_title <- str_squish(str_remove_all(car_name_part, "(?i)^(Bán xe ô tô|Bán xe|Xe)\\s+"))
  words <- str_split(clean_title, " ")[[1]]
  
  # Xử lý Hãng xe có 2 chữ (Mercedes, Land Rover...)
  if (toupper(words[1]) %in% c("MERCEDES", "LAND", "ASTON", "ROLLS") && length(words) >= 2) {
    txt_brand <- paste(toupper(words[1]), toupper(words[2]))
    txt_model <- ifelse(length(words) >= 3, toupper(words[3]), NA_character_)
    txt_trim_raw <- ifelse(length(words) >= 4, toupper(paste(words[4:length(words)], collapse = " ")), NA_character_)
  } else {
    txt_brand <- toupper(words[1]) 
    txt_model <- ifelse(length(words) >= 2, toupper(words[2]), NA_character_)
    txt_trim_raw <- ifelse(length(words) >= 3, toupper(paste(words[3:length(words)], collapse = " ")), NA_character_)
  }
  
  num_year <- as.integer(str_extract(clean_title, "[0-9]{4}"))
  
  # Làm sạch cột Trim
  if (!is.na(txt_trim_raw)) {
    txt_trim <- txt_trim_raw
    if (!is.na(num_year)) txt_trim <- str_remove_all(txt_trim, as.character(num_year))
    txt_trim <- str_remove_all(txt_trim, "[0-9]+[.,][0-9]+\\s*L?") 
    txt_trim <- str_remove_all(txt_trim, "(?i)\\b(AT|MT|CVT)\\b") 
    txt_trim <- str_squish(txt_trim) 
    txt_trim <- ifelse(txt_trim == "", "BẢN THƯỜNG", txt_trim)
  } else {
    txt_trim <- NA_character_
  }
  
  # Xử lý Giá tiền
  num_price <- case_when(
    str_detect(price_part, "(?i)Tỷ") & str_detect(price_part, "(?i)Triệu|Tr") ~ 
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*Tỷ)")) * 10^9 + 
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*(Triệu|Tr))")) * 10^6,
    str_detect(price_part, "(?i)Tỷ") ~ as.numeric(str_extract(price_part, "[0-9]+")) * 10^9,
    str_detect(price_part, "(?i)Triệu|Tr") ~ as.numeric(str_extract(price_part, "[0-9]+")) * 10^6,
    TRUE ~ NA_real_
  )
  
  #BỐC CÁC THÔNG SỐ CHI TIẾT 
  km_raw <- get_spec(full_text, "Số Km đã đi")
  num_mileage <- as.integer(str_replace_all(km_raw, "[^0-9]", ""))
  
  txt_body_type <- get_spec(full_text, "Kiểu dáng")
  txt_trans     <- get_spec(full_text, "Hộp số")
  txt_drive     <- get_spec(full_text, "Dẫn động")
  txt_origin    <- get_spec(full_text, "Xuất xứ")
  txt_color     <- get_spec(full_text, "Màu ngoại thất")
  
  seat_raw <- get_spec(full_text, "Số chỗ ngồi")
  num_seats <- as.integer(str_replace_all(seat_raw, "[^0-9]", ""))
  
  # XỬ LÝ KẾP HỢP ĐỘNG CƠ (Nhiên liệu + Dung tích
  dong_co_raw <- get_spec(full_text, "Động cơ")
  
  if (!is.na(dong_co_raw) && dong_co_raw != "") {
    txt_fuel <- case_when(
      str_detect(dong_co_raw, "(?i)Dầu|Diesel") ~ "Dầu",
      str_detect(dong_co_raw, "(?i)Hybrid|Lai") ~ "Hybrid",
      str_detect(dong_co_raw, "(?i)Điện") ~ "Điện",
      TRUE ~ "Xăng" 
    )
  } else {
    txt_fuel <- case_when(
      str_detect(clean_title, "(?i)Máy dầu|Diesel") ~ "Dầu",
      str_detect(clean_title, "(?i)Hybrid|HEV|PHEV") ~ "Hybrid",
      str_detect(clean_title, "(?i)\\bEV\\b|VF") ~ "Điện",
      TRUE ~ "Xăng"
    )
  }
  
  if (!is.na(dong_co_raw) && str_detect(dong_co_raw, "[0-9]+[.,][0-9]+")) {
    num_engine <- as.numeric(str_replace_all(str_extract(dong_co_raw, "[0-9]+[.,][0-9]+"), ",", "."))
  } else {
    num_engine <- as.numeric(str_replace_all(str_extract(txt_trim_raw, "[0-9]+[.,][0-9]+"), ",", "."))
  }
  
  # ĐẨY VÀO DATAFRAM
  temp_df <- data.frame(
    brand = txt_brand, model = txt_model, trim = txt_trim, year = num_year,
    body_type = txt_body_type, fuel_type = txt_fuel, transmission = txt_trans,
    engine_size = num_engine, seat_count = num_seats, drivetrain = txt_drive,
    price = num_price, mileage = num_mileage, origin = txt_origin, color = txt_color,
    city = txt_city, posted_date = format(Sys.Date(), "%d-%m-%Y"), source = "bonbanh.com", url = car_url,
    stringsAsFactors = FALSE
  )
  
  data_18_cols <- bind_rows(data_18_cols, temp_df)
  
  cat("Đã cào xong xe", j, "/", nrow(df_links), "\n")
  Sys.sleep(1) # Nghỉ 1s chống khóa IP
}

# LƯU FILE
duong_dan <- "D:/Huy/DAHO/Nam2/Hk2/Dot_2/LT_R/project/data_bonbanh_raw.csv"
write.csv(data_18_cols, duong_dan, row.names = FALSE, fileEncoding = "UTF-8")
cat("\nHOÀN TẤT! Dữ liệu đã lưu tại:", duong_dan, "\n")