# Real-time Bonbanh delta fetch.
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
  library(purrr)
})

source("script/utils.R")

SCRIPT_NAME <- "realtime_bonbanh.R"
DB_FILE <- "data/master_data.db"
TABLE_NAME <- "car_listings"
LISTING_URL <- "https://bonbanh.com/oto/page,1"
SOURCE_NAME <- "bonbanh.com"

get_spec <- function(text, keyword) {
  pattern <- paste0("(?i)", keyword, "\\s*[:\\n]*\\s*([^\\n]+)")
  val <- str_match(text, pattern)[,2]
  return(ifelse(is.na(val), NA_character_, str_squish(val)))
}

scrape_detail_page <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  full_text <- page %>% html_text2()
  
  #BÓC TÁCH TIÊU ĐỀ
  raw_h1 <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  parts <- str_split(raw_h1, "-")[[1]]
  car_name_part <- str_squish(parts[1])
  price_part <- ifelse(length(parts) > 1, str_squish(parts[length(parts)]), full_text)
  
  clean_title <- str_squish(str_remove_all(car_name_part, "(?i)^(Bán xe ô tô|Bán xe|Xe)\\s+"))
  words <- str_split(clean_title, " ")[[1]]
  
  if (toupper(words[1]) %in% c("MERCEDES", "LAND", "ASTON", "ROLLS") && length(words) >= 2) {
    raw_brand <- paste(words[1], words[2])
    raw_model <- ifelse(length(words) >= 3, words[3], NA_character_)
    raw_trim <- ifelse(length(words) >= 4, paste(words[4:length(words)], collapse = " "), NA_character_)
  } else {
    raw_brand <- words[1]
    raw_model <- ifelse(length(words) >= 2, words[2], NA_character_)
    raw_trim <- ifelse(length(words) >= 3, paste(words[3:length(words)], collapse = " "), NA_character_)
  }
  
  raw_year <- str_extract(clean_title, "[0-9]{4}")
  
  # Làm sạch đặc thù cho Trim
  if (!is.na(raw_trim)) {
    if (!is.na(raw_year)) raw_trim <- str_remove_all(raw_trim, raw_year)
    raw_trim <- str_remove_all(raw_trim, "[0-9]+[.,][0-9]+\\s*L?") 
    raw_trim <- str_remove_all(raw_trim, "(?i)\\b(AT|MT|CVT)\\b") 
    raw_trim <- ifelse(str_squish(raw_trim) == "", "BẢN THƯỜNG", str_squish(raw_trim))
  }
  
  # Xử lý giá tiền 
  calculated_price <- case_when(
    str_detect(price_part, "(?i)Tỷ") & str_detect(price_part, "(?i)Triệu|Tr") ~ 
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*Tỷ)")) * 10^9 + 
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*(Triệu|Tr))")) * 10^6,
    str_detect(price_part, "(?i)Tỷ") ~ as.numeric(str_extract(price_part, "[0-9]+")) * 10^9,
    str_detect(price_part, "(?i)Triệu|Tr") ~ as.numeric(str_extract(price_part, "[0-9]+")) * 10^6,
    TRUE ~ NA_real_
  )
  
  # XỬ LÝ NHIÊN LIỆU & DUNG TÍCH TỪ Ô "ĐỘNG CƠ"
  dong_co_raw <- get_spec(full_text, "Động cơ")
  if (!is.na(dong_co_raw) && dong_co_raw != "") {
    raw_fuel <- dong_co_raw
    raw_engine <- str_extract(dong_co_raw, "[0-9]+[.,][0-9]+")
  } else {
    raw_fuel <- clean_title # Ném cả title vào cho utils.R tự lọc chữ Hybrid/Điện
    raw_engine <- str_extract(ifelse(is.na(raw_trim), "", raw_trim), "[0-9]+[.,][0-9]+")
  }
  
  # TRẢ VỀ TIBBLE THÔ 
  tibble(
    brand = raw_brand, 
    model = raw_model, 
    trim = raw_trim, 
    year = raw_year,
    body_type = get_spec(full_text, "Kiểu dáng"), 
    fuel_type = raw_fuel, 
    transmission = get_spec(full_text, "Hộp số"),
    engine_size = raw_engine, 
    seat_count = get_spec(full_text, "Số chỗ ngồi"), 
    drivetrain = get_spec(full_text, "Dẫn động"),
    price = calculated_price, 
    mileage = get_spec(full_text, "Số Km đã đi"), 
    origin = get_spec(full_text, "Xuất xứ"), 
    color = get_spec(full_text, "Màu ngoại thất"),
    city = get_spec(full_text, "Nơi bán"), 
    posted_date = as.character(Sys.Date()), 
    source = SOURCE_NAME, 
    url = url
  )
}

insert_new_bonbanh_records <- function() {
  cat("\nStarting Bonbanh real-time delta fetch...\n")
  log_message(SCRIPT_NAME, "Starting Bonbanh real-time delta fetch.")
  
  if (!file.exists(DB_FILE)) stop("Database does not exist.")
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  listing_page <- tryCatch(read_html(LISTING_URL), error = function(e) NULL)
  if (is.null(listing_page)) return(0L)
  
  links <- listing_page %>% html_nodes(".car-item a") %>% html_attr("href")
  links <- unique(links[!is.na(links) & links != ""])
  full_links <- paste0("https://bonbanh.com/", str_replace(links, "^/", ""))
  
  inserted <- 0L
  cat(sprintf("Found %s candidate links. Checking database...\n", length(full_links)))
  
  for (url in full_links) {
    # Check DB - Delta Fetch
    if (DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s WHERE url = ?", TABLE_NAME), params = list(url))$n > 0) {
      cat(sprintf("Encountered existing record (%s). Breaking loop.\n", url))
      break
    }
    
    raw_row <- tryCatch(scrape_detail_page(url), error = function(e) NULL)
    if (!is.null(raw_row) && nrow(raw_row) > 0) {
    
      clean_row <- standardize_car_data(raw_row) %>% mutate(posted_date = as.character(posted_date))
      
      DBI::dbWriteTable(con, TABLE_NAME, clean_row, append = TRUE)
      inserted <- inserted + 1L
      cat(sprintf("Inserted: %s\n", url))
    }
    Sys.sleep(1) # Nghỉ để tránh block
  }
  cat(sprintf("Real-time fetch completed. %s new records inserted.\n", inserted))
}

insert_new_bonbanh_records()