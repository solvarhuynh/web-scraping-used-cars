# ==============================================================================
# Script: realtime_bonbanh.R
# Purpose: Delta scrape bonbanh.com — chỉ cào các URL chưa có trong DB
#
# Logic phát hiện URL mới (Bonbanh dùng HTML tĩnh, không cần Chromote):
#   - Cào trang 1 (~20 URL), so ngược URL cuối cùng với init_db
#   - Nếu URL cuối CHƯA có trong DB → cả trang chưa cào → sang trang 2
#   - Nếu URL cuối đã có → so từng URL từ cuối lên, dừng khi gặp URL đã có
#   - Ghi URLs mới vào urls_bonbanh.txt (prepend, không ghi đè)
#   - Cào chi tiết từng URL mới → data/realtime/data_bonbanh_rt.csv
#   - INSERT OR IGNORE vào init_db/data_bonbanh.db và master_data.db
#
# Output: web_scraping/data/realtime/data_bonbanh_rt.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
  library(stringr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME   <- "realtime_bonbanh.R"
SOURCE_NAME   <- "bonbanh.com"
TABLE_NAME    <- "car_listings"
BASE_URL      <- "https://bonbanh.com"
LISTING_BASE  <- "https://bonbanh.com/oto/page,"
INIT_DB_FILE  <- "web_scraping/data/init_db/data_bonbanh.db"
MASTER_DB     <- "web_scraping/data/master_data.db"
URLS_FILE     <- "web_scraping/data/raw/meta/urls_bonbanh.txt"
RT_OUTPUT_DIR <- "web_scraping/data/realtime"
RT_OUTPUT     <- file.path(RT_OUTPUT_DIR, "data_bonbanh_rt.csv")
MAX_PAGES_RT  <- 10

# ── Lấy URLs từ 1 trang listing (HTML tĩnh) ───────────────────────────────────
fetch_listing_page_urls <- function(page_num) {
  url <- paste0(LISTING_BASE, page_num)
  pg <- tryCatch(read_html(url), error = function(e) {
    log_message(SCRIPT_NAME, sprintf("Không đọc được trang %d: %s", page_num, e$message), "WARN")
    NULL
  })
  if (is.null(pg)) return(character(0))

  links <- pg %>% html_nodes(".car-item a") %>% html_attr("href")
  links <- unique(links[!is.na(links) & links != ""])
  paste0(BASE_URL, "/", str_replace(links, "^/", ""))
}

# ── Cào chi tiết 1 xe (giống scrape_detail_page từ file cũ) ──────────────────
get_spec <- function(text, keyword) {
  pattern <- paste0("(?i)", keyword, "\\s*[:\\n]*\\s*([^\\n]+)")
  val <- str_match(text, pattern)[, 2]
  ifelse(is.na(val), NA_character_, str_squish(val))
}

scrape_detail_bonbanh <- function(url) {
  pg <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(pg)) return(NULL)

  full_text <- pg %>% html_text2()
  raw_h1    <- pg %>% html_node("h1") %>% html_text(trim = TRUE)
  parts     <- str_split(raw_h1, "-")[[1]]
  car_name_part <- str_squish(parts[1])
  price_part    <- ifelse(length(parts) > 1, str_squish(parts[length(parts)]), full_text)

  clean_title <- str_squish(str_remove_all(car_name_part, "(?i)^(Bán xe ô tô|Bán xe|Xe)\\s+"))
  words <- str_split(clean_title, " ")[[1]]

  if (toupper(words[1]) %in% c("MERCEDES", "LAND", "ASTON", "ROLLS") && length(words) >= 2) {
    raw_brand <- paste(words[1], words[2])
    raw_model <- ifelse(length(words) >= 3, words[3], NA_character_)
    raw_trim  <- ifelse(length(words) >= 4, paste(words[4:length(words)], collapse = " "), NA_character_)
  } else {
    raw_brand <- words[1]
    raw_model <- ifelse(length(words) >= 2, words[2], NA_character_)
    raw_trim  <- ifelse(length(words) >= 3, paste(words[3:length(words)], collapse = " "), NA_character_)
  }

  raw_year <- str_extract(clean_title, "[0-9]{4}")

  if (!is.na(raw_trim)) {
    if (!is.na(raw_year)) raw_trim <- str_remove_all(raw_trim, raw_year)
    raw_trim <- str_remove_all(raw_trim, "[0-9]+[.,][0-9]+\\s*L?")
    raw_trim <- str_remove_all(raw_trim, "(?i)\\b(AT|MT|CVT)\\b")
    raw_trim <- ifelse(str_squish(raw_trim) == "", NA_character_, str_squish(raw_trim))
  }

  calculated_price <- case_when(
    str_detect(price_part, "(?i)Tỷ") & str_detect(price_part, "(?i)Triệu|Tr") ~
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*Tỷ)")) * 1e9 +
      as.numeric(str_extract(price_part, "[0-9]+(?=\\s*(Triệu|Tr))")) * 1e6,
    str_detect(price_part, "(?i)Tỷ") ~
      as.numeric(str_extract(price_part, "[0-9]+[.,]?[0-9]*")) * 1e9,
    str_detect(price_part, "(?i)Triệu|Tr") ~
      as.numeric(str_extract(price_part, "[0-9]+")) * 1e6,
    TRUE ~ NA_real_
  )

  dong_co_raw <- get_spec(full_text, "Động cơ")
  if (!is.na(dong_co_raw) && dong_co_raw != "") {
    raw_fuel   <- dong_co_raw
    raw_engine <- str_extract(dong_co_raw, "[0-9]+[.,][0-9]+")
  } else {
    raw_fuel   <- clean_title
    raw_engine <- str_extract(ifelse(is.na(raw_trim), "", raw_trim), "[0-9]+[.,][0-9]+")
  }

  tibble(
    brand        = raw_brand,
    model        = raw_model,
    trim         = raw_trim,
    year         = raw_year,
    body_type    = get_spec(full_text, "Kiểu dáng"),
    fuel_type    = raw_fuel,
    transmission = get_spec(full_text, "Hộp số"),
    engine_size  = raw_engine,
    seat_count   = get_spec(full_text, "Số chỗ ngồi"),
    drivetrain   = get_spec(full_text, "Dẫn động"),
    price        = as.character(calculated_price),
    mileage      = get_spec(full_text, "Số Km đã đi"),
    origin       = get_spec(full_text, "Xuất xứ"),
    color        = get_spec(full_text, "Màu ngoại thất"),
    city         = get_spec(full_text, "Nơi bán"),
    posted_date  = as.character(Sys.Date()),
    source       = SOURCE_NAME,
    url          = url
  )
}

# ── Kiểm tra URL có trong DB chưa ────────────────────────────────────────────
url_in_db <- function(con, url) {
  res <- DBI::dbGetQuery(con,
    sprintf("SELECT 1 FROM %s WHERE url = ? LIMIT 1", TABLE_NAME),
    params = list(url))
  nrow(res) > 0
}

# ── Main ──────────────────────────────────────────────────────────────────────
run_realtime_bonbanh <- function(con_master = NULL) {
  log_message(SCRIPT_NAME, "=== Bắt đầu realtime BonBanh ===")
  dir.create(RT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(URLS_FILE), recursive = TRUE, showWarnings = FALSE)

  owns_master <- is.null(con_master)
  if (owns_master) {
    con_master <- DBI::dbConnect(RSQLite::SQLite(), MASTER_DB)
    on.exit(DBI::dbDisconnect(con_master), add = TRUE)
  }
  if (!file.exists(INIT_DB_FILE)) {
    log_message(SCRIPT_NAME, paste("Không tìm thấy init DB:", INIT_DB_FILE), "ERROR")
    return(0L)
  }
  con_init <- DBI::dbConnect(RSQLite::SQLite(), INIT_DB_FILE)
  on.exit(DBI::dbDisconnect(con_init), add = TRUE)

  # ── BƯỚC 1: Phát hiện URLs mới ────────────────────────────────
  # Kiểm tra URL cuối mỗi trang: nếu chưa có → sang trang tiếp.
  # Khi gặp trang có URL cuối đã có trong DB: kiểm tra từng URL riêng lẻ
  # (Ô tránh bỏ sót URL mới xen kẽ do web thay đổi thứ tự listing)
  new_urls <- character(0)

  for (pg_num in seq_len(MAX_PAGES_RT)) {
    log_message(SCRIPT_NAME, sprintf("Kiểm tra trang %d...", pg_num))
    page_urls <- fetch_listing_page_urls(pg_num)

    if (length(page_urls) == 0) {
      log_message(SCRIPT_NAME, sprintf("Trang %d không có URL, dừng.", pg_num), "WARN")
      break
    }

    last_url   <- page_urls[length(page_urls)]
    last_in_db <- url_in_db(con_init, last_url)

    if (!last_in_db) {
      # URL cuối chưa có → kiểm tra từng URL trong trang (tránh URL đã tồn tại xen kẽ)
      page_new <- Filter(function(u) !url_in_db(con_init, u), page_urls)
      log_message(SCRIPT_NAME, sprintf(
        "Trang %d: URL cuối chưa có trong DB → lấy %d/%d URL mới.",
        pg_num, length(page_new), length(page_urls)))
      new_urls <- c(new_urls, page_new)
      Sys.sleep(1)
      next
    }

    # URL cuối đã có trong DB → kiểm tra từng URL trong trang, lấy những cái chưa có
    page_new <- Filter(function(u) !url_in_db(con_init, u), page_urls)
    log_message(SCRIPT_NAME, sprintf(
      "Trang %d: URL cuối đã có trong DB → tìm được %d/%d URL mới.",
      pg_num, length(page_new), length(page_urls)))
    new_urls <- c(new_urls, page_new)
    break  # trang này đã khóa → không cần kiểm tra trang tiếp
  }

  n_new <- length(new_urls)
  log_message(SCRIPT_NAME, sprintf("Phát hiện %d URL mới.", n_new))
  if (n_new == 0) {
    log_message(SCRIPT_NAME, "Không có URL mới. Kết thúc.")
    return(0L)
  }

  # ── BƯỚC 2: Ghi URLs mới vào file (prepend) ───────────────────────────────────
  urls_to_write  <- rev(new_urls)   # URL mới nhất lên đầu
  existing_lines <- if (file.exists(URLS_FILE)) {
    x <- readLines(URLS_FILE, warn = FALSE)
    x[nchar(trimws(x)) > 0]
  } else character(0)
  writeLines(c(urls_to_write, existing_lines), URLS_FILE)
  log_message(SCRIPT_NAME, sprintf("Đã ghi %d URL mới vào đầu: %s", n_new, URLS_FILE))

  # ── BƯỚC 3: Cào chi tiết + INSERT vào DB ──────────────────────────────────────
  batch          <- list()
  inserted_init  <- 0L
  inserted_master <- 0L

  for (i in seq_along(new_urls)) {
    u <- new_urls[i]
    cat(sprintf("[bonbanh-rt] %d/%d: %s\n", i, n_new, u))

    raw_row <- tryCatch(scrape_detail_bonbanh(u), error = function(e) {
      log_message(SCRIPT_NAME, sprintf("Lỗi cào %s: %s", u, e$message), "WARN")
      NULL
    })
    if (is.null(raw_row)) next

    clean_row <- tryCatch(
      standardize_car_data(raw_row) %>% apply_business_rules(),
      error = function(e) NULL)
    if (is.null(clean_row) || nrow(clean_row) == 0) next

    batch[[length(batch) + 1]] <- clean_row

    tryCatch({
      DBI::dbWriteTable(con_init, TABLE_NAME, clean_row, append = TRUE, row.names = FALSE)
      inserted_init <- inserted_init + 1L
    }, error = function(e) {
      log_message(SCRIPT_NAME, sprintf("init_db INSERT lỗi (%s): %s", u, e$message), "WARN")
    })

    tryCatch({
      DBI::dbWriteTable(con_master, TABLE_NAME, clean_row, append = TRUE, row.names = FALSE)
      inserted_master <- inserted_master + 1L
    }, error = function(e) {
      log_message(SCRIPT_NAME, sprintf("master INSERT lỗi (%s): %s", u, e$message), "WARN")
    })

    Sys.sleep(runif(1, 0.8, 1.5))
  }

  # ── BƯỚC 4: Ghi ra rt.csv ─────────────────────────────────────────────────────
  if (length(batch) > 0) {
    rt_df <- bind_rows(batch)
    if (!file.exists(RT_OUTPUT)) {
      readr::write_csv(rt_df, RT_OUTPUT, na = "")
    } else {
      readr::write_csv(rt_df, RT_OUTPUT, na = "", append = TRUE, col_names = FALSE)
    }
    log_message(SCRIPT_NAME, sprintf("Đã ghi %d dòng vào: %s", nrow(rt_df), RT_OUTPUT))
  }

  log_message(SCRIPT_NAME, sprintf(
    "=== Hoàn thành. %d URL mới | %d vào init_db | %d vào master ===",
    n_new, inserted_init, inserted_master))
  return(inserted_init)
}
