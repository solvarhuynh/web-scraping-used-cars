# script/scrap/scrap_banxehoicu.R -----------------------------------------------
# Purpose : Scrape used-car listings from https://banxehoicu.vn/ban-oto-cu
# Method  : rvest + httr (static HTML – no JS rendering needed)
# Output  : data/raw/data_banxehoicu_raw.csv
# Rule ref: scrap_rule.md
# -------------------------------------------------------------------------------

# ── 1. Packages ─────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
  library(cli)
})

# ── 2. Logging ──────────────────────────────────────────────────────────────────
log_file <- file.path(getwd(), "log.txt")
.log <- function(level, msg) {
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [scrap_banxehoicu.R] - %s: %s\n", ts, toupper(level), msg)
  cat(line)
  cat(line, file = log_file, append = TRUE)
}
log_info  <- function(m) .log("INFO",  m)
log_warn  <- function(m) .log("WARN",  m)
log_error <- function(m) .log("ERROR", m)

log_info("=== Bán Xe Hơi Cũ scraper started ===")

# ── 3. Constants ─────────────────────────────────────────────────────────────────
BASE_URL        <- "https://banxehoicu.vn/ban-oto-cu"
BASE_DOMAIN     <- "https://banxehoicu.vn"
OUTPUT_CSV      <- file.path("data", "raw", "data_banxehoicu_raw.csv")
CHECKPOINT_FILE <- file.path("data", "raw", "checkpoint_banxehoicu.txt")
SOURCE_DOMAIN   <- "banxehoicu.vn"
MAX_PAGES       <- 300
SLEEP_EVERY_N   <- 20      # pause after every N pages
SLEEP_PAGES_SEC <- c(2, 5)   # seconds to sleep between page batches
SLEEP_DETAIL_SEC <- c(0.5, 1)   # seconds between individual detail pages

CANONICAL_COLS <- c("brand", "model", "trim", "year", "body_type", "fuel_type",
                    "transmission", "engine_size", "seat_count", "drivetrain",
                    "price", "mileage", "origin", "color", "city",
                    "posted_date", "source", "url")

dir.create(dirname(OUTPUT_CSV),      recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(CHECKPOINT_FILE), recursive = TRUE, showWarnings = FALSE)

# ── 4. Helpers ───────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) {
  if (length(a) == 0 || (length(a) == 1 && (is.na(a) || trimws(a) == ""))) b else a
}

# Chuẩn hoá chuỗi tiếng Việt: bỏ icon/ký tự lạ, giữ UTF-8
clean_str <- function(x) {
  if (is.na(x) || length(x) == 0) return(NA_character_)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")
  x <- str_squish(x)
  # Bỏ ký tự không phải chữ/số/khoảng trắng/dấu tiếng Việt phổ biến/dấu câu
  x <- gsub("[^\u0020-\u007E\u00C0-\u024F\u1E00-\u1EFF]", " ", x)
  x <- str_squish(x)
  if (nchar(x) == 0) NA_character_ else x
}

# GET với full browser headers để tránh block
safe_get <- function(url, retries = 3) {
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      resp <- GET(
        url,
        add_headers(
          `User-Agent`               = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
          `Accept`                   = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
          `Accept-Language`          = "vi-VN,vi;q=0.9,en-US;q=0.8,en;q=0.7",
          `Accept-Encoding`          = "gzip, deflate, br",
          `Connection`               = "keep-alive",
          `Upgrade-Insecure-Requests`= "1",
          `Cache-Control`            = "max-age=0",
          `Referer`                  = BASE_DOMAIN
        ),
        timeout(30)
      )
      if (status_code(resp) == 200) {
        html_text_content <- content(resp, as = "text", encoding = "UTF-8")
        list(ok = TRUE, doc = read_html(html_text_content))
      } else {
        log_warn(sprintf("HTTP %d for %s (attempt %d)", status_code(resp), url, attempt))
        list(ok = FALSE, doc = NULL)
      }
    }, error = function(e) {
      log_warn(sprintf("GET error attempt %d for %s: %s", attempt, url, e$message))
      list(ok = FALSE, doc = NULL)
    })
    if (result$ok) return(result$doc)
    Sys.sleep(runif(1, 2, 4) * attempt)
  }
  NULL
}

# ── 5. Checkpoint: xác định trang bắt đầu ────────────────────────────────────────
# Đọc last_page đã cào từ file CSV kết quả (nếu tồn tại)
start_page <- 1L
if (file.exists(OUTPUT_CSV)) {
  existing <- tryCatch(read_csv(OUTPUT_CSV, col_types = cols(.default = "c"), show_col_types = FALSE),
                       error = function(e) NULL)
  if (!is.null(existing) && "url" %in% names(existing) && nrow(existing) > 0) {
    # Lấy số trang lớn nhất đã lưu từ checkpoint file
    if (file.exists(CHECKPOINT_FILE)) {
      last_page_saved <- suppressWarnings(as.integer(readLines(CHECKPOINT_FILE, warn = FALSE)[1]))
      if (!is.na(last_page_saved) && last_page_saved >= 1) {
        start_page <- last_page_saved + 1L
        log_info(sprintf("Resuming from page %d (checkpoint: page %d already done)", start_page, last_page_saved))
      }
    }
  }
} else {
  # Tạo file CSV rỗng với header để có thể append sau này
  empty_df <- as_tibble(setNames(
    lapply(CANONICAL_COLS, function(x) character(0)),
    CANONICAL_COLS
  ))
  write_csv(empty_df, OUTPUT_CSV, na = "NA")
  log_info("Created fresh output CSV.")
}

if (start_page > MAX_PAGES) {
  log_info("All pages already scraped. Exiting.")
  quit(save = "no")
}

# ── 6. Lấy URLs xe từ 1 trang listing ────────────────────────────────────────────
# Dựa trên HTML ảnh 1:
#   div.list-car > div.items > div.item > div.thumbnail > a[href]
# href có dạng: /ban-oto-cu/.../...html

get_listing_urls <- function(page_num) {
  pg_url <- if (page_num == 1) BASE_URL else sprintf("%s?page=%d", BASE_URL, page_num)
  doc <- safe_get(pg_url)
  if (is.null(doc)) return(character(0))

  # Selector chính xác từ ảnh 1
  links <- tryCatch(
    doc %>%
      html_nodes("div.list-car div.items div.item div.thumbnail a") %>%
      html_attr("href"),
    error = function(e) character(0)
  )

  # Fallback: tất cả <a> trong div.item
  if (length(links) == 0 || all(is.na(links))) {
    links <- tryCatch(
      doc %>%
        html_nodes("div.item div.thumbnail a, div.items div.item a") %>%
        html_attr("href"),
      error = function(e) character(0)
    )
  }

  # Fallback cuối: mọi link có pattern /ban-oto-cu/.../...html
  if (length(links) == 0 || all(is.na(links))) {
    links <- tryCatch(
      doc %>%
        html_nodes(paste0("a[href*='/ban-oto-cu/']")) %>%
        html_attr("href"),
      error = function(e) character(0)
    )
  }

  links <- links[!is.na(links) & nchar(links) > 0]
  # Chỉ giữ link chi tiết (không phải trang danh sách)
  links <- links[grepl("^/ban-oto-cu/", links) & grepl("\\.html$", links)]
  # Prefix domain
  links <- paste0(BASE_DOMAIN, links)
  unique(links)
}

# ── 7. Lấy thông tin chi tiết 1 xe ───────────────────────────────────────────────
# Dựa trên HTML ảnh 2 & 3:
#   div.box1 > div.basic-info > div.item > span.label + span.value
# Tên label (span.label): Ngày đăng, Hãng xe, Loại xe, Phiên bản, Đời xe,
#                         Hộp số, Nhiên liệu, Màu xe, Xuất xứ, Tình trạng,
#                         Số km đã đi, Giá bán (trong div.item.price)
#                         Kiểu dáng, Hệ dẫn động, Dung tích, Số chỗ ngồi, ...

get_spec_by_label <- function(doc, label) {
  # Tìm div.item chứa span.label có text = label, rồi lấy span.value liền kề
  val <- tryCatch({
    xpath <- sprintf(
      "//div[contains(@class,'basic-info')]//div[contains(@class,'item')][.//span[@class='label' and normalize-space(text())='%s']]//span[contains(@class,'value')]",
      label
    )
    nodes <- html_nodes(doc, xpath = xpath)
    if (length(nodes) == 0) {
      # Thử contains thay vì exact match
      xpath2 <- sprintf(
        "//div[contains(@class,'basic-info')]//div[contains(@class,'item')][.//span[contains(@class,'label') and contains(text(),'%s')]]//span[contains(@class,'value')]",
        label
      )
      nodes <- html_nodes(doc, xpath = xpath2)
    }
    if (length(nodes) > 0) clean_str(html_text(nodes[[1]], trim = TRUE)) else NA_character_
  }, error = function(e) NA_character_)
  val
}

# Giá bán nằm trong div.item.price – dùng selector riêng
get_price <- function(doc) {
  val <- tryCatch({
    nodes <- html_nodes(doc, "div.item.price span.value, div.item.price .value")
    if (length(nodes) > 0) clean_str(html_text(nodes[[1]], trim = TRUE)) else NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(val) && nchar(val) > 0) return(val)
  # Fallback: tìm label "Giá bán"
  get_spec_by_label(doc, "Giá bán")
}

# Lấy city từ phần địa điểm (có thể ở header/breadcrumb/sidebar)
get_city <- function(doc) {
  # Thử các vị trí thường gặp
  for (sel in c(".location", ".address", ".city", "[class*='location']",
                ".info-address", ".car-location", ".post-location")) {
    v <- tryCatch(clean_str(html_text(html_node(doc, sel), trim = TRUE)),
                  error = function(e) NA_character_)
    if (!is.na(v) && nchar(v) > 0) return(v)
  }
  # Thử spec label
  v <- get_spec_by_label(doc, "Tỉnh/Thành phố")
  if (!is.na(v)) return(v)
  v <- get_spec_by_label(doc, "Địa điểm")
  if (!is.na(v)) return(v)
  NA_character_
}

scrape_detail <- function(url) {
  doc <- safe_get(url)
  if (is.null(doc)) {
    return(tibble(!!!setNames(
      c(as.list(rep(NA_character_, length(CANONICAL_COLS) - 2)),
        list(SOURCE_DOMAIN), list(url)),
      CANONICAL_COLS
    )))
  }

  # -- Các field lấy trực tiếp từ span.label / span.value trong div.basic-info --

  # posted_date: label "Ngày đăng" -> thường là "Hôm nay", "Hôm qua", "DD/MM/YYYY"
  posted_raw  <- get_spec_by_label(doc, "Ngày đăng") %||% NA_character_

  brand       <- get_spec_by_label(doc, "Hãng xe")   %||% NA_character_
  model       <- get_spec_by_label(doc, "Loại xe")   %||% NA_character_  # "Loại xe" = model name
  trim        <- get_spec_by_label(doc, "Phiên bản") %||% NA_character_
  year_raw    <- get_spec_by_label(doc, "Đời xe")    %||% NA_character_
  year        <- year_raw

  transmission <- get_spec_by_label(doc, "Hộp số")       %||% NA_character_
  fuel_type    <- get_spec_by_label(doc, "Nhiên liệu")    %||% NA_character_
  color        <- get_spec_by_label(doc, "Màu xe")        %||% NA_character_
  origin       <- get_spec_by_label(doc, "Xuất xứ")      %||% NA_character_
  mileage      <- get_spec_by_label(doc, "Số km đã đi")  %||% NA_character_
  body_type    <- get_spec_by_label(doc, "Kiểu dáng")    %||% NA_character_
  drivetrain   <- get_spec_by_label(doc, "Hệ dẫn động")  %||% NA_character_
  engine_size  <- get_spec_by_label(doc, "Dung tích")    %||% NA_character_
  seat_count   <- get_spec_by_label(doc, "Số chỗ ngồi") %||% NA_character_

  price <- get_price(doc)
  city  <- get_city(doc)

  tibble(
    brand        = brand,
    model        = model,
    trim         = trim,
    year         = year,
    body_type    = body_type,
    fuel_type    = fuel_type,
    transmission = transmission,
    engine_size  = engine_size,
    seat_count   = seat_count,
    drivetrain   = drivetrain,
    price        = price,
    mileage      = mileage,
    origin       = origin,
    color        = color,
    city         = city,
    posted_date  = posted_raw,   # giữ nguyên chuỗi gốc, để file clean xử lý
    source       = SOURCE_DOMAIN,
    url          = url
  )
}

# ── 8. Main loop: duyệt từng trang, từng xe ──────────────────────────────────────
total_scraped <- 0L

log_info(sprintf("Starting main loop from page %d to %d", start_page, MAX_PAGES))

for (pg in seq(start_page, MAX_PAGES)) {

  # Lấy danh sách URLs của trang này
  page_urls <- get_listing_urls(pg)

  if (length(page_urls) == 0) {
    log_info(sprintf("Page %d returned 0 URLs – assuming last page, stopping.", pg))
    # Xoá checkpoint vì đã cào xong
    if (file.exists(CHECKPOINT_FILE)) file.remove(CHECKPOINT_FILE)
    break
  }

  log_info(sprintf("Page %d: %d listing URLs found.", pg, length(page_urls)))

  # Progress bar cho các xe trong trang này
  pb_page <- cli_progress_bar(
    total  = length(page_urls),
    format = paste0("[banxehoicu] Page ", pg, " | {pb_percent} | {pb_current}/{pb_total} | ETA: {pb_eta}"),
    clear  = FALSE
  )

  page_records <- vector("list", length(page_urls))

  for (j in seq_along(page_urls)) {
    cli_progress_update(id = pb_page, set = j)
    page_records[[j]] <- scrape_detail(page_urls[[j]])
    Sys.sleep(runif(1, SLEEP_DETAIL_SEC[1], SLEEP_DETAIL_SEC[2]))
  }

  cli_progress_done(pb_page)

  # Append vào CSV ngay sau mỗi trang (an toàn khi bị interrupt)
  page_df <- bind_rows(page_records) %>%
    select(all_of(CANONICAL_COLS))

  write_csv(page_df, OUTPUT_CSV, append = TRUE, na = "NA", col_names = FALSE)
  total_scraped <- total_scraped + nrow(page_df)

  # Ghi checkpoint
  writeLines(as.character(pg), CHECKPOINT_FILE)
  log_info(sprintf("Page %d done: +%d records | Total so far: %d", pg, nrow(page_df), total_scraped))

  # Ngủ sau mỗi SLEEP_EVERY_N trang
  if (pg %% SLEEP_EVERY_N == 0) {
    sleep_secs <- runif(1, SLEEP_PAGES_SEC[1], SLEEP_PAGES_SEC[2])
    log_info(sprintf("Batch pause: sleeping %.1f seconds after page %d...", sleep_secs, pg))
    Sys.sleep(sleep_secs)
  }
}

log_info(sprintf("=== Scraper finished. Total records written: %d | Output: %s ===",
                 total_scraped, OUTPUT_CSV))
