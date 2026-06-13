# ==============================================================================
# Script: scrap_banxehoicu.R
# Purpose: Scrape raw data from banxehoicu.vn into canonical 18-column layout
# Output: web_scraping/data/raw/data_banxehoicu_raw.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

source("web_scraping/script/utils.R")

SCRIPT_NAME <- "web_scraping/script/scrap/scrap_banxehoicu.R"
BASE_URL <- "https://banxehoicu.vn"
LISTING_URL <- paste0(BASE_URL, "/ban-oto-cu")
OUTPUT_FILE <- "web_scraping/data/raw/data_banxehoicu_raw.csv"
URL_META_FILE <- "web_scraping/data/raw/meta/urls_banxehoicu.txt"
SOURCE_NAME <- "banxehoicu.vn"

safe_get <- function(url) {
  tryCatch({
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (compatible; vn-used-car-analysis/1.0)"),
      httr::timeout(30)
    )
    if (httr::http_error(resp)) return(NULL)
    rvest::read_html(resp, encoding = "UTF-8")
  }, error = function(e) {
    log_message(SCRIPT_NAME, sprintf("GET failed for %s: %s", url, e$message), "WARN")
    NULL
  })
}

make_absolute_url <- function(href) {
  href <- href[!is.na(href) & href != ""]
  ifelse(str_detect(href, "^https?://"), href, paste0(BASE_URL, ifelse(str_starts(href, "/"), href, paste0("/", href))))
}

get_listing_urls <- function(page_num = 1) {
  candidates <- unique(c(
    if (page_num <= 1) LISTING_URL else character(0),
    paste0(LISTING_URL, "/page/", page_num),
    paste0(LISTING_URL, "?page=", page_num),
    paste0(LISTING_URL, "?trang=", page_num)
  ))

  for (page_url in candidates) {
    page <- safe_get(page_url)
    if (is.null(page)) next

    links <- page %>%
      html_elements("a[href]") %>%
      html_attr("href") %>%
      make_absolute_url()

    links <- links[str_detect(links, "/ban-oto-cu/.+\\.html")]
    links <- unique(str_replace(links, "#.*$", ""))

    if (length(links)) return(links)
  }

  character(0)
}

text_or_na <- function(node) {
  val <- tryCatch(html_text2(node), error = function(e) NA_character_)
  val <- str_squish(val)
  ifelse(is.na(val) | val == "", NA_character_, val)
}

get_spec_by_label <- function(full_text, labels) {
  labels <- paste(labels, collapse = "|")
  pattern <- paste0("(?i)(", labels, ")\\s*[:：]?\\s*([^\\n\\r]+)")
  val <- str_match(full_text, pattern)[, 3]
  ifelse(is.na(val), NA_character_, str_squish(val))
}

extract_price <- function(full_text) {
  val <- str_extract(
    full_text,
    regex("[0-9]+[,.]?[0-9]*\\s*t[ỷy](\\s*[0-9]+[,.]?[0-9]*\\s*triệu)?|[0-9]+[,.]?[0-9]*\\s*triệu", ignore_case = TRUE)
  )
  ifelse(is.na(val), NA_character_, str_squish(val))
}

slug_to_name <- function(x) {
  x <- str_replace_all(x, "-", " ")
  str_to_title(x)
}

scrape_detail <- function(url) {
  page <- safe_get(url)
  if (is.null(page)) return(tibble())

  full_text <- text_or_na(page)
  title <- page %>%
    html_element("h1") %>%
    text_or_na()

  if (is.na(title)) {
    title <- page %>% html_element("title") %>% text_or_na()
  }

  url_parts <- str_match(url, "/ban-oto-cu/([^/]+)/([^/]+)/")
  brand <- ifelse(!is.na(url_parts[, 2]), slug_to_name(url_parts[, 2]), NA_character_)
  model <- ifelse(!is.na(url_parts[, 3]), slug_to_name(url_parts[, 3]), NA_character_)

  year <- str_extract(paste(title, full_text), "(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])")
  trim <- title %>%
    str_remove(regex("^\\s*(bán\\s+)?(xe\\s+)?(ô\\s*tô\\s+)?(cũ\\s+)?", ignore_case = TRUE)) %>%
    str_remove(regex(paste(brand, model, sep = ".*"), ignore_case = TRUE)) %>%
    str_remove(fixed(ifelse(is.na(year), "", year))) %>%
    str_squish()
  if (is.na(trim) || trim == "") trim <- NA_character_

  tibble(
    brand = brand,
    model = model,
    trim = trim,
    year = coalesce(get_spec_by_label(full_text, c("Đời xe", "Năm sản xuất", "Năm SX")), year),
    body_type = get_spec_by_label(full_text, c("Kiểu dáng", "Dòng xe")),
    fuel_type = get_spec_by_label(full_text, c("Nhiên liệu", "Động cơ")),
    transmission = get_spec_by_label(full_text, c("Hộp số")),
    engine_size = get_spec_by_label(full_text, c("Dung tích", "Động cơ")),
    seat_count = get_spec_by_label(full_text, c("Số chỗ", "Số chỗ ngồi")),
    drivetrain = get_spec_by_label(full_text, c("Dẫn động")),
    price = extract_price(full_text),
    mileage = get_spec_by_label(full_text, c("Số Km đã đi", "Số km đã đi", "Km đã đi", "ODO")),
    origin = get_spec_by_label(full_text, c("Xuất xứ")),
    color = get_spec_by_label(full_text, c("Màu xe", "Màu ngoại thất")),
    city = get_spec_by_label(full_text, c("Quận/Huyện", "Nơi bán", "Địa chỉ")),
    posted_date = get_spec_by_label(full_text, c("Ngày đăng", "Đăng ngày")),
    source = SOURCE_NAME,
    url = url
  ) %>%
    mutate(posted_date = ifelse(is.na(posted_date), "Hôm nay", posted_date)) %>%
    align_schema()
}

run_scrap_banxehoicu <- function(max_pages = as.integer(Sys.getenv("BANXEHOICU_PAGES", "5"))) {
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(URL_META_FILE), recursive = TRUE, showWarnings = FALSE)

  log_message(SCRIPT_NAME, sprintf("Starting BanXeHoiCu scraping for %d page(s).", max_pages))

  urls <- unique(unlist(lapply(seq_len(max_pages), get_listing_urls), use.names = FALSE))
  urls <- urls[!is.na(urls) & urls != ""]

  writeLines(urls, URL_META_FILE, useBytes = TRUE)
  log_message(SCRIPT_NAME, sprintf("Collected %d detail URLs.", length(urls)))

  if (!length(urls)) {
    safe_write_csv(align_schema(data.frame()), OUTPUT_FILE)
    return(invisible(align_schema(data.frame())))
  }

  rows <- lapply(seq_along(urls), function(i) {
    log_message(SCRIPT_NAME, sprintf("Scraping [%d/%d] %s", i, length(urls), urls[[i]]))
    row <- scrape_detail(urls[[i]])
    Sys.sleep(runif(1, 0.6, 1.4))
    row
  })

  raw_df <- bind_rows(rows) %>% align_schema()
  readr::write_csv(raw_df, OUTPUT_FILE, na = "NA", col_names = FALSE)
  log_message(SCRIPT_NAME, sprintf("Saved %d raw rows to %s", nrow(raw_df), OUTPUT_FILE))

  invisible(raw_df)
}

if (!exists(".is_realtime_sourcing", inherits = TRUE) || !isTRUE(.is_realtime_sourcing)) {
  run_scrap_banxehoicu()
}
