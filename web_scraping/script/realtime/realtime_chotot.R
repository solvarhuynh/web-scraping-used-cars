# ==============================================================================
# Script: realtime_chotot.R
# Purpose: Delta scrape xe.chotot.com — chỉ cào các URL chưa có trong DB
#
# Logic phát hiện URL mới:
#   - Cào trang 1 (20 URL), so ngược từ URL cuối lên URL đầu với DB
#   - Nếu URL cuối cùng của trang CHƯA có trong DB → cả trang chưa cào → sang trang 2
#   - Nếu URL cuối đã có → so từng URL từ cuối lên, dừng khi gặp URL đã có
#   - Ghi URLs mới vào urls_chotot.txt (prepend - mới nhất ở đầu file, không ghi đè)
#   - Cào chi tiết từng URL mới → data/realtime/data_chotot_rt.csv
#   - INSERT OR IGNORE vào init_db/data_chotot.db và master_data.db
#
# Output: web_scraping/data/realtime/data_chotot_rt.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(DBI)
  library(RSQLite)
  library(readr)
})

source("web_scraping/script/utils.R")

# Flag báo scrap_chotot.R chỉ load hàm/config, không chạy Step A+B
REALTIME_MODE <- TRUE
source("web_scraping/script/scrap/scrap_chotot.R")

SCRIPT_NAME   <- "realtime_chotot.R"
SOURCE_NAME   <- "xe.chotot.com"
TABLE_NAME    <- "car_listings"
INIT_DB_FILE  <- "web_scraping/data/init_db/data_chotot.db"
MASTER_DB     <- "web_scraping/data/master_data.db"
URLS_FILE     <- file.path(OUTPUT_DIR, "meta", "urls_chotot.txt")
RT_OUTPUT_DIR <- "web_scraping/data/realtime"
RT_OUTPUT     <- file.path(RT_OUTPUT_DIR, "data_chotot_rt.csv")
MAX_PAGES_RT  <- 10   # Giới hạn số trang kiểm tra trong 1 lần realtime

# ── Lấy URLs từ 1 trang listing ───────────────────────────────────────────────
fetch_listing_page_urls <- function(sess, page_num) {
  url <- if (page_num == 1) LISTING_URL else
    paste0("https://xe.chotot.com/mua-ban-oto?page=", page_num)

  nav <- safe_navigate(sess, url)
  if (!nav$ok) {
    log_message(SCRIPT_NAME, sprintf("Không navigate được trang %d", page_num), "WARN")
    return(character(0))
  }
  sess <- nav$session

  for (i in seq_len(5)) {
    tryCatch(sess$Runtime$evaluate('window.scrollBy(0, window.innerHeight)'), error = function(e) NULL)
    Sys.sleep(1)
  }

  html_raw <- tryCatch(
    sess$Runtime$evaluate('document.documentElement.outerHTML')$result$value,
    error = function(e) NULL)
  if (is.null(html_raw)) return(character(0))

  pg <- tryCatch(read_html(html_raw, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(pg)) return(character(0))

  links <- pg |> html_nodes("a.c15fd2pn") |> html_attr("href") |> na.omit()
  links <- links[str_detect(links, "\\/\\d+\\.htm")]
  links <- str_replace(links, "#.*$", "")
  unique(paste0(BASE_URL, links))
}

# ── Kiểm tra URL có trong DB chưa ────────────────────────────────────────────
url_in_db <- function(con, url) {
  res <- DBI::dbGetQuery(con,
    sprintf("SELECT 1 FROM %s WHERE url = ? LIMIT 1", TABLE_NAME),
    params = list(url))
  nrow(res) > 0
}

# ── Main realtime function ────────────────────────────────────────────────────
run_realtime_chotot <- function(con_master = NULL) {
  log_message(SCRIPT_NAME, "=== Bắt đầu realtime Chợ Tốt ===")
  dir.create(RT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  # Kết nối DB
  owns_master <- is.null(con_master)
  if (owns_master) {
    con_master <- DBI::dbConnect(RSQLite::SQLite(), MASTER_DB)
    on.exit(DBI::dbDisconnect(con_master), add = TRUE)
  }
  # Init DB riêng của chotot
  if (!file.exists(INIT_DB_FILE)) {
    log_message(SCRIPT_NAME, paste("Không tìm thấy init DB:", INIT_DB_FILE), "ERROR")
    return(0L)
  }
  con_init <- DBI::dbConnect(RSQLite::SQLite(), INIT_DB_FILE)
  on.exit(DBI::dbDisconnect(con_init), add = TRUE)

  # Khởi session Chromote
  sess <- make_session()
  on.exit({ close_session(sess); log_message(SCRIPT_NAME, "Đã đóng session.") }, add = TRUE)

  # ── BƯỚC 1: Phát hiện URLs mới ──────────────────────────────────────────────
  new_urls <- character(0)

  for (pg_num in seq_len(MAX_PAGES_RT)) {
    log_message(SCRIPT_NAME, sprintf("Kiểm tra trang %d...", pg_num))
    page_urls <- fetch_listing_page_urls(sess, pg_num)

    if (length(page_urls) == 0) {
      log_message(SCRIPT_NAME, sprintf("Trang %d không có URL, dừng.", pg_num), "WARN")
      break
    }

    # Kiểm tra URL CUỐI của trang (URL cũ nhất trong trang này)
    last_url <- page_urls[length(page_urls)]
    last_in_db <- url_in_db(con_init, last_url)

    if (!last_in_db) {
      # Cả trang chưa cào → lấy tất cả, sang trang tiếp
      log_message(SCRIPT_NAME, sprintf("Trang %d: URL cuối chưa có trong DB → lấy hết %d URL, sang trang tiếp.", pg_num, length(page_urls)))
      new_urls <- c(new_urls, page_urls)
      next
    }

    # URL cuối đã có → so ngược từ cuối lên để tìm ranh giới
    log_message(SCRIPT_NAME, sprintf("Trang %d: URL cuối đã có trong DB → so ngược từng URL.", pg_num))
    for (i in rev(seq_along(page_urls))) {
      if (!url_in_db(con_init, page_urls[i])) {
        new_urls <- c(new_urls, page_urls[i])
      } else {
        break  # Gặp URL đã cào → dừng (những URL trước đó cũng đã cào rồi)
      }
    }
    break  # Đã xác định ranh giới, không cần kiểm tra trang tiếp
  }

  n_new <- length(new_urls)
  log_message(SCRIPT_NAME, sprintf("Phát hiện %d URL mới cần cào.", n_new))

  if (n_new == 0) {
    log_message(SCRIPT_NAME, "Không có URL mới. Kết thúc.")
    return(0L)
  }

  # ── BƯỚC 2: Ghi URLs mới vào urls_chotot.txt (prepend - mới ở đầu) ──────────
  # Đảo ngược new_urls để URL mới nhất (trang 1, vị trí 1) lên đầu file
  urls_to_write <- rev(new_urls)
  dir.create(dirname(URLS_FILE), recursive = TRUE, showWarnings = FALSE)
  existing_content <- if (file.exists(URLS_FILE)) readLines(URLS_FILE, warn = FALSE) else character(0)
  existing_content <- existing_content[nchar(trimws(existing_content)) > 0]
  writeLines(c(urls_to_write, existing_content), URLS_FILE)
  log_message(SCRIPT_NAME, sprintf("Đã ghi %d URL mới vào đầu file: %s", n_new, URLS_FILE))

  # ── BƯỚC 3: Cào chi tiết từng URL mới ───────────────────────────────────────
  assign("b", sess, envir = .GlobalEnv)
  batch <- list()
  inserted_init <- 0L
  inserted_master <- 0L

  for (i in seq_along(new_urls)) {
    u <- new_urls[i]
    cat(sprintf("[chotot-rt] %d/%d: %s\n", i, n_new, u))

    raw_row <- tryCatch(scrape_car(u), error = function(e) {
      log_message(SCRIPT_NAME, sprintf("Lỗi cào %s: %s", u, e$message), "WARN")
      NULL
    })

    if (is.null(raw_row) || nrow(raw_row) == 0) next

    clean_row <- tryCatch(
      standardize_car_data(raw_row) %>% apply_business_rules(),
      error = function(e) NULL)
    if (is.null(clean_row) || nrow(clean_row) == 0) next

    batch[[length(batch) + 1]] <- clean_row

    # INSERT vào init_db
    tryCatch({
      DBI::dbWriteTable(con_init, TABLE_NAME, clean_row, append = TRUE, row.names = FALSE)
      inserted_init <- inserted_init + 1L
    }, error = function(e) {
      log_message(SCRIPT_NAME, sprintf("init_db INSERT lỗi (%s): %s", u, e$message), "WARN")
    })

    # INSERT vào master_data.db
    tryCatch({
      DBI::dbWriteTable(con_master, TABLE_NAME, clean_row, append = TRUE, row.names = FALSE)
      inserted_master <- inserted_master + 1L
    }, error = function(e) {
      log_message(SCRIPT_NAME, sprintf("master INSERT lỗi (%s): %s", u, e$message), "WARN")
    })

    Sys.sleep(runif(1, 1, 2))
  }

  # ── BƯỚC 4: Ghi ra rt.csv ────────────────────────────────────────────────────
  if (length(batch) > 0) {
    rt_df <- bind_rows(batch)
    # Append vào rt.csv (tạo mới nếu chưa có, thêm header chỉ lần đầu)
    if (!file.exists(RT_OUTPUT)) {
      readr::write_csv(rt_df, RT_OUTPUT, na = "")
    } else {
      readr::write_csv(rt_df, RT_OUTPUT, na = "", append = TRUE, col_names = FALSE)
    }
    log_message(SCRIPT_NAME, sprintf("Đã ghi %d dòng vào: %s", nrow(rt_df), RT_OUTPUT))
  }

  log_message(SCRIPT_NAME, sprintf(
    "=== Hoàn thành. %d URL mới | %d dòng vào init_db | %d dòng vào master ===",
    n_new, inserted_init, inserted_master))
  return(inserted_init)
}

run_realtime_chotot()