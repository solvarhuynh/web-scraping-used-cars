# script/realtime/realtime_banxehoicu.R ----------------------------------------
# Purpose : Delta-fetch NEW listings from banxehoicu.vn and INSERT into SQLite.
# Method  : rvest + httr (static HTML – mirrors scrap_banxehoicu.R).
# Rule ref: rule/realtime_rule.md
#
# Usage (called by Orchestrator):
#   source("script/realtime/realtime_banxehoicu.R")
#   n_new <- run_realtime_banxehoicu(con)   # con = DBI SQLite connection
# -------------------------------------------------------------------------------
# Purpose: Fetch new listings from banxehoicu.vn and insert into SQLite.
# Called by: run_realtime.R

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(DBI)
})

# ── 1. Source batch scraper để tái sử dụng helpers ───────────────────────────
# Điều này kéo vào:  safe_get(), get_listing_urls(), scrape_detail(),
#                    get_spec_by_label(), get_price(), get_city(), clean_str()
# Lưu ý: file batch có 1 đoạn code top-level (main loop + log startup) sẽ chạy
# khi source(). Để tránh side-effect, chúng ta wrap source() trong
# local() — các hàm vẫn được export ra global env thông qua <<- hoặc
# chúng ta chỉ cần accept rằng batch sẽ in vài dòng log khi được source.
# Cách sạch nhất: batch script tự kiểm tra biến môi trường SOURCED_AS_LIB.
# Source helpers from shared utils and the batch scraper
source("web_scraping/script/utils.R")

# Đặt flag trước khi source để batch script biết nó đang được source, không chạy main loop.
.rt_bxhc_sourcing <- TRUE
# Set a flag to prevent the batch script's main loop from running when sourced
.is_realtime_sourcing <- TRUE
source("web_scraping/script/scrap/scrap_banxehoicu.R")
rm(.rt_bxhc_sourcing)
rm(.is_realtime_sourcing)

# ── 2. Constants ─────────────────────────────────────────────────────────────
RT_SCRIPT_NAME   <- "realtime_banxehoicu.R"
RT_TABLE         <- "car_listings"
RT_LOG_FILE      <- LOG_FILE
RT_SLEEP_DETAIL  <- c(0.8, 1.8)   # giây giữa mỗi lần cào trang chi tiết
# --- Constants ---
SCRIPT_NAME <- "realtime_banxehoicu.R"
DB_TABLE    <- "car_listings"
SLEEP_SEC   <- c(0.8, 1.8) # Pause between scraping new items

# ── 3. Logging (dùng riêng để không xung đột với .log của batch) ─────────────
.rt_log_bxhc <- function(level, msg) {
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] - %s: %s\n", ts, RT_SCRIPT_NAME, toupper(level), msg)
  cat(line)
  cat(line, file = RT_LOG_FILE, append = TRUE)
}
rt_info_bxhc  <- function(m) .rt_log_bxhc("INFO",  m)
rt_warn_bxhc  <- function(m) .rt_log_bxhc("WARN",  m)
rt_error_bxhc <- function(m) .rt_log_bxhc("ERROR", m)

# ── 4. Hàm chính ─────────────────────────────────────────────────────────────
#' @param con  DBI connection đến SQLite (truyền vào từ Orchestrator).
#' @return     Số lượng bản ghi mới đã INSERT (integer).

#' Fetches new listings from Page 1 of banxehoicu.vn.
#'
#' @param con A DBI connection object from the orchestrator.
#' @return The number of new records inserted (integer).
run_realtime_banxehoicu <- function(con) {
  rt_info_bxhc("=== Delta-fetch started ===")

  # ---- 1. Get URLs from page 1 using batch helper -------------------------
  page1_urls <- tryCatch(
    get_listing_urls(page_num = 1),
    error = function(e) {
      rt_error_bxhc(sprintf("get_listing_urls() failed: %s", e$message))
      character(0)
    })

  if (length(page1_urls) == 0) {
    rt_warn_bxhc("No listing URLs found on Page 1 – aborting.")
    return(0L)
  }

  rt_info_bxhc(sprintf("Page 1: %d URLs – beginning delta‑fetch loop...", length(page1_urls)))

  inserted_count <- 0L

  for (i in seq_along(page1_urls)) {
    url <- page1_urls[i]

    # ---- A. Check if URL already exists in SQLite ------------------------
    exists_result <- tryCatch(
      DBI::dbGetQuery(con,
        sprintf("SELECT 1 FROM %s WHERE url = ?", RT_TABLE),
        params = list(url)),
      error = function(e) {
        rt_warn_bxhc(sprintf("DB lookup failed for %s: %s", url, e$message))
        NULL
      })
    if (is.null(exists_result)) next  # skip on DB error
    if (nrow(exists_result) > 0) {
      rt_info_bxhc(sprintf("Duplicate at position %d/%d (url: %s) – stopping loop.",
                         i, length(page1_urls), url))
      break
    }

    # ---- B. New URL – scrape detail --------------------------------------
    rt_info_bxhc(sprintf("[%d/%d] NEW – scraping %s", i, length(page1_urls), url))
    detail_row <- tryCatch(
      scrape_detail(url),
      error = function(e) {
        rt_warn_bxhc(sprintf("scrape_detail() error for %s: %s", url, e$message))
        NULL
      })
    if (is.null(detail_row) || nrow(detail_row) == 0) {
      rt_warn_bxhc(sprintf("Empty result for %s – skipping insert.", url))
      next
    }
    detail_row <- standardize_car_data(detail_row) %>% apply_business_rules()
    if (nrow(detail_row) == 0) {
      rt_warn_bxhc(sprintf("Record failed clean business rules for %s – skipping insert.", url))
      next
    }

    # ---- C. Insert into SQLite ------------------------------------------
    insert_ok <- tryCatch({
      DBI::dbWriteTable(con, RT_TABLE, detail_row, append = TRUE, row.names = FALSE)
      TRUE
    }, error = function(e) {
      rt_warn_bxhc(sprintf("INSERT failed for %s: %s", url, e$message))
      FALSE
    })
    if (insert_ok) {
      inserted_count <- inserted_count + 1L
      rt_info_bxhc(sprintf("Inserted %d (url: %s)", inserted_count, url))
    }

    # ---- optional short pause between detail pages -----------------------
    Sys.sleep(runif(1, RT_SLEEP_DETAIL[1], RT_SLEEP_DETAIL[2]))
  }

  rt_info_bxhc(sprintf("=== Delta-fetch finished – %d new record(s) inserted. ===",
                       inserted_count))
  inserted_count
}
