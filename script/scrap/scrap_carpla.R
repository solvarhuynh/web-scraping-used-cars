# script/scrap/scrap_carpla.R --------------------------------------------------
# Purpose : Scrape used-car listings from https://carpla.vn/mua-xe
# Method  : chromote (headless Edge/Chrome) – JS-rendered site
# Output  : data/raw/data_carpla_raw.csv
# Rule ref: scrap_rule.md
# ------------------------------------------------------------------------------

# ── 1. Packages ────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(cli)
})

# ── 2. Thiết lập trình duyệt (ưu tiên Edge) ───────────────────────────────────
if (Sys.getenv("CHROMOTE_CHROME") == "") {
  candidates <- c(
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    Sys.setenv(CHROMOTE_CHROME = found[1])
    message("[carpla] Browser found: ", found[1])
  } else {
    stop("[carpla] Không tìm thấy trình duyệt. Set CHROMOTE_CHROME thủ công.")
  }
}

# ── 3. Logging ─────────────────────────────────────────────────────────────────
LOG_FILE    <- file.path(getwd(), "log.txt")
SCRIPT_NAME <- "scrap_carpla.R"

.log <- function(level, msg) {
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] - %s: %s\n", ts, SCRIPT_NAME, toupper(level), msg)
  cat(line)
  cat(line, file = LOG_FILE, append = TRUE)
}
log_info  <- function(m) .log("INFO",  m)
log_warn  <- function(m) .log("WARN",  m)
log_error <- function(m) .log("ERROR", m)

# ── 4. Hằng số ─────────────────────────────────────────────────────────────────
BASE_DOMAIN      <- "https://carpla.vn"
LISTING_URL      <- "https://carpla.vn/mua-xe"
OUTPUT_CSV       <- file.path("data", "raw", "data_carpla_raw.csv")
CHECKPOINT_FILE  <- file.path("data", "raw", "checkpoint_carpla.txt")
SOURCE_DOMAIN    <- "carpla.vn"
PAGE_TIMEOUT     <- 25000   # ms
MAX_PAGES        <- 300     # giới hạn tối đa mỗi session
SLEEP_INTERVAL   <- 20       # số trang giữa các lần nghỉ
SLEEP_DURATION   <- c(2, 5) # khoảng nghỉ (giây), lấy ngẫu nhiên

CANONICAL_COLS <- c("brand", "model", "trim", "year", "body_type", "fuel_type",
                    "transmission", "engine_size", "seat_count", "drivetrain",
                    "price", "mileage", "origin", "color", "city",
                    "posted_date", "source", "url")

dir.create(dirname(OUTPUT_CSV), recursive = TRUE, showWarnings = FALSE)
log_info("=== Carpla scraper started ===")

# ── 5. Helpers ──────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

clean_str <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  x <- str_squish(str_trim(x))
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_squish(x)
  if (x == "" || x == "-") NA_character_ else x
}

# Parse HTML từ session chromote
get_page_html <- function(session) {
  res <- tryCatch(
    session$Runtime$evaluate("document.documentElement.outerHTML"),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  html_txt <- res$result$value
  if (is.null(html_txt) || is.na(html_txt) || html_txt == "") return(NULL)
  tryCatch(read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
}

# Scroll xuống cuối trang
scroll_page <- function(session, times = 6, delay = 1.5) {
  for (i in seq_len(times)) {
    tryCatch(
      session$Runtime$evaluate("window.scrollBy(0, window.innerHeight);"),
      error = function(e) NULL
    )
    Sys.sleep(delay)
  }
}

# ── Hàm checkpoint ─────────────────────────────────────────────────────────────
save_checkpoint <- function(page_num) {
  writeLines(as.character(page_num), CHECKPOINT_FILE)
}

read_checkpoint <- function() {
  if (!file.exists(CHECKPOINT_FILE)) return(0L)
  val <- suppressWarnings(as.integer(readLines(CHECKPOINT_FILE, warn = FALSE)[1]))
  if (is.na(val)) 0L else val
}

# ── Hàm đọc trang cuối đã lưu trong CSV ────────────────────────────────────────
# Carpla không lưu page_num trong CSV nên dùng checkpoint file là chính
get_last_scraped_url_count <- function() {
  if (!file.exists(OUTPUT_CSV)) return(0L)
  tryCatch({
    df <- read_csv(OUTPUT_CSV, show_col_types = FALSE, n_max = Inf)
    nrow(df)
  }, error = function(e) 0L)
}

# ── Hàm lấy giá trị từ cấu trúc label-value của Carpla ───────────────────────
get_spec_by_label <- function(doc, label_patterns) {
  for (pat in label_patterns) {
    val <- tryCatch({
      xpath_lbl <- paste0(
        "//div[not(@class) or not(contains(@class,'font-semibold'))][normalize-space(text())='",
        pat, "']"
      )
      lbls <- html_nodes(doc, xpath = xpath_lbl)

      if (length(lbls) == 0) {
        xpath_lbl2 <- paste0(
          "//div[contains(text(),'", pat, "') and not(contains(@class,'font-semibold'))]"
        )
        lbls <- html_nodes(doc, xpath = xpath_lbl2)
      }

      if (length(lbls) == 0) return(NA_character_)

      for (lbl_node in lbls) {
        val_node <- html_node(lbl_node,
          xpath = "following-sibling::div[contains(@class,'font-semibold')][1]")
        if (!is.null(val_node)) {
          txt <- html_text(val_node, trim = TRUE)
          if (!is.na(txt) && nchar(clean_str(txt) %||% "") > 0)
            return(clean_str(txt))
        }
        val_node2 <- html_node(lbl_node,
          xpath = "parent::div/div[contains(@class,'font-semibold')]")
        if (!is.null(val_node2)) {
          txt2 <- html_text(val_node2, trim = TRUE)
          if (!is.na(txt2) && nchar(clean_str(txt2) %||% "") > 0)
            return(clean_str(txt2))
        }
      }
      NA_character_
    }, error = function(e) NA_character_)

    if (!is.na(val)) return(val)
  }
  NA_character_
}

# Row rỗng khi lỗi
empty_row <- function(url) {
  tibble(!!!setNames(
    c(rep(list(NA_character_), length(CANONICAL_COLS) - 2),
      list(SOURCE_DOMAIN), list(url)),
    CANONICAL_COLS
  ))
}

# ── Hàm khởi tạo lại ChromoteSession (retry khi session die) ──────────────────
make_session <- function(max_tries = 3) {
  for (attempt in seq_len(max_tries)) {
    sess <- tryCatch({
      s <- ChromoteSession$new()
      Sys.sleep(2)
      s
    }, error = function(e) {
      log_warn(sprintf("Session launch attempt %d failed: %s", attempt, e$message))
      Sys.sleep(3)
      NULL
    })
    if (!is.null(sess)) return(sess)
  }
  stop("Cannot launch ChromoteSession after multiple attempts.")
}

close_session <- function(sess) {
  tryCatch(sess$close(), error = function(e) NULL)
}

# ── Hàm navigate an toàn (tự restart session nếu cần) ─────────────────────────
safe_navigate <- function(sess, url, timeout = PAGE_TIMEOUT) {
  result <- tryCatch({
    sess$Page$navigate(url, wait_ = FALSE)
    sess$Page$loadEventFired(timeout_ = timeout)
    Sys.sleep(3)
    list(ok = TRUE, session = sess)
  }, error = function(e) {
    log_warn(sprintf("Nav error '%s': %s — restarting session", url, e$message))
    close_session(sess)
    Sys.sleep(3)
    new_sess <- tryCatch(make_session(), error = function(e2) NULL)
    if (is.null(new_sess)) return(list(ok = FALSE, session = NULL))
    result2 <- tryCatch({
      new_sess$Page$navigate(url, wait_ = FALSE)
      new_sess$Page$loadEventFired(timeout_ = timeout)
      Sys.sleep(3)
      list(ok = TRUE, session = new_sess)
    }, error = function(e2) {
      log_warn(sprintf("Retry nav error '%s': %s", url, e2$message))
      list(ok = FALSE, session = new_sess)
    })
    result2
  })
  result
}

# ── 6. Khởi tạo Chromote ───────────────────────────────────────────────────────
log_info("Launching chromote session...")
b <- make_session()
log_info("Browser session ready.")

# ── 7. Đọc checkpoint & xác định start_page ────────────────────────────────────
last_checkpoint <- read_checkpoint()
start_page      <- last_checkpoint + 1L
log_info(sprintf("Checkpoint: last completed page = %d. Starting from page %d.",
                 last_checkpoint, start_page))

# ── 8. Thu thập tất cả URLs qua pagination ────────────────────────────────────
all_urls <- character(0)
page_num  <- start_page

# Nếu có CSV cũ, đọc URL đã scrape để tránh duplicate
existing_urls <- character(0)
if (file.exists(OUTPUT_CSV)) {
  existing_df   <- tryCatch(read_csv(OUTPUT_CSV, show_col_types = FALSE), error = function(e) NULL)
  existing_urls <- if (!is.null(existing_df) && "url" %in% names(existing_df))
                     existing_df$url else character(0)
  log_info(sprintf("Loaded %d existing URLs from output CSV.", length(existing_urls)))
}

log_info("=== STEP A+C: Collecting listing URLs ===")

pages_this_session <- 0L

repeat {
  # Giới hạn 500 trang mỗi session
  if (pages_this_session >= MAX_PAGES) {
    log_info(sprintf("Reached session page limit (%d). Stopping URL collection.", MAX_PAGES))
    break
  }

  pg_url <- if (page_num == 1) LISTING_URL else
    paste0(LISTING_URL, "?page=", page_num)

  log_info(sprintf("Navigating to page %d: %s", page_num, pg_url))

  nav_result <- safe_navigate(b, pg_url)
  b <- nav_result$session

  if (!nav_result$ok || is.null(b)) {
    log_warn(sprintf("Failed to navigate to page %d. Stopping URL collection.", page_num))
    break
  }

  # Scroll để trigger lazy-load
  scroll_page(b, times = 6, delay = 1.2)

  # Lấy HTML
  pg_html <- get_page_html(b)
  if (is.null(pg_html)) {
    log_warn(sprintf("Page %d: Could not get HTML", page_num))
    break
  }

  links <- pg_html |>
    html_nodes("a[href^='/xe/']") |>
    html_attr("href")

  links <- links[!is.na(links)]
  links <- links[str_detect(links, "^/xe/[a-z0-9-]+-[a-z0-9]+$")]
  links <- unique(links)

  if (length(links) == 0) {
    log_info(sprintf("Page %d: No listings found — stopping pagination", page_num))
    break
  }

  full_links <- paste0(BASE_DOMAIN, links)
  new_only   <- setdiff(full_links, c(all_urls, existing_urls))

  if (length(new_only) == 0) {
    log_info(sprintf("Page %d: No new URLs — stopping pagination", page_num))
    break
  }

  all_urls <- c(all_urls, new_only)
  log_info(sprintf("Page %d: +%d URLs | Total new: %d", page_num, length(new_only), length(all_urls)))

  # Lưu checkpoint sau mỗi trang thành công
  save_checkpoint(page_num)
  pages_this_session <- pages_this_session + 1L
  page_num <- page_num + 1L

  # Nghỉ mỗi SLEEP_INTERVAL trang
  if (pages_this_session %% SLEEP_INTERVAL == 0) {
    sleep_sec <- runif(1, SLEEP_DURATION[1], SLEEP_DURATION[2])
    log_info(sprintf("Pausing %.1f seconds after %d pages...", sleep_sec, pages_this_session))
    Sys.sleep(sleep_sec)
  } else {
    Sys.sleep(runif(1, 1.5, 2.5))
  }
}

log_info(sprintf("Total new listing URLs collected this session: %d", length(all_urls)))

if (length(all_urls) == 0) {
  log_warn("No new URLs found. Nothing to scrape.")
  close_session(b)
  stop("No new listing URLs found.")
}

# ── 9. Scrape từng trang chi tiết ──────────────────────────────────────────────
log_info("=== STEP B: Scraping detail pages ===")

total   <- length(all_urls)
results <- vector("list", total)

cli_progress_bar(
  name   = "Scraping carpla.vn",
  total  = total,
  format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
)

for (i in seq_len(total)) {
  u <- all_urls[i]

  nav_result <- safe_navigate(b, u)
  b <- nav_result$session

  if (!nav_result$ok || is.null(b)) {
    log_warn(sprintf("[%d/%d] Failed to navigate %s", i, total, u))
    results[[i]] <- empty_row(u)
    cli_progress_update()
    next
  }

  pg <- get_page_html(b)

  if (is.null(pg)) {
    results[[i]] <- empty_row(u)
    cli_progress_update()
    next
  }

  # ── Brand ─────────────────────────────────────────────────────────────────
  brand_raw <- tryCatch(
    clean_str(html_text(
      html_node(pg, "h1.text-secondary, h1[class*='text-secondary']"),
      trim = TRUE
    )),
    error = function(e) NA_character_
  )
  if (is.na(brand_raw)) {
    brand_raw <- tryCatch(
      clean_str(html_text(html_node(pg, "h1"), trim = TRUE)),
      error = function(e) NA_character_
    )
  }

  # ── Giá ───────────────────────────────────────────────────────────────────
  price_raw <- tryCatch(
    clean_str(html_text(
      html_node(pg, "div.font-semibold.text-\\[\\#D63E26\\], [class*='text-'][class*='font-semibold']"),
      trim = TRUE
    )),
    error = function(e) NA_character_
  )
  if (is.na(price_raw)) {
    price_raw <- tryCatch({
      nodes <- html_nodes(pg, xpath = "//div[contains(@class,'font-semibold') and contains(text(),'đ')]")
      if (length(nodes) > 0) clean_str(html_text(nodes[[1]], trim = TRUE)) else NA_character_
    }, error = function(e) NA_character_)
  }

  # ── Ngày đăng ─────────────────────────────────────────────────────────────
  posted_raw <- tryCatch({
    date_nodes <- html_nodes(pg, "div.text-secondary.text-sm, div[class*='text-secondary'][class*='text-sm']")
    found_date <- NA_character_
    for (dn in date_nodes) {
      txt <- html_text(dn, trim = TRUE)
      if (str_detect(txt %||% "", "trước|hôm nay|hôm qua|tháng|tuần|ngày|giờ|phút")) {
        parts <- str_split(txt, "\\s*Đăng\\s*|\\s*đăng\\s*")[[1]]
        parts <- str_trim(parts[parts != ""])
        if (length(parts) >= 1) {
          found_date <- clean_str(parts[length(parts)])
          break
        }
      }
    }
    found_date
  }, error = function(e) NA_character_)

  # ── City ──────────────────────────────────────────────────────────────────
  city_raw <- tryCatch({
    addr_nodes <- html_nodes(pg, "div.text-secondary.text-sm")
    found_city <- NA_character_
    for (an in addr_nodes) {
      txt <- html_text(an, trim = TRUE)
      if (!is.na(txt) && str_detect(txt, ",") &&
          !str_detect(txt, "trước|hôm nay|hôm qua|Đăng")) {
        found_city <- clean_str(txt)
        break
      }
    }
    found_city
  }, error = function(e) NA_character_)

  # ── Thông số kỹ thuật ─────────────────────────────────────────────────────
  fuel_type    <- get_spec_by_label(pg, c("Nhiên liệu"))
  body_type    <- get_spec_by_label(pg, c("Kiểu dáng"))
  transmission <- get_spec_by_label(pg, c("Hộp số", "Động cơ"))
  seat_count   <- get_spec_by_label(pg, c("Số chỗ", "Số chỗ ngồi"))
  drivetrain   <- get_spec_by_label(pg, c("Hệ dẫn động", "Dẫn động"))
  engine_size  <- get_spec_by_label(pg, c("Dung tích (lít)", "Dung tích"))
  mileage      <- get_spec_by_label(pg, c("Số km đã đi", "Km đã đi", "Odometer"))
  color        <- get_spec_by_label(pg, c("Màu ngoại thất", "Màu xe", "Màu"))
  origin       <- get_spec_by_label(pg, c("Xuất xứ", "Nguồn gốc"))

  if (is.na(transmission))
    transmission <- get_spec_by_label(pg, c("Động cơ"))

  # ── Ghi kết quả ───────────────────────────────────────────────────────────
  results[[i]] <- tibble(
    brand        = brand_raw,
    model        = NA_character_,
    trim         = NA_character_,
    year         = NA_character_,
    body_type    = body_type,
    fuel_type    = fuel_type,
    transmission = transmission,
    engine_size  = engine_size,
    seat_count   = seat_count,
    drivetrain   = drivetrain,
    price        = price_raw,
    mileage      = mileage,
    origin       = origin,
    color        = color,
    city         = city_raw,
    posted_date  = posted_raw,
    source       = SOURCE_DOMAIN,
    url          = u
  )

  cli_progress_update()

  # Nghỉ mỗi SLEEP_INTERVAL records
  Sys.sleep(runif(1, 0.5, 1.0))
}

cli_progress_done()

# ── 10. Kết hợp với dữ liệu cũ & xuất CSV ─────────────────────────────────────
new_df <- bind_rows(results) |> select(all_of(CANONICAL_COLS))

if (file.exists(OUTPUT_CSV) && length(existing_urls) > 0) {
  old_df <- tryCatch(read_csv(OUTPUT_CSV, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(old_df)) {
    final_df <- bind_rows(old_df, new_df) |> distinct(url, .keep_all = TRUE)
  } else {
    final_df <- new_df
  }
} else {
  final_df <- new_df
}

write_csv(final_df, OUTPUT_CSV, na = "NA")

log_info(sprintf("Session scraped %d new records. Total in CSV: %d. Saved to %s",
                 nrow(new_df), nrow(final_df), OUTPUT_CSV))
log_info("=== Carpla scraper finished ===")

close_session(b)