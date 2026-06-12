# ==============================================================================
# Script: scrap_carpla.R
# Purpose: Scrape used car listings from carpla.vn/mua-xe
# Method : chromote (headless Chrome/Edge) – JS-rendered SPA
# Output : web_scraping/data/raw/data_carpla_raw.csv
# Note   : Carpla là SPA — chuyển trang bằng JS click (KHÔNG đổi URL).
#          Cấu trúc giống scrap_chotot.R:
#            STEP A — thu thập toàn bộ listing URLs qua pagination
#            STEP B — cào chi tiết từng xe, flush mỗi 20 xe vào CSV
# ==============================================================================

# ── 0. Libraries ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(chromote)
  library(rvest)
  library(dplyr)
  library(stringr)
})

# ── 1. Config ─────────────────────────────────────────────────────────────────
BASE_DOMAIN      <- "https://carpla.vn"
LISTING_URL      <- "https://carpla.vn/mua-xe"
OUTPUT_DIR       <- "web_scraping/data/raw"
LOG_FILE         <- "web_scraping/log.txt"
SCRIPT_NAME      <- "scrap_carpla.R"
OUTPUT_CSV       <- file.path(OUTPUT_DIR, "data_carpla_raw.csv")
CHECKPOINT_FILE  <- file.path(OUTPUT_DIR, "meta", "checkpoint_carpla.txt")
URLS_FILE        <- file.path(OUTPUT_DIR, "meta", "urls_carpla.txt")
URLNUM_FILE      <- file.path(OUTPUT_DIR, "meta", "urlnum_carpla.txt")
SOURCE_DOMAIN    <- "carpla.vn"
PAGE_TIMEOUT     <- 25000   # ms
MAX_PAGES        <- 300     # giới hạn tối đa mỗi session
SLEEP_INTERVAL   <- 20      # số trang giữa các lần nghỉ
SLEEP_DURATION   <- c(2, 5) # khoảng nghỉ (giây)
BATCH_SIZE       <- 20L
RESET_EVERY      <- 50L

CANONICAL_COLS <- c("brand", "model", "trim", "year", "body_type", "fuel_type",
                    "transmission", "engine_size", "seat_count", "drivetrain",
                    "price", "mileage", "origin", "color", "city",
                    "posted_date", "source", "url")

dir.create(file.path(OUTPUT_DIR, "meta"), recursive = TRUE, showWarnings = FALSE)

# ── 2. Helpers ─────────────────────────────────────────────────────────────────

log_msg <- function(level, msg) {
  entry <- sprintf(
    "[%s] [%s] - %s: %s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    SCRIPT_NAME,
    level,
    msg
  )
  cat(entry)
  cat(entry, file = LOG_FILE, append = TRUE)
}

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

# Scroll xuống cuối trang để trigger lazy-load
scroll_page <- function(session, times = 6, delay = 1.2) {
  for (i in seq_len(times)) {
    tryCatch(
      session$Runtime$evaluate("window.scrollBy(0, window.innerHeight);"),
      error = function(e) NULL
    )
    Sys.sleep(delay)
  }
}

# Lấy spec xe theo label (cấu trúc div label + div.font-semibold của Carpla)
get_spec_by_label <- function(doc, label_patterns) {
  for (pat in label_patterns) {
    val <- tryCatch({
      xpath_lbl <- paste0(
        "//div[not(contains(@class,'font-semibold'))][normalize-space(text())='", pat, "']"
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
          if (!is.na(txt) && nchar(str_squish(txt)) > 0)
            return(str_squish(txt))
        }
        val_node2 <- html_node(lbl_node,
          xpath = "parent::div/div[contains(@class,'font-semibold')]")
        if (!is.null(val_node2)) {
          txt2 <- html_text(val_node2, trim = TRUE)
          if (!is.na(txt2) && nchar(str_squish(txt2)) > 0)
            return(str_squish(txt2))
        }
      }
      NA_character_
    }, error = function(e) NA_character_)

    if (!is.na(val)) return(val)
  }
  NA_character_
}

# Row rỗng khi lỗi detail
empty_row <- function(url) {
  r <- setNames(as.list(rep(NA_character_, length(CANONICAL_COLS))), CANONICAL_COLS)
  r$source <- SOURCE_DOMAIN
  r$url    <- url
  as.data.frame(r, stringsAsFactors = FALSE)
}

# ── Checkpoint helpers ─────────────────────────────────────────────────────────
save_checkpoint <- function(page_num) {
  writeLines(as.character(page_num), CHECKPOINT_FILE)
}

read_checkpoint <- function() {
  if (!file.exists(CHECKPOINT_FILE)) return(0L)
  val <- suppressWarnings(as.integer(readLines(CHECKPOINT_FILE, warn = FALSE)[1]))
  if (is.na(val)) 0L else val
}

save_urlnum <- function(idx) {
  writeLines(as.character(idx), URLNUM_FILE)
}

read_urlnum <- function() {
  if (!file.exists(URLNUM_FILE)) return(0L)
  val <- suppressWarnings(as.integer(readLines(URLNUM_FILE, warn = FALSE)[1]))
  if (is.na(val)) 0L else val
}

# ── Session helpers ────────────────────────────────────────────
make_session <- function(max_tries = 3) {
  for (attempt in seq_len(max_tries)) {
    sess <- tryCatch({
      s <- ChromoteSession$new()
      Sys.sleep(2)
      s
    }, error = function(e) {
      log_msg("WARN", sprintf("Session launch attempt %d failed: %s", attempt, e$message))
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

safe_navigate <- function(sess, url, timeout = PAGE_TIMEOUT) {
  result <- tryCatch({
    sess$Page$navigate(url, wait_ = FALSE)
    sess$Page$loadEventFired(timeout_ = timeout)
    Sys.sleep(3)
    list(ok = TRUE, session = sess)
  }, error = function(e) {
    log_msg("WARN", sprintf("Nav error '%s': %s — restarting session", url, e$message))
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
      log_msg("WARN", sprintf("Retry nav error '%s': %s", url, e2$message))
      list(ok = FALSE, session = new_sess)
    })
    result2
  })
  result
}

# ── Click nút next của Carpla (SPA – không đổi URL) ───────────────────────────
# Trả về: "clicked:<selector>" | "not_found" | "disabled" | "error"
click_next_page <- function(sess) {
  js_click <- '(function() {
    var selectors = [
      "a[aria-label=\"Next page\"]",
      "a[aria-label=\"Trang sau\"]",
      "button[aria-label=\"Next page\"]",
      "button[aria-label=\"Trang sau\"]",
      "li.next a",
      "a.next",
      "[class*=\"pagination\"] a[rel=\"next\"]",
      "[class*=\"pagination\"] li:last-child a",
      "[class*=\"Pagination\"] button:last-child",
      "[class*=\"pagination\"] button:last-child"
    ];
    for (var i = 0; i < selectors.length; i++) {
      var el = document.querySelector(selectors[i]);
      if (el) {
        if (el.disabled || el.getAttribute("aria-disabled") === "true") {
          return "disabled:" + selectors[i];
        }
        el.click();
        return "clicked:" + selectors[i];
      }
    }
    return "not_found";
  })()'

  tryCatch({
    res <- sess$Runtime$evaluate(js_click)
    val <- res$result$value
    if (is.null(val) || is.na(val) || nchar(val) == 0) "error" else val
  }, error = function(e) {
    log_msg("WARN", sprintf("JS click error: %s", e$message))
    "error"
  })
}

# ── 3. Launch browser ─────────────────────────────────────────────────────────
log_msg("INFO", "=== Carpla scraper started ===")

if (Sys.getenv("CHROMOTE_CHROME") == "") {
  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    Sys.setenv(CHROMOTE_CHROME = found[1])
  } else {
    stop("Chrome/Edge not found. Set CHROMOTE_CHROME manually.")
  }
}

b <- make_session()
log_msg("INFO", "Browser session ready.")

# ── 4. Đọc checkpoint & urlnum ────────────────────────────────────────────────
last_checkpoint <- read_checkpoint()
start_page      <- last_checkpoint + 1L
log_msg("INFO", sprintf("Checkpoint: last completed page = %d. Starting from page %d.",
                        last_checkpoint, start_page))

last_urlnum <- read_urlnum()
start_from  <- last_urlnum + 1L
if (last_urlnum > 0L) {
  log_msg("INFO", sprintf("urlnum checkpoint: last flushed URL index = %d. STEP B will resume from #%d.",
                          last_urlnum, start_from))
}

# ══════════════════════════════════════════════════════════════════════════════
# STEP A: Thu thập toàn bộ listing URLs (pagination bằng JS click)
# ══════════════════════════════════════════════════════════════════════════════
log_msg("INFO", "=== STEP A: Collecting listing URLs (SPA click-based pagination) ===")

all_urls           <- character(0)
page_num           <- start_page
pages_this_session <- 0L

# ── Navigate đến trang gốc (1 lần duy nhất) ──────────────────────────────────
nav_result <- safe_navigate(b, LISTING_URL)
b <- nav_result$session

if (!nav_result$ok || is.null(b)) {
  log_msg("ERROR", "Failed to navigate to listing page. Aborting.")
  close_session(b)
  stop("Cannot load listing page.")
}

# ── Nếu resume từ trang > 1: click qua các trang đã xử lý từ session trước ───
# (Carpla SPA không có URL trang — phải click tuần tự từ trang 1)
if (start_page > 1L) {
  log_msg("INFO", sprintf("Resuming: fast-forwarding through pages 1..%d (no URL collection).",
                          start_page - 1L))
  for (skip_p in seq_len(start_page - 1L)) {
    scroll_page(b, times = 3, delay = 0.5)   # scroll nhanh, không cần đầy đủ
    clicked <- click_next_page(b)
    if (is.null(clicked) || is.na(clicked) || clicked == "not_found" || clicked == "disabled" || clicked == "error") {
      log_msg("WARN", sprintf("Fast-forward: could not click next at skip page %d (%s). Starting from here.",
                              skip_p, clicked))
      page_num <- skip_p + 1L   # cập nhật lại page_num thực tế
      start_page <- page_num
      break
    }
    Sys.sleep(runif(1, 1.5, 2.5))
  }
  log_msg("INFO", sprintf("Fast-forward done. Now at page %d.", page_num))
}

prev_links_set <- character(0)

repeat {
  # Giới hạn trang mỗi session
  if (pages_this_session >= MAX_PAGES) {
    log_msg("INFO", sprintf("Reached session page limit (%d). Stopping URL collection.", MAX_PAGES))
    break
  }

  log_msg("INFO", sprintf("Collecting page %d...", page_num))

  # Scroll để trigger lazy-load
  scroll_page(b, times = 6, delay = 1.2)

  # Lấy HTML trang hiện tại
  pg_html <- get_page_html(b)
  if (is.null(pg_html)) {
    log_msg("WARN", sprintf("Page %d: Could not get HTML. Stopping.", page_num))
    break
  }

  # Trích xuất listing links (pattern: /xe/<slug>)
  links <- pg_html |>
    html_nodes("a[href^='/xe/']") |>
    html_attr("href")

  links <- links[!is.na(links)]
  links <- links[str_detect(links, "^/xe/[a-z0-9-]+-[a-z0-9]+$")]
  links <- unique(links)

  if (length(links) == 0) {
    log_msg("INFO", sprintf("Page %d: No listings found — stopping pagination.", page_num))
    break
  }

  # Phát hiện trang không thay đổi (click next không có tác dụng)
  if (length(prev_links_set) > 0 && setequal(links, prev_links_set)) {
    log_msg("INFO", sprintf("Page %d: Same links as previous page — assuming last page, stopping.", page_num))
    break
  }
  prev_links_set <- links

  full_links <- paste0(BASE_DOMAIN, links)
  new_only   <- setdiff(full_links, all_urls)
  all_urls   <- c(all_urls, new_only)

  log_msg("INFO", sprintf("Page %d: +%d new URLs | Total: %d",
                          page_num, length(new_only), length(all_urls)))

  # Ghi URLs mới vào file ngay (tránh mất khi treo)
  if (length(new_only) > 0) {
    write(new_only, file = URLS_FILE, append = TRUE, sep = "\n")
  }

  # Lưu checkpoint trang vừa hoàn thành
  save_checkpoint(page_num)
  pages_this_session <- pages_this_session + 1L
  page_num           <- page_num + 1L

  # ── Click nút next để sang trang tiếp theo ──────────────────────────────
  clicked <- click_next_page(b)

  if (is.null(clicked) || is.na(clicked) || clicked == "disabled") {
    log_msg("INFO", sprintf("Next button disabled at page %d — reached last page.", page_num - 1L))
    break
  }

  if (is.null(clicked) || is.na(clicked) || clicked == "not_found") {
    log_msg("INFO", sprintf("Next button not found at page %d — assuming last page, stopping.", page_num - 1L))
    break
  }

  if (is.null(clicked) || is.na(clicked) || clicked == "error") {
    log_msg("WARN", "JS click returned error — stopping pagination.")
    break
  }

  log_msg("INFO", sprintf("Page %d: Next button clicked (%s)", page_num - 1L, clicked))

  # Chờ SPA render trang mới (quan trọng: phải đợi đủ JS render)
  if (pages_this_session %% SLEEP_INTERVAL == 0) {
    sleep_sec <- runif(1, SLEEP_DURATION[1], SLEEP_DURATION[2])
    log_msg("INFO", sprintf("Batch pause: sleeping %.1f seconds...", sleep_sec))
    Sys.sleep(sleep_sec)
  } else {
    Sys.sleep(runif(1, 2.5, 3.5))   # chờ SPA render
  }
}

log_msg("INFO", sprintf("STEP A done. New URLs collected this session: %d", length(all_urls)))

# Đọc lại toàn bộ URLs từ file (gộp các session trước)
if (file.exists(URLS_FILE)) {
  saved_urls <- tryCatch(readLines(URLS_FILE, warn = FALSE), error = function(e) character(0))
  all_urls   <- unique(saved_urls[nchar(saved_urls) > 0])
  log_msg("INFO", sprintf("Loaded %d total URLs from file (all sessions).", length(all_urls)))
}

if (length(all_urls) == 0) {
  log_msg("WARN", "No URLs found. Nothing to scrape.")
  close_session(b)
  stop("No listing URLs found.")
}

# ══════════════════════════════════════════════════════════════════════════════
# STEP B: Cào chi tiết từng xe — flush mỗi BATCH_SIZE xe vào CSV
# ══════════════════════════════════════════════════════════════════════════════
log_msg("INFO", "=== STEP B: Scraping detail pages ===")
if (last_urlnum > 0L) {
  log_msg("INFO", sprintf("Resuming from car #%d (skipping first %d already flushed).",
                          start_from, last_urlnum))
}

# Đảm bảo CSV tồn tại với đúng header
if (!file.exists(OUTPUT_CSV)) {
  write.csv(
    setNames(data.frame(matrix(ncol = length(CANONICAL_COLS), nrow = 0)), CANONICAL_COLS),
    OUTPUT_CSV, row.names = FALSE, fileEncoding = "UTF-8"
  )
  log_msg("INFO", sprintf("Created output file with header: %s", OUTPUT_CSV))
}

# flush_batch: ghi CSV xong mới ghi urlnum — đảm bảo 2 cái luôn đồng bộ
flush_batch <- function(buf, up_to_idx) {
  if (length(buf) == 0) return(invisible(NULL))
  df  <- bind_rows(buf)
  # Đảm bảo đúng cột canonical
  for (col in CANONICAL_COLS) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  df <- df[, CANONICAL_COLS]
  con <- file(OUTPUT_CSV, open = "a", encoding = "UTF-8")
  write.table(df, con, sep = ",", append = TRUE,
              row.names = FALSE, col.names = FALSE, na = "")
  close(con)
  save_urlnum(up_to_idx)
  log_msg("INFO", sprintf("Flushed %d records to CSV. urlnum = %d.", nrow(df), up_to_idx))
}

batch_buf     <- list()
session_count <- 0L
total_scraped <- 0L
total         <- length(all_urls)

for (i in seq_len(total)) {
  u <- all_urls[i]

  # ── Skip xe đã flush trong session trước ────────────────────────────────
  if (i < start_from) next

  # ── Proactive session reset mỗi RESET_EVERY xe ──────────────────────────
  if (session_count > 0L && session_count %% RESET_EVERY == 0L) {
    log_msg("INFO", sprintf("[%d/%d] Proactive session reset...", i, total))
    flush_batch(batch_buf, i - 1L)
    batch_buf     <- list()
    close_session(b)
    Sys.sleep(3)
    gc()
    b             <- make_session()
    session_count <- 0L
    log_msg("INFO", "New session started.")
  }

  # ── Navigate đến trang chi tiết ─────────────────────────────────────────
  nav_result <- safe_navigate(b, u)
  b          <- nav_result$session

  if (!nav_result$ok || is.null(b)) {
    log_msg("WARN", sprintf("[%d/%d] Failed to navigate to %s", i, total, u))
    batch_buf[[length(batch_buf) + 1]] <- empty_row(u)
  } else {
    pg <- get_page_html(b)
    if (is.null(pg)) {
      log_msg("WARN", sprintf("[%d/%d] Could not get HTML for %s", i, total, u))
      batch_buf[[length(batch_buf) + 1]] <- empty_row(u)
    } else {

      # ── Brand (từ h1) ──────────────────────────────────────────────────
      brand_raw <- tryCatch(
        clean_str(html_text(html_node(pg, "h1.text-secondary, h1[class*='text-secondary']"), trim = TRUE)),
        error = function(e) NA_character_
      )
      if (is.na(brand_raw)) {
        brand_raw <- tryCatch(
          clean_str(html_text(html_node(pg, "h1"), trim = TRUE)),
          error = function(e) NA_character_
        )
      }

      # ── Price ──────────────────────────────────────────────────────────
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

      # ── Posted date ────────────────────────────────────────────────────
      posted_raw <- tryCatch({
        date_nodes <- html_nodes(pg, "div.text-secondary.text-sm, div[class*='text-secondary'][class*='text-sm']")
        found_date <- NA_character_
        for (dn in date_nodes) {
          txt <- html_text(dn, trim = TRUE)
          if (str_detect(ifelse(is.na(txt), "", txt),
                         "trước|hôm nay|hôm qua|tháng|tuần|ngày|giờ|phút")) {
            parts <- str_split(txt, "\\s*[Đđ]ăng\\s*")[[1]]
            parts <- str_trim(parts[parts != ""])
            if (length(parts) >= 1) { found_date <- clean_str(parts[length(parts)]); break }
          }
        }
        found_date
      }, error = function(e) NA_character_)

      # ── City ───────────────────────────────────────────────────────────
      city_raw <- tryCatch({
        addr_nodes <- html_nodes(pg, "div.text-secondary.text-sm")
        found_city <- NA_character_
        for (an in addr_nodes) {
          txt <- html_text(an, trim = TRUE)
          if (!is.na(txt) && str_detect(txt, ",") &&
              !str_detect(txt, "trước|hôm nay|hôm qua|[Đđ]ăng")) {
            found_city <- clean_str(txt); break
          }
        }
        found_city
      }, error = function(e) NA_character_)

      # ── Specs ──────────────────────────────────────────────────────────
      fuel_type    <- get_spec_by_label(pg, c("Nhiên liệu"))
      body_type    <- get_spec_by_label(pg, c("Kiểu dáng"))
      transmission <- get_spec_by_label(pg, c("Hộp số"))
      if (is.na(transmission)) transmission <- get_spec_by_label(pg, c("Động cơ"))
      seat_count   <- get_spec_by_label(pg, c("Số chỗ", "Số chỗ ngồi"))
      drivetrain   <- get_spec_by_label(pg, c("Hệ dẫn động", "Dẫn động"))
      engine_size  <- get_spec_by_label(pg, c("Dung tích (lít)", "Dung tích"))
      mileage      <- get_spec_by_label(pg, c("Số km đã đi", "Km đã đi", "Odometer"))
      color        <- get_spec_by_label(pg, c("Màu ngoại thất", "Màu xe", "Màu"))
      origin       <- get_spec_by_label(pg, c("Xuất xứ", "Nguồn gốc"))
      year_raw     <- get_spec_by_label(pg, c("Năm sản xuất", "Năm sx"))

      batch_buf[[length(batch_buf) + 1]] <- data.frame(
        brand        = clean_str(brand_raw),
        model        = NA_character_,
        trim         = NA_character_,
        year         = clean_str(year_raw),
        body_type    = clean_str(body_type),
        fuel_type    = clean_str(fuel_type),
        transmission = clean_str(transmission),
        engine_size  = clean_str(engine_size),
        seat_count   = clean_str(seat_count),
        drivetrain   = clean_str(drivetrain),
        price        = clean_str(price_raw),
        mileage      = clean_str(mileage),
        origin       = clean_str(origin),
        color        = clean_str(color),
        city         = clean_str(city_raw),
        posted_date  = clean_str(posted_raw),
        source       = SOURCE_DOMAIN,
        url          = u,
        stringsAsFactors = FALSE
      )
    }
  }

  session_count <- session_count + 1L
  total_scraped <- total_scraped + 1L
  cat(sprintf("[carpla] %d/%d scraped\n", i, total))
  Sys.sleep(runif(1, 0.5, 1.0))

  # ── Flush mỗi BATCH_SIZE xe ─────────────────────────────────────────────
  if (length(batch_buf) >= BATCH_SIZE || i == total) {
    flush_batch(batch_buf, i)
    batch_buf <- list()
    gc()
  }
}

# ── Kết thúc ──────────────────────────────────────────────────────────────────
log_msg("INFO", sprintf("Session scraped %d new records. Appended to: %s",
                        total_scraped, OUTPUT_CSV))
log_msg("INFO", "=== Carpla scraper finished ===")

close_session(b)
gc()