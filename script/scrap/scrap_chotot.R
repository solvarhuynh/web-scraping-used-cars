# ==============================================================================
# Script: scrap_chotot.R
# Purpose: Scrape used car listings from xe.chotot.com
# Output:  data/raw/data_chotot_raw.csv
# ==============================================================================

# ── 0. Libraries ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(chromote)   # headless Chrome automation
  library(rvest)      # HTML parsing
  library(xml2)
  library(dplyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(cli)        # progress bar
})

# ── 1. Config ─────────────────────────────────────────────────────────────────
BASE_URL         <- "https://xe.chotot.com"
LISTING_URL      <- "https://xe.chotot.com/mua-ban-oto"
OUTPUT_DIR       <- "data/raw"
LOG_FILE         <- "log.txt"
SCRIPT_NAME      <- "scrap_chotot.R"
OUTPUT_FILE      <- file.path(OUTPUT_DIR, "data_chotot_raw.csv")
CHECKPOINT_FILE  <- file.path(OUTPUT_DIR, "checkpoint_chotot.txt")
MAX_PAGES        <- 300     # giới hạn tối đa mỗi session
SLEEP_INTERVAL   <- 20       # số trang giữa các lần nghỉ
SLEEP_DURATION   <- c(2, 5) # khoảng nghỉ (giây)

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

# Parse relative Vietnamese date strings → Date
parse_posted_date <- function(raw) {
  raw <- str_trim(raw)
  today <- Sys.Date()

  if (grepl("hôm nay|today", raw, ignore.case = TRUE))     return(today)
  if (grepl("hôm qua|yesterday", raw, ignore.case = TRUE)) return(today - 1)
  if (grepl("giờ trước|phút trước|tiếng trước", raw, ignore.case = TRUE))
    return(today)

  m <- str_match(raw, "(\\d+)\\s*ngày trước")
  if (!is.na(m[1, 1])) return(today - as.integer(m[1, 2]))

  m <- str_match(raw, "(\\d+)\\s*tuần trước")
  if (!is.na(m[1, 1])) return(today - as.integer(m[1, 2]) * 7)

  m <- str_match(raw, "(\\d+)\\s*tháng trước")
  if (!is.na(m[1, 1])) return(today - as.integer(m[1, 2]) * 30)

  tryCatch(as.Date(raw, format = "%d/%m/%Y"), error = function(e) today)
}

clean_text <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  x |> str_trim() |> str_squish()
}

na_if_empty <- function(x) if (is.na(x) || str_trim(x) == "") NA_character_ else clean_text(x)

`%||%` <- function(a, b) if (length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Get itemprop value from detail page HTML
get_itemprop <- function(page_html, prop) {
  node <- page_html |>
    html_node(sprintf('[itemprop="%s"]', prop))
  if (is.null(node)) return(NA_character_)
  val <- html_text(node, trim = TRUE)
  na_if_empty(val)
}

# Extract value from label-value pairs (div.p1ja3eq0 structure)
get_by_label <- function(page_html, label_pattern) {
  pairs <- page_html |> html_nodes("div.p1ja3eq0")
  for (pair in pairs) {
    spans <- pair |> html_nodes("span.bwq0cbs")
    if (length(spans) >= 2) {
      lbl <- html_text(spans[[1]], trim = TRUE)
      if (str_detect(lbl, regex(label_pattern, ignore_case = TRUE))) {
        return(na_if_empty(html_text(spans[[2]], trim = TRUE)))
      }
    }
  }
  NA_character_
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

# ── Session helpers ────────────────────────────────────────────────────────────
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

safe_navigate <- function(sess, url, timeout = 25000) {
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

# ── 3. Launch headless browser ────────────────────────────────────────────────
log_msg("INFO", "Launching headless Chrome browser")

if (Sys.getenv("CHROMOTE_CHROME") == "") {
  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Program Files/Chromium/Application/chromium.exe"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    Sys.setenv(CHROMOTE_CHROME = found[1])
  } else {
    stop("Chrome/Edge not found. Set CHROMOTE_CHROME env variable manually.")
  }
}

b <- make_session()
log_msg("INFO", "Browser session ready.")

# ── 4. Đọc checkpoint & trạng thái CSV hiện có ────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

last_checkpoint <- read_checkpoint()
start_page      <- last_checkpoint + 1L
log_msg("INFO", sprintf("Checkpoint: last completed page = %d. Starting from page %d.",
                        last_checkpoint, start_page))

existing_urls <- character(0)
if (file.exists(OUTPUT_FILE)) {
  existing_df   <- tryCatch(read_csv(OUTPUT_FILE, show_col_types = FALSE), error = function(e) NULL)
  existing_urls <- if (!is.null(existing_df) && "url" %in% names(existing_df))
                     existing_df$url else character(0)
  log_msg("INFO", sprintf("Loaded %d existing URLs from output CSV.", length(existing_urls)))
}

# ── 5. Collect listing URLs across all pages ──────────────────────────────────
log_msg("INFO", "=== Step A+C: Collecting listing URLs ===")

all_urls           <- character(0)
page_num           <- start_page
pages_this_session <- 0L

repeat {
  # Giới hạn 500 trang mỗi session
  if (pages_this_session >= MAX_PAGES) {
    log_msg("INFO", sprintf("Reached session page limit (%d). Stopping URL collection.", MAX_PAGES))
    break
  }

  page_url <- if (page_num == 1) LISTING_URL else
    paste0(LISTING_URL, "?page=", page_num)

  log_msg("INFO", sprintf("Navigating to listing page %d: %s", page_num, page_url))

  nav_result <- safe_navigate(b, page_url)
  b <- nav_result$session

  if (!nav_result$ok || is.null(b)) {
    log_msg("WARN", sprintf("Failed to navigate to page %d. Stopping URL collection.", page_num))
    break
  }

  # Scroll to bottom to trigger lazy-load
  for (i in seq_len(5)) {
    tryCatch(b$Runtime$evaluate('window.scrollBy(0, window.innerHeight)'),
             error = function(e) NULL)
    Sys.sleep(1)
  }

  # Get rendered HTML
  html_content <- tryCatch(
    b$Runtime$evaluate('document.documentElement.outerHTML')$result$value,
    error = function(e) NULL
  )
  if (is.null(html_content)) {
    log_msg("WARN", sprintf("Page %d: Could not get HTML. Skipping.", page_num))
    break
  }

  page_html <- tryCatch(read_html(html_content, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(page_html)) {
    log_msg("WARN", sprintf("Page %d: Could not parse HTML. Skipping.", page_num))
    break
  }

  links <- page_html |>
    html_nodes("a.c15fd2pn") |>
    html_attr("href") |>
    na.omit()

  links <- links[str_detect(links, "/\\d+\\.htm")]
  links <- str_replace(links, "#.*$", "")
  full_links <- unique(paste0(BASE_URL, links))

  if (length(full_links) == 0) {
    log_msg("INFO", sprintf("No listings found on page %d — stopping pagination", page_num))
    break
  }

  new_only <- setdiff(full_links, c(all_urls, existing_urls))
  all_urls <- c(all_urls, new_only)

  log_msg("INFO", sprintf("Page %d: found %d links (%d new) | Total new: %d",
                          page_num, length(full_links), length(new_only), length(all_urls)))

  # Lưu checkpoint sau mỗi trang thành công
  save_checkpoint(page_num)
  pages_this_session <- pages_this_session + 1L
  page_num <- page_num + 1L

  # Nghỉ mỗi SLEEP_INTERVAL trang
  if (pages_this_session %% SLEEP_INTERVAL == 0) {
    sleep_sec <- runif(1, SLEEP_DURATION[1], SLEEP_DURATION[2])
    log_msg("INFO", sprintf("Pausing %.1f seconds after %d pages...", sleep_sec, pages_this_session))
    Sys.sleep(sleep_sec)
  } else {
    Sys.sleep(runif(1, 1.5, 2.5))
  }
}

log_msg("INFO", sprintf("Total new listing URLs collected this session: %d", length(all_urls)))

if (length(all_urls) == 0) {
  log_msg("WARN", "No new URLs found. Nothing to scrape.")
  close_session(b)
  stop("No new listing URLs found.")
}

# ── 6. Scrape a single car detail page ────────────────────────────────────────
scrape_car <- function(url) {
  tryCatch({
    nav_result <- safe_navigate(b, url)
    b <<- nav_result$session   # cập nhật session trong môi trường cha

    if (!nav_result$ok || is.null(b)) {
      log_msg("WARN", sprintf("Failed to navigate to %s", url))
      return(tibble::tibble(
        brand=NA, model=NA, trim=NA, year=NA_integer_, body_type=NA,
        fuel_type=NA, transmission=NA, engine_size=NA_real_, seat_count=NA_integer_,
        drivetrain=NA, price=NA_character_, mileage=NA_integer_, origin=NA,
        color=NA, city=NA, posted_date=NA_character_,
        source="xe.chotot.com", url=url
      ))
    }

    html_content <- tryCatch(
      b$Runtime$evaluate('document.documentElement.outerHTML')$result$value,
      error = function(e) NULL
    )
    if (is.null(html_content)) stop("Could not get HTML")

    pg <- read_html(html_content, encoding = "UTF-8")

    # ── Brand
    brand <- get_itemprop(pg, "carbrand")
    if (is.na(brand)) brand <- get_by_label(pg, "hãng|thương hiệu")

    # ── Model
    model <- get_itemprop(pg, "carmodel")
    if (is.na(model)) model <- get_by_label(pg, "dòng xe|model")

    # ── Trim
    trim <- get_itemprop(pg, "option")
    if (is.na(trim)) trim <- get_by_label(pg, "phiên bản")

    # ── Year
    year_raw <- get_itemprop(pg, "mfdate")
    if (is.na(year_raw)) year_raw <- get_by_label(pg, "năm sản xuất|năm sx")
    year <- suppressWarnings(as.integer(str_extract(year_raw %||% "", "\\d{4}")))

    # ── Body type
    body_type <- get_itemprop(pg, "cartype")
    if (is.na(body_type)) body_type <- get_by_label(pg, "kiểu dáng|kiểu xe")
    if (!is.na(body_type)) body_type <- str_trim(str_split(body_type, "/")[[1]][1])

    # ── Fuel type
    fuel_raw <- get_itemprop(pg, "fuel")
    if (is.na(fuel_raw)) fuel_raw <- get_by_label(pg, "nhiên liệu")
    fuel_type <- dplyr::case_when(
      str_detect(fuel_raw %||% "", regex("xăng", ignore_case = TRUE))        ~ "Petrol",
      str_detect(fuel_raw %||% "", regex("dầu|diesel", ignore_case = TRUE))  ~ "Diesel",
      str_detect(fuel_raw %||% "", regex("hybrid", ignore_case = TRUE))      ~ "Hybrid",
      str_detect(fuel_raw %||% "", regex("điện|electric", ignore_case = TRUE)) ~ "Electric",
      !is.na(fuel_raw) ~ fuel_raw,
      TRUE ~ NA_character_
    )

    # ── Transmission
    trans_raw <- get_itemprop(pg, "gearbox")
    if (is.na(trans_raw)) trans_raw <- get_by_label(pg, "hộp số")
    transmission <- dplyr::case_when(
      str_detect(trans_raw %||% "", regex("tự động|automatic", ignore_case = TRUE)) ~ "Automatic",
      str_detect(trans_raw %||% "", regex("số sàn|manual", ignore_case = TRUE))     ~ "Manual",
      str_detect(trans_raw %||% "", regex("cvt", ignore_case = TRUE))               ~ "CVT",
      !is.na(trans_raw) ~ trans_raw,
      TRUE ~ NA_character_
    )

    # ── Engine size
    engine_raw  <- get_by_label(pg, "dung tích|động cơ")
    engine_size <- suppressWarnings(as.numeric(str_extract(engine_raw %||% "", "[0-9]+\\.?[0-9]*")))

    # ── Seat count
    seat_raw   <- get_by_label(pg, "số chỗ|số ghế|chỗ ngồi")
    seat_count <- suppressWarnings(as.integer(str_extract(seat_raw %||% "", "\\d+")))

    # ── Drivetrain
    drive_raw  <- get_by_label(pg, "dẫn động|cầu dẫn động")
    drivetrain <- dplyr::case_when(
      str_detect(drive_raw %||% "", regex("4wd|4x4", ignore_case = TRUE))         ~ "4WD",
      str_detect(drive_raw %||% "", regex("awd", ignore_case = TRUE))              ~ "AWD",
      str_detect(drive_raw %||% "", regex("fwd|cầu trước", ignore_case = TRUE))   ~ "FWD",
      str_detect(drive_raw %||% "", regex("rwd|cầu sau", ignore_case = TRUE))     ~ "RWD",
      !is.na(drive_raw) ~ drive_raw,
      TRUE ~ NA_character_
    )

    # ── Price
    price_raw <- tryCatch(
      na_if_empty(html_text(html_node(pg, "b.p26z2wb"), trim = TRUE)),
      error = function(e) NA_character_
    )
    if (is.na(price_raw)) {
      price_node <- pg |> html_node('[itemprop="price"]')
      price_raw  <- if (!is.null(price_node)) html_attr(price_node, "content") else NA_character_
    }
    if (is.na(price_raw)) {
      price_raw <- tryCatch({
        nodes <- pg |> html_nodes("b, strong")
        found <- NA_character_
        for (nd in nodes) {
          txt <- html_text(nd, trim = TRUE)
          if (!is.na(txt) && str_detect(txt, "đ$|\\d+\\.\\d{3}")) {
            found <- na_if_empty(txt); break
          }
        }
        found
      }, error = function(e) NA_character_)
    }
    price <- price_raw

    # ── Mileage
    mileage_raw <- get_itemprop(pg, "mileage_v2")
    if (is.na(mileage_raw)) mileage_raw <- get_by_label(pg, "số km|số km đã đi|km đã đi")
    mileage <- suppressWarnings(as.integer(str_replace_all(mileage_raw %||% "", "[^0-9]", "")))

    # ── Origin
    origin_raw <- get_itemprop(pg, "carorigin")
    if (is.na(origin_raw)) origin_raw <- get_by_label(pg, "xuất xứ|nguồn gốc")
    origin <- dplyr::case_when(
      str_detect(origin_raw %||% "", regex("trong nước|việt nam", ignore_case = TRUE)) ~ "Trong nước",
      str_detect(origin_raw %||% "", regex("nhập|nước ngoài|nước khác", ignore_case = TRUE)) ~ "Nhập khẩu",
      !is.na(origin_raw) ~ origin_raw,
      TRUE ~ NA_character_
    )

    # ── Color
    color <- get_by_label(pg, "màu sắc|màu ngoại thất|màu xe")

    # ── City
    city_raw <- tryCatch(
      na_if_empty(html_text(html_node(pg, "span.bwq0cbs.flex-1"), trim = TRUE)),
      error = function(e) NA_character_
    )
    if (is.na(city_raw)) {
      city_raw <- tryCatch({
        spans <- pg |> html_nodes("span.bwq0cbs")
        found <- NA_character_
        for (sp in spans) {
          txt <- html_text(sp, trim = TRUE)
          if (!is.na(txt) && str_detect(txt, ",") &&
              !str_detect(txt, "trước|hôm|Đăng|đăng")) {
            found <- na_if_empty(txt); break
          }
        }
        found
      }, error = function(e) NA_character_)
    }
    if (is.na(city_raw)) {
      city_raw <- tryCatch(
        na_if_empty(html_text(html_node(pg, '[itemprop="addressLocality"]'), trim = TRUE)),
        error = function(e) NA_character_
      )
    }
    if (is.na(city_raw)) city_raw <- get_by_label(pg, "khu vực|tỉnh thành|thành phố")
    city <- city_raw

    # ── Posted date
    posted_raw <- tryCatch({
      spans <- pg |> html_nodes("span.bwq0cbs")
      found <- NA_character_
      for (sp in spans) {
        txt <- html_text(sp, trim = TRUE)
        if (!is.na(txt) && str_detect(txt, regex("đăng", ignore_case = TRUE)) &&
            str_detect(txt, "trước|hôm nay|hôm qua")) {
          cleaned <- str_replace(txt, regex("^\\s*đăng\\s*", ignore_case = TRUE), "")
          cleaned <- str_trim(cleaned)
          if (nchar(cleaned) > 0) { found <- cleaned; break }
        }
      }
      if (is.na(found)) {
        for (sp in spans) {
          txt <- html_text(sp, trim = TRUE)
          if (!is.na(txt) &&
              str_detect(txt, "^\\d+\\s*(giờ|phút|ngày|tuần|tháng)\\s*trước$|^hôm (nay|qua)$")) {
            found <- txt; break
          }
        }
      }
      na_if_empty(found)
    }, error = function(e) NA_character_)
    posted_date <- posted_raw

    tibble::tibble(
      brand        = na_if_empty(brand),
      model        = na_if_empty(model),
      trim         = na_if_empty(trim),
      year         = year,
      body_type    = na_if_empty(body_type),
      fuel_type    = na_if_empty(fuel_type),
      transmission = na_if_empty(transmission),
      engine_size  = engine_size,
      seat_count   = seat_count,
      drivetrain   = na_if_empty(drivetrain),
      price        = price,
      mileage      = mileage,
      origin       = na_if_empty(origin),
      color        = na_if_empty(color),
      city         = na_if_empty(city),
      posted_date  = posted_date,
      source       = "xe.chotot.com",
      url          = url
    )
  }, error = function(e) {
    log_msg("WARN", sprintf("Failed to scrape %s — %s", url, conditionMessage(e)))
    tibble::tibble(
      brand=NA, model=NA, trim=NA, year=NA_integer_, body_type=NA,
      fuel_type=NA, transmission=NA, engine_size=NA_real_, seat_count=NA_integer_,
      drivetrain=NA, price=NA_character_, mileage=NA_integer_, origin=NA,
      color=NA, city=NA, posted_date=NA_character_,
      source="xe.chotot.com", url=url
    )
  })
}

# ── 7. Scrape each detail page ─────────────────────────────────────────────────
log_msg("INFO", "=== Step B: Scraping detail pages ===")

results <- vector("list", length(all_urls))

cli_progress_bar(
  name   = "Scraping xe.chotot.com",
  total  = length(all_urls),
  format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
)

for (i in seq_along(all_urls)) {
  results[[i]] <- scrape_car(all_urls[i])
  cli_progress_update()

  # Nghỉ mỗi SLEEP_INTERVAL records
  Sys.sleep(runif(1, 0.5, 1.0))
}

cli_progress_done()

# ── 8. Combine with existing data & save ──────────────────────────────────────
new_df <- bind_rows(results) |>
  mutate(
    year        = as.integer(year),
    engine_size = as.numeric(engine_size),
    seat_count  = as.integer(seat_count),
    mileage     = as.integer(mileage)
  )

if (file.exists(OUTPUT_FILE) && length(existing_urls) > 0) {
  old_df <- tryCatch(read_csv(OUTPUT_FILE, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(old_df)) {
    final_df <- bind_rows(old_df, new_df) |> distinct(url, .keep_all = TRUE)
  } else {
    final_df <- new_df
  }
} else {
  final_df <- new_df
}

write.csv(final_df, OUTPUT_FILE, row.names = FALSE, fileEncoding = "UTF-8", na = "")

log_msg("INFO", sprintf("Session scraped %d new records. Total in CSV: %d. Saved to %s",
                        nrow(new_df), nrow(final_df), OUTPUT_FILE))
log_msg("INFO", "=== Scrape complete ===")

close_session(b)