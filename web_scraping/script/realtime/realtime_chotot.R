# Realtime scraper for Chợ Tốt (Page 1 only)
# Reuses the exact HTML parsing logic from web_scraping/script/scrap/scrap_chotot.R
# Returns the number of newly inserted listings.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(cli)
})

# Load utilities and the batch scraper (contains helpers and the detail scraper)
source("web_scraping/script/utils.R")
source("web_scraping/script/scrap/scrap_chotot.R")  # defines make_session(), safe_navigate(), get_page_html(), scrape_car(), etc.

SCRIPT_NAME <- "realtime_chotot.R"
TABLE_NAME <- "car_listings"
SOURCE_NAME <- "xe.chotot.com"

#' Run realtime Chợ Tốt scraper (page 1)
#' @param con DBI connection to the SQLite database (car_listings must exist)
#' @return Number of newly inserted listings
run_realtime_chotot <- function(con) {
  log_message(SCRIPT_NAME, "Starting realtime Chợ Tốt scraper (page 1).")

  # ---------------------------------------------------------------
  # 1. Initialise Chromote session (single session reused for all detail pages)
  # ---------------------------------------------------------------
  sess <- make_session()
  on.exit({
    close_session(sess)
    log_message(SCRIPT_NAME, "Chromote session closed.")
  }, add = TRUE)

  # ---------------------------------------------------------------
  # 2. Fetch URLs from the first listing page
  # ---------------------------------------------------------------
  nav_res <- safe_navigate(sess, LISTING_URL)
  sess <- nav_res$session
  if (!nav_res$ok) {
    log_message(SCRIPT_NAME, "Failed to navigate to Chợ Tốt page 1.", "ERROR")
    return(0L)
  }

  # Scroll a few times to trigger lazy loading (same as batch script)
  for (i in seq_len(5)) {
    tryCatch(sess$Runtime$evaluate('window.scrollBy(0, window.innerHeight)'), error = function(e) NULL)
    Sys.sleep(1)
  }

  html_content <- tryCatch(
    sess$Runtime$evaluate('document.documentElement.outerHTML')$result$value,
    error = function(e) NULL)
  if (is.null(html_content)) {
    log_message(SCRIPT_NAME, "Unable to retrieve page HTML.", "ERROR")
    return(0L)
  }

  page_html <- tryCatch(read_html(html_content, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(page_html)) {
    log_message(SCRIPT_NAME, "Failed to parse page HTML.", "ERROR")
    return(0L)
  }

  links <- page_html |>
    html_nodes("a.c15fd2pn") |>
    html_attr("href") |>
    na.omit()

  links <- links[str_detect(links, "\\/\\d+\\.htm")]
  links <- str_replace(links, "#.*$", "")
  page_urls <- unique(paste0(BASE_URL, links))

  total_urls <- length(page_urls)
  log_message(SCRIPT_NAME, sprintf("Found %d URLs on Page 1.", total_urls))

  if (total_urls == 0) {
    log_message(SCRIPT_NAME, "No URLs discovered – exiting.", "WARN")
    return(0L)
  }

  # ---------------------------------------------------------------
  # 3. Delta fetching loop: stop at first duplicate URL
  # ---------------------------------------------------------------
  inserted <- 0L
  pb <- cli_progress_bar(total = total_urls,
                         format = "[{pb_progress}] {pb_current}/{pb_total} URLs – new: {inserted}")

  for (i in seq_along(page_urls)) {
    url <- page_urls[[i]]
    # Check for existence
    dup <- DBI::dbGetQuery(con,
      sprintf("SELECT 1 FROM %s WHERE url = ? LIMIT 1", TABLE_NAME),
      params = list(url))
    if (nrow(dup) > 0) {
      log_message(SCRIPT_NAME, sprintf("Duplicate URL %s encountered – breaking.", url), "INFO")
      break
    }

    # Scrape detail using the batch scraper's `scrape_car()` function
    # Ensure the global session variable used by `scrape_car` points to our session
    assign("b", sess, envir = .GlobalEnv)
    detail <- tryCatch(
      scrape_car(url),
      error = function(e) {
        log_message(SCRIPT_NAME, sprintf("Error scraping %s: %s", url, e$message), "ERROR")
        NULL
      }
    )

    if (!is.null(detail) && nrow(detail) == 1) {
      detail_clean <- standardize_car_data(detail) %>% apply_business_rules()
      if (nrow(detail_clean) != 1) next
      DBI::dbWriteTable(con, TABLE_NAME, detail_clean, append = TRUE, row.names = FALSE)
      inserted <- inserted + 1L
      log_message(SCRIPT_NAME, sprintf("Inserted new listing %s (total %d).", url, inserted))
    }
    cli_progress_update(id = pb, set = i)
  }

  cli_progress_done(pb)
  log_message(SCRIPT_NAME, sprintf("Realtime Chợ Tốt completed – %d new rows inserted.", inserted))
  return(inserted)
}

# Convenience wrapper for interactive testing
if (interactive()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "web_scraping/data/master_data.db")
  run_realtime_chotot(con)
  DBI::dbDisconnect(con)
}
