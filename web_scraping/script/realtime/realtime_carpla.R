# Realtime scraper for Carpla (Page 1 only)
# Uses the same helper functions defined in script/scrap/scrap_carpla.R
# Returns the number of newly inserted listings.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(cli)
})

# Load utilities and the batch scraper (contains all helper functions)
source("web_scraping/script/utils.R")
source("web_scraping/script/scrap/scrap_carpla.R")  # defines get_listing_urls(), scrape_detail(), etc.

SCRIPT_NAME <- "realtime_carpla.R"
TABLE_NAME <- "car_listings"
SOURCE_NAME <- "carpla.vn"

#' Run realtime Carpla scraper (page 1)
#' @param con DBI connection to the SQLite database (car_listings must exist)
#' @return Number of newly inserted listings
run_realtime_carpla <- function(con) {
  log_message(SCRIPT_NAME, "Starting realtime Carpla scraper (page 1).")

  # ---------------------------------------------------------------
  # 1. Retrieve URLs from the first page (most recent listings)
  # ---------------------------------------------------------------
  page_urls <- get_listing_urls(1)  # helper from scrap_carpla.R
  total_urls <- length(page_urls)
  log_message(SCRIPT_NAME, sprintf("Found %d URLs on page 1.", total_urls))

  if (total_urls == 0) {
    log_message(SCRIPT_NAME, "No URLs found – exiting.", "WARN")
    return(0L)
  }

  # ---------------------------------------------------------------
  # 2. Initialise a Chromote session (re‑used for every detail page)
  # ---------------------------------------------------------------
  sess <- make_session()
  on.exit({
    close_session(sess)
    log_message(SCRIPT_NAME, "Chromote session closed.")
  }, add = TRUE)

  inserted <- 0L
  pb <- cli_progress_bar(total = total_urls,
                         format = "[{pb_progress}] {pb_current}/{pb_total} URLs – new: {inserted}")

  for (i in seq_along(page_urls)) {
    url <- page_urls[[i]]
    # -----------------------------------------------
    # Delta‑fetching: stop at the first duplicate URL
    # -----------------------------------------------
    exists <- DBI::dbGetQuery(con,
      sprintf("SELECT 1 FROM %s WHERE url = ? LIMIT 1", TABLE_NAME),
      params = list(url))
    if (nrow(exists) > 0) {
      log_message(SCRIPT_NAME, sprintf("Duplicate URL %s encountered – breaking loop.", url), "INFO")
      break
    }

    # ------------------------------------------------
    # 3. Scrape the detail page using the same logic as batch scraper
    # ------------------------------------------------
    # Ensure the global session variable used by scrape_detail() points to `sess`
    assign("b", sess, envir = .GlobalEnv)
    detail <- tryCatch(
      scrape_detail(url),
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
  log_message(SCRIPT_NAME, sprintf("Realtime Carpla completed – %d new rows inserted.", inserted))
  return(inserted)
}

# If run directly (e.g., via `Rscript script/realtime/realtime_carpla.R`),
# open a DB connection for convenience.
if (interactive()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "web_scraping/data/master_data.db")
  run_realtime_carpla(con)
  DBI::dbDisconnect(con)
}
