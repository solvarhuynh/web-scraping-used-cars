# Real‑time Processing Rules

> **Purpose:** Define a strict, repeatable process for continuously ingesting *new* car listings in real‑time while guaranteeing data integrity, memory safety, and consistency with the batch pipelines.

---

## 1. Database Architecture

- **Primary store:** SQLite (`RSQLite`) located at `web_scraping/data/master_data.db`.
- **Schema:** Must contain the canonical 18‑column layout defined in `script/utils.R`.
- **Uniqueness:** Column `url` is declared `UNIQUE` and indexed (primary key) to enable O(1) look‑ups.
- **Transactional inserts:** All new rows for a realtime run are wrapped in a single transaction and committed only after the loop finishes or breaks.

```r
con <- dbConnect(RSQLite::SQLite(), "web_scraping/data/master_data.db")

dbExecute(con, """
CREATE TABLE IF NOT EXISTS car_listings (
  brand TEXT, model TEXT, trim TEXT, year INTEGER,
  body_type TEXT, fuel_type TEXT, transmission TEXT,
  engine_size REAL, seat_count INTEGER, drivetrain TEXT,
  price REAL, mileage INTEGER, origin TEXT, color TEXT,
  city TEXT, posted_date DATE, source TEXT,
  url TEXT PRIMARY KEY
);
""")
```

---

## 2. Delta‑Fetching Algorithm (Stop on Duplicate)

1. **Fetch only page 1** of the listing (the site always shows the newest adverts first).
2. **Extract the ordered list of URLs** (newest → oldest).
3. Iterate **top‑down** through the URL vector:
   - Query the SQLite table for the current `url`.
   - **If not found:**
     - Scrape the detail page (using the shared scraper function from `script/scrap/scrap_<site>.R`).
     - Insert the result into `car_listings`.
   - **If found:**
     - Log the duplicate (`INFO`).
     - **Break** the loop for that site – all remaining URLs are older and already stored.
4. After the loop, close the DB connection.

> **Why break?** Encountering the first duplicate tells us the remainder of the page consists of older listings already captured in previous realtime sessions, avoiding unnecessary network traffic and duplicate inserts.

---

## 3. Code Reuse – Shared Extraction Logic

- Scripts in `script/realtime/` **must not duplicate** any HTML parsing, Chromote session handling, or helper utilities.
- Instead, each realtime script **sources** its corresponding batch scraper:
  ```r
  source("web_scraping/script/scrap/scrap_<site>.R")  # pulls in all helper functions
  ```
- The detail‑scraping function (e.g., `scrape_car()` for Chợ Tốt) lives **only** in the batch scraper. Realtime scripts call that function directly, guaranteeing identical field extraction between batch and realtime pipelines.

---

## 4. Session Management (Headless Browser)

- For JavaScript‑heavy sites (**Chợ Tốt**, **Carpla**):
  1. **Open a Chromote session** at the start of the realtime run.
  2. **Reuse the same session** for every URL of that site.
  3. **When a duplicate URL triggers a break** (or when the script finishes normally):
     - Call `close_session(session)` (or the helper defined in the batch scraper) to terminate the browser process and free RAM.
     - Set the session variable to `NULL` to avoid accidental reuse.
- All session lifecycle events are logged via `log_message()` (`INFO` on open, `INFO` on close).
- No lazy or implicit session creation – the script must abort if the browser cannot be launched.

---

## 5. Standard Workflow (Realtime Execution)

```mermaid
flowchart TD
    A[Start script] --> B[Open SQLite connection]
    B --> C[Source batch scraper (helpers)]
    C --> D{Open Chromote?}
    D -->|Yes| E[Create session]
    D -->|No| E[Proceed]
    E --> F[Fetch page 1 URLs]
    F --> G[Iterate URLs (newest→oldest)]
    G --> H{URL exists in DB?}
    H -->|No| I[Scrape detail, INSERT row]
    I --> G
    H -->|Yes| J[Log duplicate, BREAK loop]
    J --> K[Close Chromote session (if any)]
    K --> L[Commit transaction]
    L --> M[Close DB connection]
    M --> N[End script]
```

**Key guarantees:**
- No duplicate rows can ever be inserted because `url` is a primary key.
- Memory usage is bounded: a single browser instance per site, closed immediately on break.
- The script is safe to schedule via Windows Task Scheduler (e.g., every 5 minutes).

---

## 6. Logging & Auditing

- Use the `log_message()` helper from `script/utils.R` for all major actions (session start/stop, URL fetched, duplicate detection, DB insert, errors).
- Log format:
  ```
  [YYYY‑MM‑DD HH:MM:SS] [realtime_<site>.R] - LEVEL: message
  ```
- Errors (failed navigation, parsing issues) are logged at `ERROR` level and cause the script to exit with a non‑zero status code.

---

*This file should reside in `rule/realtime_rule.md` and be referenced by any realtime script in `script/realtime/`. All contributors must adhere to these rules to maintain a reliable real‑time data pipeline.*
