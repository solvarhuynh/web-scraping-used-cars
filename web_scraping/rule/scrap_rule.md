# Data Scraping Rules

This document outlines the requirements for scraping used car data from various websites using R.

---

### Handling Timeout Errors with Chromote

**Checkpointing Mechanism**: Never run the scraper continuously from the first page to the last in a single go. Save the last successfully scraped page number to a tracking file located in the raw data directory (e.g., `web_scraping/data/raw/meta/checkpoint_{website_name}.txt`). If the process is interrupted, your script must automatically read this file and resume from the last saved page.
    **Logical Workflow for "Resuming" Tasks**:
    1. **Initialization**: When the script runs, it checks the output tracking file (e.g., `data.csv` where the scraped results are stored).
    2. **Read State**: The script reads the maximum page number (`last_page`) that has already been saved in the file.
    3. **Set Starting Point**: The script sets `start_page = last_page + 1`.
    4. **Loop Execution**: The script begins running the loop from `start_page` until the desired total number of pages is reached.

**Sleep/Pause Intervals**: After scraping every 5-10 pages, pause the execution for about 5-10 seconds. This prevents the target servers from blocking your IP as a bot and reduces the load on the headless browser.

**Page Limit**: The maximum number of pages to scrape per session is capped at 500 pages.

---


### 1. Target Websites

    Đối với thư viện chromote - tôi dùng Edge Microsoft

#### 1.1. Chợ Tốt
*   **URL**: https://xe.chotot.com/mua-ban-oto

*   **Website** Type: Dynamic Website (JavaScript-rendered)

*   **Method**: Browser Automation / Dynamic Web Scraping

*   **Recommended R Library**: RSelenium hoặc chromote

*   **Reasoning**: The entire vehicle listing and detailed content on Chợ Tốt are dynamically rendered using JavaScript (React/Next.js) after the browser finishes loading the initial source code. You are required to use RSelenium to initialize a headless virtual browser (such as Chrome or Firefox in the background) and instruct it to automatically scroll down the page to trigger the loading of additional listings, waiting until the user interface is fully rendered before proceeding with the HTML parsing.

#### 1.2. Carpla
*   **URL:** `https://carpla.vn/mua-xe`
*   **Website Type:** Dynamic Website (JavaScript-rendered)
*   **Method:** Browser Automation / Dynamic Web Scraping
*   **Recommended R Library:** `RSelenium` or `chromote`
*   **Reasoning:** This site loads data asynchronously via JavaScript. Fetching pure HTML will result in an empty page with no listings. You must use `RSelenium` or `chromote` to control a headless browser, wait for the UI to fully load, and then scrape the rendered HTML.

#### 1.3. Bán Xe Hơi Cũ
*   **URL:** `https://banxehoicu.vn/ban-oto-cu`
*   **Website Type:** Static Website (Traditional HTML)
*   **Method:** HTML Parsing
*   **Recommended R Library:** `rvest` and `xml2`
*   **Reasoning:** The interface and car data are embedded directly in the source HTML when the server responds. You can use `rvest::read_html()` and CSS selectors to extract vehicle information rapidly without needing a virtual browser.

---

### 2. Data Schema

Each scraped record must be structured into a data frame with the following 18 columns.

| Column Name   | Data Type | Description                                             |
|---------------|-----------|---------------------------------------------------------|
| `brand`       | String    | Car brand (e.g., Toyota, Mercedes, Kia)                 |
| `model`       | String    | Car model name (e.g., Vios, Carnival, Raize)            |
| `trim`        | String    | Specific version/trim of the model (e.g., G, Luxury)    |
| `year`        | Integer   | Year of manufacture                                     |
| `body_type`   | String    | Body style (e.g., Sedan, SUV, Crossover, Minivan)       |
| `fuel_type`   | String    | Fuel type (e.g., Petrol, Diesel, Hybrid)                |
| `transmission`| String    | Transmission type (e.g., Automatic, Manual, CVT)        |
| `engine_size` | Float     | Engine displacement in liters (e.g., 1.5, 2.0)          |
| `seat_count`  | Integer   | Number of seats                                         |
| `drivetrain`  | String    | Drive system (e.g., FWD, RWD, AWD, 4WD)                 |
| `price`       | Integer   | Current selling price in VND                            |
| `mileage`     | Integer   | Odometer reading in kilometers                          |
| `origin`      | String    | Origin of the car ("Trong nước" - Domestic, "Nhập khẩu" - Imported) |
| `color`       | String    | Exterior color                                          |
| `city`        | String    | Province/City where the car is sold                     |
| `posted_date` | Date      | The date the listing was posted |
| `source`      | String    | The source website (e.g., xe.chotot.com, carpla.vn)     |
| `url`         | String    | Direct URL to the car listing                           |

---

### 3. Scraping Rules & Process

1.  **File & Directory Structure**:
    -   Each website must have its own scraping script: `script/scrap/scrap_{website_name}.R`.
    -   The output must be a CSV file saved to the `web_scraping/data/raw/` directory.
    -   The output filename must be: `data_{website_name}_raw.csv` (e.g., `data_chotot_raw.csv`).

2.  **Data Handling**:
    -   If a value for a specific field cannot be found, it must be set to `NA`.
    -   Ensure all text data is handled with UTF-8 encoding.
    -   Do not include any icons or special characters in the data.

3.  **Scraping Logic**:
    -   **Step A (Listing Page)**: From the main listing page, collect all URLs leading to individual car posts.
    -   **Step B (Detail Page)**: Iterate through the collected list of URLs. For each URL, visit the page and scrape the 18 required data fields.
    -   **Step C (Pagination)**: After processing all URLs on the current page, proceed to the next page (or trigger the "Load More" button) and repeat from Step A.


### General Rule: Logging

-   **File**: All process notifications and events must be appended to `web_scraping/log.txt`.
-   **Format**: Each log entry should be timestamped and include the script name and a descriptive message (e.g., `[YYYY-MM-DD HH:MM:SS] [scrap_chotot.R] - INFO: Successfully scraped 520 records.`).
