# Data Cleaning Rules

This document defines the rules for cleaning and standardizing the raw scraped data across all sources to ensure consistency.

### 1. General Formatting

-   **Missing Values**: All missing data, empty strings (`""`), or unrecognized values must be replaced with `NA`.
-   **Whitespace**: Trim all leading, trailing, and multiple internal spaces in all string columns.
-   **Capitalization**: The values in the `brand` and `model` columns must be converted to UPPERCASE.
-   **Encoding**: Ensure all text is properly encoded in UTF-8.

### 2. Specific Column Rules

-   **`price`**: Standardize to numeric VND. Convert various formats (e.g., "300.000.000", "300 triệu") into a pure integer (e.g., `300000000`). Remove currency symbols, commas, and dots.
-   **`posted_date`**: Standardize date format to `DD-MM-YYYY`.
-   **`mileage`**: Remove text like "km", commas, or dots, and convert to Integer.
-   **`engine_size`**: Extract the numeric value (Float) and remove suffixes like "L" (e.g., "1.5L" becomes `1.5`).
-   **`year`, `seat_count`**: Ensure these are strictly cast to Integer data types.
-   **`body_type`, `fuel_type`, `transmission`, `drivetrain`**: Standardize to a predefined list of string categories if possible (e.g., convert "Số tự động" to "Tự động").
-   **Units of Measurement**: Standardize any other metrics (e.g., weight to tons, fuel consumption to liters) if they appear, though they must map properly to the defined 18 columns.

### 3. Output Requirements

-   The cleaned data must strictly follow the 18 columns defined in the schema (`scrap_rule.md`).
-   Any column that does not exist in the raw data from a specific website must be added and filled with `NA`.
-   Export the final cleaned file as `data_{website_name}_clean.csv` in the `web_scraping/data/clean/` directory.

### 4. Utilizing `web_scraping/script/utils.R` for Cleaning and Standardization

To maintain consistency and promote code reuse, all cleaning scripts (`clean_{website_name}.R`) must leverage the shared utility functions defined in `script/utils.R`.

-   **Shared Cleaning Functions**: `script/utils.R` will house common cleaning and standardization logic. This includes functions for:
    Individual cleaning scripts should call these shared functions from `web_scraping/script/utils.R`.
    -   Handling missing values and empty strings.
    -   Trimming whitespace.
    -   Standardizing `price`, `mileage`, `engine_size`, `year`, `seat_count` to their correct data types and formats.
    -   Converting `posted_date` formats.
    -   Applying capitalization rules for `brand` and `model`.
    Individual cleaning scripts should call these shared functions.
-   **Website-Specific Adaptations**: While `utils.R` provides general functions, each `clean_{website_name}.R` script will be responsible for applying these functions to its specific `data_{website_name}_raw.csv` and implementing any unique cleaning logic required for that particular dataset.
-   **Schema Enforcement**: `script/utils.R` should define the canonical 18-column data schema. Cleaning scripts must use this definition to ensure that the output `data_{website_name}_clean.csv` strictly adheres to the required columns, adding `NA` for any missing fields.
-   **Logging**: All logging activities (e.g., start/end times, record counts, data quality issues) within cleaning scripts must utilize the logging functions provided by `script/utils.R`.
-   **Directory Helpers**: Functions for managing file paths and directory structures (e.g., locating `data/raw/` and `data/clean/` directories) should also be centralized in `script/utils.R`.

### 5. Data Schema

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
| `posted_date` | Date      | The date the listing was posted. Must handle relative formats (e.g., "4 hours ago", "yesterday") and convert them to an absolute date (`YYYY-MM-DD`). For times like "X hours ago", calculate the date based on the current time. If the date cannot be determined, default to the current date.                         |
| `source`      | String    | The source website (e.g., xe.chotot.com, carpla.vn)     |
| `url`         | String    | Direct URL to the car listing                           |
