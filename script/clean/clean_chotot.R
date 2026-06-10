# Clean Chợ Tốt raw data.
# Output: data/clean/data_chotot_clean.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

source("script/utils.R")

SCRIPT_NAME <- "clean_chotot.R"
INPUT_FILE <- "data/raw/data_chotot_raw.csv"
OUTPUT_FILE <- "data/clean/data_chotot_clean.csv"

# Brand mapping table extracted from the original cleaner (kept for consistency)
CHOTOT_BRAND_MAP <- c(
  "1" = "Kia", "2" = "Toyota", "3" = "Ford", "4" = "Chevrolet", "5" = "Hyundai",
  "6" = "Honda", "7" = "Mazda", "8" = "Audi", "9" = "BMW", "10" = "Daewoo",
  "13" = "Isuzu", "14" = "Jeep", "15" = "Lexus", "16" = "Mercedes-Benz",
  "18" = "Mitsubishi", "19" = "Nissan", "20" = "Peugeot", "21" = "Smart",
  "22" = "Suzuki", "23" = "Volkswagen", "24" = "Jaecoo", "27" = "Asia",
  "32" = "BYD", "35" = "Omoda", "37" = "Citroen", "42" = "Geely", "48" = "Jaguar",
  "51" = "Land Rover", "60" = "MG", "63" = "Porsche", "68" = "Samsung",
  "71" = "Subaru", "76" = "Volvo", "80" = "VinFast", "83" = "Haval",
  "84" = "Skoda", "85" = "Lynk & Co", "87" = "Wuling", "88" = "GAC",
  "92" = "Dongfeng"
)

# Known colour list – used to extract colour from the title
KNOWN_COLORS <- c(
  "Đen", "Trắng", "Bạc", "Đỏ", "Xanh", "Xám", "Vàng", "Nâu", "Ghi",
  "Cam", "Kem", "Đồng", "Hồng", "Tím", "Xanh lá", "Xanh dương", "Xanh lục"
)

clean_chotot <- function() {
  dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
  log_message(SCRIPT_NAME, "Starting Chợ Tốt cleaning.")

  if (!file.exists(INPUT_FILE)) {
    log_message(SCRIPT_NAME, "Input file not found.", "ERROR")
    return(invisible(NULL))
  }

  # Read raw CSV – keep everything as character to simplify cleaning
  raw <- readr::read_csv(INPUT_FILE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))

  if (nrow(raw) == 0) {
    log_message(SCRIPT_NAME, "Raw data is empty.", "WARN")
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------
  # 1. Basic sanitisation & brand mapping
  # ---------------------------------------------------------------------
  df_cleaned <- raw %>%
    mutate(
      title_clean = str_replace_all(trim, "[^[:alnum:]À-ỹ\\s-]", " ") %>% str_squish(),
      mapped_brand = CHOTOT_BRAND_MAP[brand],
      brand = ifelse(!is.na(mapped_brand), mapped_brand, NA_character_)
    ) %>%
    # -------------------------------------------------------------------
    # 2. Extract colour – keep the *last* colour token if multiple appear
    # -------------------------------------------------------------------
    mutate(
      color_regex = str_c("(?i)\\b(", paste(KNOWN_COLORS, collapse = "|"), ")\\b"),
      extracted_colors = str_extract_all(title_clean, color_regex),
      color = map_chr(extracted_colors, ~ if (length(.x) > 0) str_to_title(.x[length(.x)]) else NA_character_)
    ) %>%
    # -------------------------------------------------------------------
    # 3. Remove brand, year, colour and other noise to isolate model/trim
    # -------------------------------------------------------------------
    mutate(
      safe_brand = coalesce(brand, "DUMMY_BRAND"),
      brand_pattern = str_c("(?i)\\b", safe_brand, "\\b"),
      title_filtered = ifelse(is.na(brand), str_to_lower(title_clean), str_replace(str_to_lower(title_clean), brand_pattern, "")),
      title_filtered = str_remove_all(title_filtered, "\\b(19|20)\\d{2}\\b"),
      title_filtered = str_remove_all(title_filtered, str_to_lower(color_regex)),
      title_filtered = str_remove_all(title_filtered, "\\b\\d+[\\.,]?\\d*\\s*(km|vạn)\\b"),
      title_filtered = str_remove_all(title_filtered, "\\b(màu|số tự động|số sàn|at|mt|số|tự động|sàn|chính chủ|siêu lướt|xe đẹp|bao test|bản|lên|full|đẹp|zin|mới)\\b"),
      title_filtered = str_squish(title_filtered)
    ) %>%
    # -------------------------------------------------------------------
    # 4. Heuristic model extraction for Chợ Tốt
    # -------------------------------------------------------------------
    mutate(
      model_raw = case_when(
        str_detect(title_filtered, "^santa\\s*fe") ~ "Santa Fe",
        str_detect(title_filtered, "^land\\s*cruiser") ~ "Land Cruiser",
        str_detect(title_filtered, "^range\\s*rover") ~ "Range Rover",
        str_detect(title_filtered, "^cr[-\\s]*v|^crv") ~ "CR-V",
        TRUE ~ str_extract(title_filtered, "^[a-z0-9-]+")
      ),
      model = ifelse(is.na(model_raw), NA_character_, model_raw)
    ) %>%
    # -------------------------------------------------------------------
    # 5. Trim extraction based on the model placeholder
    # -------------------------------------------------------------------
    mutate(
      safe_model = coalesce(model_raw, "DUMMY_MODEL"),
      model_pattern = str_c("(?i)^", str_replace_all(safe_model, "-", "[- ]?"), "\\b"),
      trim_extracted = ifelse(is.na(model_raw), title_filtered, str_replace(title_filtered, model_pattern, "")),
      trim = str_squish(trim_extracted),
      trim = ifelse(trim == "", NA_character_, str_to_title(trim))
    ) %>%
    # -------------------------------------------------------------------
    # 6. Normalise categorical codes that appear as numbers on Chợ Tốt
    # -------------------------------------------------------------------
    mutate(
      fuel_type = case_when(
        fuel_type == "1" ~ "Xăng",
        fuel_type == "2" ~ "Dầu",
        fuel_type == "3" ~ "Hybrid",
        fuel_type == "4" ~ "Điện",
        TRUE ~ fuel_type
      ),
      transmission = case_when(
        transmission == "1" ~ "Tự động",
        transmission == "2" ~ "Số sàn",
        transmission == "3" ~ "CVT",
        TRUE ~ transmission
      ),
      body_type = case_when(
        body_type == "1" ~ "Sedan",
        body_type == "2" ~ "Coupe",
        body_type == "3" ~ "SUV",
        body_type == "4" ~ "Hatchback",
        body_type == "6" ~ "Bán tải",
        body_type == "7" ~ "Mui trần",
        body_type == "8" ~ "MPV",
        body_type == "9" ~ "Van/Minibus",
        TRUE ~ body_type
      )
    ) %>%
    # Remove helpers
    select(-title_clean, -mapped_brand, -color_regex, -extracted_colors, -safe_brand, -brand_pattern, -title_filtered, -model_raw, -safe_model, -model_pattern, -trim_extracted)

  # Apply the generic schema normaliser (price, mileage, dates, etc.)
  df_final <- standardize_car_data(df_cleaned)

  safe_write_csv(df_final, OUTPUT_FILE)
  log_message(SCRIPT_NAME, sprintf("Finished Chợ Tốt cleaning with %s rows.", nrow(df_final)))
  return(df_final)
}

# Execute when sourced
clean_chotot <- clean_chotot()
