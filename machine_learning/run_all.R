# ==============================================================================
# Script: run_all.R
# Purpose: Train all ML models from repository master data
# Output: machine_learning/output_models.RData
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("web_scraping/script/utils.R")

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
OUTPUT_FILE <- "machine_learning/output_models.RData"

median_safe <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(default)
  median(x, na.rm = TRUE)
}

normalize_transmission_ml <- function(x) {
  y <- str_to_lower(str_squish(as.character(x)))
  case_when(
    y %in% c("automatic", "auto", "at", "số tự động", "so tu dong", "tự động", "tu dong") ~ "Tự động",
    y %in% c("manual", "mt", "số sàn", "so san", "sàn", "san", "số tay") ~ "Số sàn",
    y == "cvt" ~ "CVT",
    TRUE ~ NA_character_
  )
}

normalize_origin_ml <- function(x) {
  y <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(y, "nhập|nhap|import") ~ "Nhập khẩu",
    str_detect(y, "trong nước|trong nuoc|lắp ráp|lap rap|việt nam|viet nam") ~ "Trong nước",
    TRUE ~ NA_character_
  )
}

df <- read_master_data() %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    price = suppressWarnings(as.numeric(price)),
    mileage = suppressWarnings(as.numeric(mileage)),
    engine_size = suppressWarnings(as.numeric(engine_size)),
    seat_count = suppressWarnings(as.integer(seat_count)),
    transmission = normalize_transmission_ml(transmission),
    origin = normalize_origin_ml(origin)
  ) %>%
  filter(
    !is.na(year), year >= 1990, year <= CURRENT_YEAR,
    !is.na(price), price >= 5e7, price <= 1.5e10,
    !is.na(brand), brand != "",
    !is.na(model), model != ""
  ) %>%
  mutate(
    car_age = CURRENT_YEAR - year,
    price_billion = price / 1e9,
    log_price = log(price),
    mileage_k = mileage / 1000,
    is_auto = as.integer(transmission %in% c("Tự động", "Số tự động", "CVT")),
    is_imported = as.integer(origin == "Nhập khẩu"),
    price_segment = factor(
      case_when(
        price_billion < 0.5 ~ "Phổ thông",
        price_billion < 1.0 ~ "Tầm trung",
        price_billion < 2.5 ~ "Khá",
        TRUE ~ "Cao cấp"
      ),
      levels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp")
    ),
    body_type_clean = case_when(
      body_type %in% c("SUV", "Crossover") ~ "SUV/Crossover",
      body_type == "Sedan" ~ "Sedan",
      body_type %in% c("Hatchback", "Wagon") ~ "Hatchback/Wagon",
      body_type %in% c("Van/Minibus", "Van/Minivan") ~ "Van/Minibus",
      body_type %in% c("Bán tải", "Bán tải / Pickup", "Pickup", "Truck") ~ "Bán tải/Truck",
      TRUE ~ "Khác"
    ),
    cluster_id = NA_integer_,
    cluster_name = NA_character_
  ) %>%
  group_by(body_type_clean) %>%
  mutate(mileage_k = ifelse(is.na(mileage_k), median_safe(mileage_k), mileage_k)) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(engine_size = ifelse(is.na(engine_size), median_safe(engine_size), engine_size)) %>%
  ungroup() %>%
  mutate(
    mileage_k = ifelse(is.na(mileage_k), median_safe(mileage_k, 0), mileage_k),
    engine_size = ifelse(is.na(engine_size), median_safe(engine_size, 1.5), engine_size),
    seat_count = ifelse(is.na(seat_count), round(median_safe(seat_count, 5)), seat_count)
  ) %>%
  filter(
    is.finite(log_price),
    is.finite(car_age),
    is.finite(mileage_k),
    is.finite(engine_size),
    is.finite(seat_count)
  )

if (nrow(df) < 100) {
  stop(sprintf("Not enough valid rows for ML training: %d", nrow(df)))
}

source("machine_learning/model1_regression.R", local = TRUE)
source("machine_learning/model2_clustering.R", local = TRUE)
source("machine_learning/model3_decision_tree.R", local = TRUE)

df_final <- df %>%
  select(
    brand, model, trim, year, car_age,
    body_type, body_type_clean, fuel_type, transmission,
    engine_size, seat_count, drivetrain,
    price, price_billion, price_segment,
    mileage, mileage_k, origin, color, city, posted_date, source, url,
    is_auto, is_imported, cluster_id, cluster_name
  )

summary_stats <- list(
  total_listings = nrow(df_final),
  n_sources = n_distinct(df_final$source),
  n_brands = n_distinct(df_final$brand),
  n_cities = n_distinct(df_final$city),
  price_mean = round(mean(df_final$price_billion, na.rm = TRUE), 3),
  price_median = round(median(df_final$price_billion, na.rm = TRUE), 3),
  price_min = round(min(df_final$price_billion, na.rm = TRUE), 3),
  price_max = round(max(df_final$price_billion, na.rm = TRUE), 3),
  year_range = c(min(df_final$year), max(df_final$year)),
  pct_auto = round(mean(df_final$is_auto, na.rm = TRUE) * 100, 1),
  pct_imported = round(mean(df_final$is_imported, na.rm = TRUE) * 100, 1)
)

source_summary <- df_final %>%
  group_by(source) %>%
  summarise(n_xe = n(), gia_median = round(median(price_billion, na.rm = TRUE), 3), .groups = "drop") %>%
  arrange(desc(n_xe))

brand_summary <- df_final %>%
  group_by(brand) %>%
  summarise(
    n_xe = n(),
    gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3),
    gia_median = round(median(price_billion, na.rm = TRUE), 3),
    km_tb = round(mean(mileage_k, na.rm = TRUE), 1),
    tuoi_tb = round(mean(car_age, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_xe))

body_summary <- df_final %>%
  group_by(body_type_clean) %>%
  summarise(
    n_xe = n(),
    gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3),
    gia_median = round(median(price_billion, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(n_xe))

segment_summary <- df_final %>%
  group_by(price_segment) %>%
  summarise(n_xe = n(), pct = round(n() / nrow(df_final) * 100, 1), .groups = "drop")

city_summary <- df_final %>%
  group_by(city) %>%
  summarise(n_xe = n(), gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3), .groups = "drop") %>%
  arrange(desc(n_xe)) %>%
  slice_head(n = 15)

save(
  df_final, summary_stats, source_summary, brand_summary, body_summary,
  segment_summary, city_summary,
  model_regression, reg_metrics, coef_df, reg_test_result,
  model_kmeans, cluster_profiles_raw, cluster_centers_real,
  cluster_name_map, elbow_df, avg_silhouette, OPTIMAL_K,
  model_tree, tree_accuracy, tree_kappa,
  conf_table, feat_imp, class_metrics,
  file = OUTPUT_FILE
)

cat("Done. ML artifacts saved to:", OUTPUT_FILE, "\n")
