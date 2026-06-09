suppressPackageStartupMessages(library(dplyr))

df <- read.csv(
  "D:/data_bonbanh_clean.csv",
  stringsAsFactors = FALSE
)

df <- df %>%
  filter(!is.na(year), year >= 1990, year <= 2026,
         !is.na(price), price >= 5e7, price <= 1.5e10) %>%
  mutate(
    car_age       = 2026 - year,
    price_billion = price / 1e9,
    log_price     = log(price),
    mileage_k     = mileage / 1000,
    is_auto       = as.integer(transmission == "Số tự động"),
    is_imported   = as.integer(origin == "Nhập khẩu"),
    price_segment = factor(
      case_when(
        price / 1e9 <  0.5 ~ "Phổ thông",
        price / 1e9 <  1.0 ~ "Tầm trung",
        price / 1e9 <  2.5 ~ "Khá",
        TRUE               ~ "Cao cấp"
      ),
      levels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp")
    ),
    body_type_clean = case_when(
      body_type %in% c("SUV", "Crossover")          ~ "SUV/Crossover",
      body_type == "Sedan"                           ~ "Sedan",
      body_type %in% c("Hatchback", "Wagon")        ~ "Hatchback/Wagon",
      body_type == "Van/Minivan"                     ~ "Van/Minivan",
      body_type %in% c("Bán tải / Pickup", "Truck") ~ "Bán tải/Truck",
      TRUE                                           ~ "Khác"
    ),
    cluster_id   = NA_integer_,
    cluster_name = NA_character_
  ) %>%
  group_by(body_type) %>%
  mutate(mileage_k = ifelse(is.na(mileage_k), median(mileage_k, na.rm = TRUE), mileage_k)) %>%
  ungroup() %>%
  group_by(brand) %>%
  mutate(engine_size = ifelse(is.na(engine_size), median(engine_size, na.rm = TRUE), engine_size)) %>%
  ungroup() %>%
  mutate(
    mileage_k   = ifelse(is.na(mileage_k),   median(mileage_k,   na.rm = TRUE), mileage_k),
    engine_size = ifelse(is.na(engine_size), median(engine_size, na.rm = TRUE), engine_size)
  )

source("D:/LT R/ML_FINAL/model1_regression.R")
source("D:/LT R/ML_FINAL/model2_clustering.R")
source("D:/LT R/ML_FINAL/model3_decision_tree.R")

df_final <- df %>%
  select(brand, model, trim, year, car_age,
         body_type, body_type_clean, fuel_type, transmission,
         engine_size, seat_count, drivetrain,
         price, price_billion, price_segment,
         mileage, mileage_k, origin, color, city, posted_date,
         is_auto, is_imported, cluster_id, cluster_name)

summary_stats <- list(
  total_listings = nrow(df_final),
  n_brands       = n_distinct(df_final$brand),
  n_cities       = n_distinct(df_final$city),
  price_mean     = round(mean(df_final$price_billion, na.rm = TRUE), 3),
  price_median   = round(median(df_final$price_billion, na.rm = TRUE), 3),
  price_min      = round(min(df_final$price_billion, na.rm = TRUE), 3),
  price_max      = round(max(df_final$price_billion, na.rm = TRUE), 3),
  year_range     = c(min(df_final$year), max(df_final$year)),
  pct_auto       = round(mean(df_final$is_auto) * 100, 1),
  pct_imported   = round(mean(df_final$is_imported) * 100, 1)
)

brand_summary <- df_final %>%
  group_by(brand) %>%
  summarise(n_xe = n(),
            gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3),
            gia_median     = round(median(price_billion, na.rm = TRUE), 3),
            km_tb          = round(mean(mileage_k, na.rm = TRUE), 1),
            tuoi_tb        = round(mean(car_age, na.rm = TRUE), 1),
            .groups = "drop") %>%
  arrange(desc(n_xe))

body_summary <- df_final %>%
  group_by(body_type_clean) %>%
  summarise(n_xe = n(),
            gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3),
            gia_median     = round(median(price_billion, na.rm = TRUE), 3),
            .groups = "drop") %>%
  arrange(desc(n_xe))

segment_summary <- df_final %>%
  group_by(price_segment) %>%
  summarise(n_xe = n(),
            pct  = round(n() / nrow(df_final) * 100, 1),
            .groups = "drop")

city_summary <- df_final %>%
  group_by(city) %>%
  summarise(n_xe = n(),
            gia_trung_binh = round(mean(price_billion, na.rm = TRUE), 3),
            .groups = "drop") %>%
  arrange(desc(n_xe)) %>%
  head(15)

save(
  df_final, summary_stats, brand_summary, body_summary,
  segment_summary, city_summary,
  model_regression, reg_metrics, coef_df, reg_test_result,
  model_kmeans, cluster_profiles_raw, cluster_centers_real,
  cluster_name_map, elbow_df, avg_silhouette, OPTIMAL_K,
  model_tree, tree_accuracy, tree_kappa,
  conf_table, feat_imp, class_metrics,
  file = "D:/LT R/ML_FINAL/output_models.RData"
)

cat("Done. output_models.RData saved.\n")

