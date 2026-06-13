suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(plotly)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(rpart)
})

# -------------------------------------------------------------------------
# HCMUTE AutoInsight - Shiny port of auto-insight-hub-main
# -------------------------------------------------------------------------

BRAND_FALLBACK <- c(
  "Toyota", "Hyundai", "Mazda", "Kia", "Honda", "Ford",
  "Mitsubishi", "VinFast", "Mercedes-Benz", "BMW"
)
REGIONS <- c(
  "Hà Nội", "Tp Hồ Chí Minh", "Đà Nẵng", "Hải Phòng",
  "Cần Thơ", "Bình Dương", "Đồng Nai"
)
FUELS <- c("Xăng", "Dầu", "Hybrid", "Điện")
TRANSMISSIONS <- c("Tự động", "Số sàn", "CVT")
CONDITIONS <- c("Như mới", "Tốt", "Trung bình")
ORIGINS <- c("Trong nước", "Nhập khẩu")
CHART_COLORS <- c("#0f78c7", "#22b07d", "#e9a826", "#cf3f36", "#7657d6", "#45b8df")
CURRENT_YEAR <- max(2026, as.integer(format(Sys.Date(), "%Y")))

first_existing_file <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit)) hit[1] else NULL
}

DATA_FILE_CANDIDATES <- c(
  file.path("web_scraping", "data", "master_data.csv")
)
MODEL_FILE_CANDIDATES <- c(
  file.path("machine_learning", "output_models.RData"),
  file.path("Model", "output_models.RData"),
  "output_models.RData"
)

DATA_FILE <- first_existing_file(DATA_FILE_CANDIDATES)
MODEL_FILE <- first_existing_file(MODEL_FILE_CANDIDATES)
DATA_SOURCE_LABEL <- if (!is.null(DATA_FILE)) DATA_FILE else "chưa tìm thấy CSV"
MODEL_SOURCE_LABEL <- if (!is.null(MODEL_FILE)) MODEL_FILE else "chưa tìm thấy output_models.RData"

num <- function(x, digits = 0) {
  out <- format(round(x, digits), big.mark = ".", decimal.mark = ",", trim = TRUE, nsmall = ifelse(digits > 0, digits, 0), scientific = FALSE)
  out[is.na(x) | is.nan(x)] <- "0"
  out
}

format_vnd <- function(value) {
  out <- ifelse(
    value >= 1e9,
    paste0(format(round(value / 1e9, 2), decimal.mark = ".", nsmall = 2, trim = TRUE), " tỷ"),
    ifelse(value >= 1e6, paste0(num(value / 1e6, 0), " tr"), paste0(num(value, 0), " ₫"))
  )
  out[is.na(value) | is.nan(value)] <- "0 ₫"
  out
}

format_km <- function(value) paste0(num(value, 0), " km")

format_metric <- function(value, digits = 3, suffix = "") {
  value <- suppressWarnings(as.numeric(value))
  if (!length(value) || !is.finite(value[1])) return("N/A")
  paste0(num(value[1], digits), suffix)
}

get_price_ticks <- function(max_val) {
  if (is.null(max_val) || is.na(max_val) || max_val <= 0) {
    return(list(vals = c(0), labels = c("0 ₫")))
  }
  step <- if (max_val > 2e9) {
    5e8
  } else if (max_val > 1e9) {
    2e8
  } else if (max_val > 5e8) {
    1e8
  } else {
    5e7
  }
  vals <- seq(0, max_val + step, by = step)
  labels <- format_vnd(vals)
  list(vals = vals, labels = labels)
}

clean_transmission <- function(x) {
  y <- tolower(trimws(as.character(x)))
  case_when(
    y %in% c("automatic", "auto", "at", "số tự động", "so tu dong", "tự động", "tu dong") ~ "Tự động",
    y %in% c("cvt") ~ "CVT",
    y %in% c("manual", "mt", "robot", "số sàn", "so san", "sàn", "san") ~ "Số sàn",
    TRUE ~ "Tự động"
  )
}

clean_fuel <- function(x) {
  y <- tolower(trimws(as.character(x)))
  case_when(
    y %in% c("petrol", "gasoline", "xăng", "xang") ~ "Xăng",
    y %in% c("diesel", "dầu", "dau") ~ "Dầu",
    y == "hybrid" ~ "Hybrid",
    y %in% c("electric", "điện", "dien") ~ "Điện",
    TRUE ~ "Xăng"
  )
}

clean_origin <- function(x) {
  y <- tolower(trimws(as.character(x)))
  ifelse(grepl("nhập|nhap|import", y), "Nhập khẩu", "Trong nước")
}

median_safe <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(0)
  median(x)
}

mean_safe <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(0)
  mean(x)
}

q_safe <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(0)
  as.numeric(quantile(x, p, names = FALSE, type = 7))
}

ml_artifacts <- new.env(parent = emptyenv())
if (!is.null(MODEL_FILE) && file.exists(MODEL_FILE)) {
  load(MODEL_FILE, envir = ml_artifacts)
}

prepare_bonbanh_data <- function(raw) {
  raw %>%
    mutate(
      year = suppressWarnings(as.integer(year)),
      price = suppressWarnings(as.numeric(price)),
      mileage = suppressWarnings(as.numeric(mileage)),
      engine_size = suppressWarnings(as.numeric(engine_size)),
      seat_count = suppressWarnings(as.numeric(seat_count))
    ) %>%
    filter(!is.na(year), year >= 1990, year <= CURRENT_YEAR, !is.na(price), price >= 5e7, price <= 1.5e10) %>%
    mutate(
      car_age = CURRENT_YEAR - year,
      price_billion = price / 1e9,
      log_price = log(price),
      mileage_k = mileage / 1000,
      transmission = clean_transmission(transmission),
      fuel_type = clean_fuel(fuel_type),
      origin = clean_origin(origin),
      is_auto = as.integer(transmission %in% c("Tự động", "CVT")),
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
      mileage_k = ifelse(is.na(mileage_k), median_safe(mileage_k), mileage_k),
      engine_size = ifelse(is.na(engine_size), median_safe(engine_size), engine_size),
      seat_count = ifelse(is.na(seat_count), median_safe(seat_count), seat_count)
    )
}

if (exists("df_final", envir = ml_artifacts)) {
  source_data <- get("df_final", envir = ml_artifacts)
  APP_DATA_SOURCE_LABEL <- paste0(MODEL_SOURCE_LABEL, "::df_final")
} else {
  if (is.null(DATA_FILE) || !file.exists(DATA_FILE)) stop("Không tìm thấy file dữ liệu sạch trong thư mục data.")
  raw_data <- read.csv(DATA_FILE, stringsAsFactors = FALSE, check.names = TRUE)
  names(raw_data)[1] <- sub("^\ufeff", "", names(raw_data)[1])
  source_data <- prepare_bonbanh_data(raw_data)
  APP_DATA_SOURCE_LABEL <- DATA_SOURCE_LABEL
}

data_clean <- source_data %>%
  mutate(
    id = paste0("CAR-", sprintf("%04d", row_number())),
    brand = trimws(as.character(brand)),
    model = trimws(as.character(model)),
    version = ifelse(is.na(trim) | trim == "" | trim == "NA", "Tiêu chuẩn", as.character(trim)),
    year = as.integer(year),
    price = round(as.numeric(price)),
    price_billion = as.numeric(price_billion),
    km = ifelse(is.na(mileage), as.numeric(mileage_k) * 1000, as.numeric(mileage)),
    mileage_k = as.numeric(mileage_k),
    engine_size = as.numeric(engine_size),
    seat_count = as.integer(round(as.numeric(seat_count))),
    transmission = clean_transmission(transmission),
    fuel = clean_fuel(fuel_type),
    origin = clean_origin(origin),
    region = ifelse(is.na(city) | city == "", "Không rõ", trimws(as.character(city))),
    age = CURRENT_YEAR - year,
    is_auto = as.integer(transmission %in% c("Tự động", "CVT")),
    is_imported = as.integer(origin == "Nhập khẩu"),
    price_segment = factor(price_segment, levels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp")),
    condition = case_when(
      age <= 2 & km <= 45000 ~ "Như mới",
      age <= 8 & km <= 150000 ~ "Tốt",
      TRUE ~ "Trung bình"
    )
  ) %>%
  filter(
    !is.na(brand), brand != "", !is.na(model), model != "", !is.na(year),
    !is.na(price), !is.na(km), !is.na(engine_size), !is.na(seat_count),
    year >= 1990, year <= CURRENT_YEAR, price > 0, km >= 0
  ) %>%
  select(
    id, brand, model, version, year, price, price_billion, km, mileage_k,
    transmission, fuel, origin, region, condition, age, engine_size, seat_count,
    price_segment, cluster_id, cluster_name, body_type, body_type_clean
  )

if (!nrow(data_clean)) stop(paste0(DATA_FILE, " không có dữ liệu hợp lệ cho dashboard."))

BRANDS <- sort(unique(data_clean$brand))
YEARS <- seq(min(data_clean$year, na.rm = TRUE), max(data_clean$year, na.rm = TRUE))
REGIONS <- sort(unique(data_clean$region))
FUELS <- sort(unique(data_clean$fuel))
TRANSMISSIONS <- sort(unique(data_clean$transmission))
PRICE_MAX <- ceiling(max(data_clean$price, na.rm = TRUE) / 1e8) * 1e8
KM_MAX <- ceiling(max(data_clean$km, na.rm = TRUE) / 10000) * 10000
DEFAULT_BRAND <- if ("TOYOTA" %in% BRANDS) "TOYOTA" else BRANDS[1]
DEFAULT_REGION <- if ("Tp Hồ Chí Minh" %in% REGIONS) "Tp Hồ Chí Minh" else if ("TP HCM" %in% REGIONS) "TP HCM" else if ("Hà Nội" %in% REGIONS) "Hà Nội" else REGIONS[1]
DEFAULT_YEAR <- min(2022, max(YEARS))

models_for_brand <- function(brand) {
  source <- data_clean
  if (is.null(brand) || !nzchar(brand) || brand == "all") {
    return(source %>% count(model, sort = TRUE) %>% arrange(desc(n), model) %>% pull(model))
  }
  models <- source %>%
    filter(.data$brand == .env$brand) %>%
    count(model, sort = TRUE) %>%
    arrange(desc(n), model) %>%
    pull(model)
  if (!length(models)) source %>% count(model, sort = TRUE) %>% arrange(desc(n), model) %>% pull(model) else models
}

top_one <- function(df, col) {
  if (!nrow(df)) return("—")
  df %>% count(.data[[col]], sort = TRUE) %>% slice_head(n = 1) %>% pull(1)
}

kpis <- function(df = data_clean) {
  total <- nrow(df)
  auto <- sum(df$transmission %in% c("Tự động", "CVT"), na.rm = TRUE)
  list(
    total = total,
    medianPrice = median_safe(df$price),
    meanKm = mean_safe(df$km),
    topBrand = top_one(df, "brand"),
    topRegion = top_one(df, "region"),
    automaticRatio = if (total) auto / total else 0
  )
}

artifact <- function(name, default = NULL) {
  if (exists(name, envir = ml_artifacts)) get(name, envir = ml_artifacts) else default
}

model_available <- function(name) {
  exists(name, envir = ml_artifacts)
}

median_lookup <- function(brand, model, col, default = 0) {
  exact <- data_clean %>% filter(.data$brand == !!brand, .data$model == !!model)
  if (nrow(exact)) return(median_safe(exact[[col]]))
  by_brand <- data_clean %>% filter(.data$brand == !!brand)
  if (nrow(by_brand)) return(median_safe(by_brand[[col]]))
  val <- median_safe(data_clean[[col]])
  if (val == 0) default else val
}

nearest_cluster <- function(price_billion, form) {
  centers <- artifact("cluster_centers_real")
  if (is.null(centers) || !nrow(centers)) return(list(id = NA_integer_, name = "Chưa xác định"))
  features <- c(
    price_billion = price_billion,
    car_age = CURRENT_YEAR - form$year,
    mileage_k = form$km / 1000,
    engine_size = form$engine_size
  )
  center_mat <- as.matrix(centers[, names(features)])
  scale_vec <- apply(center_mat, 2, sd, na.rm = TRUE)
  scale_vec[!is.finite(scale_vec) | scale_vec == 0] <- 1
  diff_mat <- center_mat - matrix(rep(as.numeric(features), each = nrow(center_mat)), nrow = nrow(center_mat))
  dist <- rowSums((diff_mat / matrix(rep(scale_vec, each = nrow(center_mat)), nrow = nrow(center_mat))) ^ 2, na.rm = TRUE)
  idx <- which.min(dist)
  list(id = centers$cluster[idx], name = centers$ten_cum[idx])
}

estimate_price <- function(form) {
  similar <- data_clean %>%
    filter(
      .data$brand == form$brand,
      .data$model == form$model,
      abs(.data$year - form$year) <= 2
    )
  model_regression <- artifact("model_regression")
  pred_input <- data.frame(
    car_age = max(0, CURRENT_YEAR - form$year),
    mileage_k = max(0, form$km / 1000),
    engine_size = form$engine_size,
    is_auto = as.integer(form$transmission %in% c("Tự động", "CVT")),
    is_imported = as.integer(form$origin == "Nhập khẩu"),
    seat_count = form$seat_count
  )

  if (!is.null(model_regression)) {
    point <- as.numeric(exp(predict(model_regression, newdata = pred_input)))
    source <- "Linear Regression ML"
  } else {
    fallback <- data_clean %>% filter(.data$brand == form$brand)
    base <- if (nrow(similar)) {
      median_safe(similar$price)
    } else if (nrow(fallback)) {
      median_safe(fallback$price) * (0.92 ^ max(0, CURRENT_YEAR - form$year))
    } else {
      median_safe(data_clean$price)
    }
    km_adj <- 1 - min(0.25, (form$km / 200000) * 0.25)
    trans_adj <- if (form$transmission %in% c("Tự động", "CVT")) 1.02 else 0.97
    point <- base * km_adj * trans_adj
    source <- "Heuristic fallback"
  }

  condition_adj <- if (form$condition == "Như mới") 1.04 else if (form$condition == "Tốt") 1 else 0.94
  region_adj <- if (form$region %in% c("Tp Hồ Chí Minh", "TP HCM", "TP. Hồ Chí Minh", "Hà Nội")) 1.015 else 1
  point <- max(5e7, point * condition_adj * region_adj)

  reg_metrics <- artifact("reg_metrics", list(r_squared = 0.55, rmse_billion = 0.7))
  rmse <- as.numeric(reg_metrics$rmse_billion %||% 0.7) * 1e9
  similar_iqr <- if (nrow(similar) >= 6) IQR(similar$price, na.rm = TRUE) / 2 else NA_real_
  spread_abs <- max(point * ifelse(nrow(similar) >= 10, 0.10, 0.16), min(rmse, point * 0.35), ifelse(is.finite(similar_iqr), similar_iqr, 0))
  model_tree <- artifact("model_tree")
  segment <- if (!is.null(model_tree)) {
    as.character(predict(model_tree, newdata = pred_input, type = "class"))
  } else {
    as.character(cut(point / 1e9, c(-Inf, 0.5, 1, 2.5, Inf), labels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp")))
  }
  cluster <- nearest_cluster(point / 1e9, form)
  confidence <- min(0.94, max(0.35, as.numeric(reg_metrics$r_squared %||% 0.55) + min(nrow(similar), 40) / 160))

  list(
    point = point,
    low = max(0, point - spread_abs),
    high = point + spread_abs,
    confidence = confidence,
    sampleSize = nrow(similar),
    source = source,
    segment = segment,
    clusterName = cluster$name
  )
}

representative_car <- function(brand, model, year) {
  if (is.null(brand) || is.null(model) || is.na(year) || !nzchar(brand) || !nzchar(model)) return(NULL)
  matches <- data_clean %>%
    filter(.data$brand == !!brand, .data$model == !!model, .data$year == !!as.integer(year))
  if (!nrow(matches)) {
    matches <- data_clean %>%
      filter(.data$brand == !!brand, .data$model == !!model, abs(.data$year - !!as.integer(year)) <= 1)
  }
  if (!nrow(matches)) return(NULL)
  matches <- matches %>% arrange(price)
  matches[ceiling(nrow(matches) / 2), , drop = FALSE]
}

score_of <- function(car) {
  age <- CURRENT_YEAR - car$year
  gia <- max(0, min(100, 100 - (car$price / 1800000000) * 100))
  do_moi <- max(0, min(100, 100 - age * 7))
  odo <- max(0, min(100, 100 - (car$km / 200000) * 100))
  thanh_khoan <- c(
    TOYOTA = 92, HYUNDAI = 84, HONDA = 82, KIA = 78, MAZDA = 75, FORD = 70,
    MITSUBISHI = 68, VINFAST = 65, `MERCEDES-BENZ` = 60, BMW = 58
  )
  brand_key <- toupper(as.character(car$brand))
  liquidity <- if (brand_key %in% names(thanh_khoan)) thanh_khoan[[brand_key]] else 64
  fuel_eff <- if (car$fuel == "Điện") 95 else if (car$fuel == "Hybrid") 88 else if (car$fuel == "Xăng") 72 else 65
  c("Giá" = round(gia), "Độ mới" = round(do_moi), "Odo thấp" = round(odo), "Thanh khoản" = liquidity, "Tiết kiệm nhiên liệu" = fuel_eff)
}

region_stats <- function(df = data_clean) {
  df %>%
    group_by(region) %>%
    summarise(
      count = n(),
      medianPrice = median_safe(price),
      topBrand = names(sort(table(brand), decreasing = TRUE))[1],
      .groups = "drop"
    ) %>%
    mutate(velocity = round(40 + (count / max(sum(count), 1)) * 320)) %>%
    arrange(desc(count))
}

price_by_year <- function(df) {
  df %>%
    group_by(year) %>%
    summarise(median = median_safe(price), mean = mean_safe(price), .groups = "drop") %>%
    arrange(year)
}

top_brands <- function(df, n = 8) {
  df %>% count(brand, sort = TRUE) %>% slice_head(n = n)
}

breakdown <- function(df, col) {
  df %>% count(.data[[col]], sort = TRUE) %>% rename(name = 1, value = n)
}

# -------------------------------------------------------------------------
# UI helpers
# -------------------------------------------------------------------------

icon_svg <- function(name) {
  paths <- list(
    layout = '<rect x="3" y="3" width="7" height="9" rx="1"/><rect x="14" y="3" width="7" height="5" rx="1"/><rect x="14" y="12" width="7" height="9" rx="1"/><rect x="3" y="16" width="7" height="5" rx="1"/>',
    chart = '<path d="M3 3v18h18"/><path d="M7 14l4-4 3 3 5-7"/>',
    map = '<path d="M20 10c0 6-8 12-8 12S4 16 4 10a8 8 0 1 1 16 0Z"/><circle cx="12" cy="10" r="3"/>',
    scale = '<path d="m16 16 3-8 3 8c-.9.7-1.9 1-3 1s-2.1-.3-3-1Z"/><path d="m2 16 3-8 3 8c-.9.7-1.9 1-3 1s-2.1-.3-3-1Z"/><path d="M7 21h10"/><path d="M12 3v18"/><path d="M3 7h18"/>',
    calculator = '<rect x="4" y="2" width="16" height="20" rx="2"/><path d="M8 6h8"/><path d="M8 10h.01"/><path d="M12 10h.01"/><path d="M16 10h.01"/><path d="M8 14h.01"/><path d="M12 14h.01"/><path d="M16 14h.01"/><path d="M8 18h.01"/><path d="M12 18h.01"/><path d="M16 18h.01"/>',
    table = '<path d="M12 3v18"/><rect x="3" y="4" width="18" height="16" rx="2"/><path d="M3 10h18"/><path d="M3 16h18"/>',
    report = '<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8Z"/><path d="M14 2v6h6"/><path d="M8 13h8"/><path d="M8 17h5"/>',
    gauge = '<path d="m12 14 4-4"/><path d="M3.34 19a10 10 0 1 1 17.32 0"/><path d="M8 19h8"/>',
    search = '<circle cx="11" cy="11" r="8"/><path d="m21 21-4.3-4.3"/>',
    download = '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><path d="M7 10l5 5 5-5"/><path d="M12 15V3"/>',
    refresh = '<path d="M3 12a9 9 0 0 1 15-6.7L21 8"/><path d="M21 3v5h-5"/><path d="M21 12a9 9 0 0 1-15 6.7L3 16"/><path d="M3 21v-5h5"/>',
    database = '<ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v14c0 1.7 4 3 9 3s9-1.3 9-3V5"/><path d="M3 12c0 1.7 4 3 9 3s9-1.3 9-3"/>',
    car = '<path d="M19 17h2c.6 0 1-.4 1-1v-3c0-.9-.7-1.7-1.5-1.9L18.4 6c-.3-.6-.9-1-1.6-1H7.2c-.7 0-1.3.4-1.6 1l-2.1 5.1C2.7 11.3 2 12.1 2 13v3c0 .6.4 1 1 1h2"/><circle cx="7" cy="17" r="2"/><circle cx="17" cy="17" r="2"/><path d="M5 11h14"/>',
    dollar = '<circle cx="12" cy="12" r="10"/><path d="M16 8h-6a2 2 0 0 0 0 4h4a2 2 0 0 1 0 4H8"/><path d="M12 18V6"/>',
    trophy = '<path d="M6 9H4.5a2.5 2.5 0 0 1 0-5H6"/><path d="M18 9h1.5a2.5 2.5 0 0 0 0-5H18"/><path d="M4 22h16"/><path d="M10 14.7V17c0 .6-.4 1-1 1h6c-.6 0-1-.4-1-1v-2.3"/><path d="M18 2H6v7a6 6 0 0 0 12 0V2Z"/>',
    cog = '<path d="M12 20a8 8 0 1 0 0-16 8 8 0 0 0 0 16Z"/><path d="M12 14a2 2 0 1 0 0-4 2 2 0 0 0 0 4Z"/>',
    filter = '<path d="M22 3H2l8 9.5V20l4 2v-9.5L22 3Z"/>',
    activity = '<path d="M22 12h-4l-3 9L9 3l-3 9H2"/>',
    fuel = '<path d="M3 22V5a2 2 0 0 1 2-2h7a2 2 0 0 1 2 2v17"/><path d="M14 13h2a2 2 0 0 1 2 2v2a2 2 0 0 0 4 0V9.8a2 2 0 0 0-.6-1.4L18 5"/><path d="M5 14h7"/>',
    trend = '<path d="M3 17l6-6 4 4 8-8"/><path d="M14 7h7v7"/>',
    sparkles = '<path d="M12 3l1.7 4.3L18 9l-4.3 1.7L12 15l-1.7-4.3L6 9l4.3-1.7L12 3Z"/><path d="M19 15l.9 2.1L22 18l-2.1.9L19 21l-.9-2.1L16 18l2.1-.9L19 15Z"/>',
    file = '<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8Z"/><path d="M14 2v6h6"/>',
    copy = '<rect x="9" y="9" width="13" height="13" rx="2"/><rect x="2" y="2" width="13" height="13" rx="2"/>',
    lightbulb = '<path d="M15 14c.2-1 .7-1.7 1.5-2.5A5 5 0 1 0 7.5 11.5C8.3 12.3 8.8 13 9 14"/><path d="M9 18h6"/><path d="M10 22h4"/>',
    plus = '<path d="M5 12h14"/><path d="M12 5v14"/>',
    x = '<path d="M18 6 6 18"/><path d="m6 6 12 12"/>',
    up = '<path d="M7 17 17 7"/><path d="M7 7h10v10"/>',
    down = '<path d="m7 7 10 10"/><path d="M17 7v10H7"/>'
  )
  HTML(sprintf('<svg class="ui-icon-svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.1" stroke-linecap="round" stroke-linejoin="round">%s</svg>', paths[[name]] %||% paths$car))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

nav_item <- function(tab, title, icon, subtitle) {
  tags$a(
    href = "#", class = "sidebar-item", `data-tab` = tab, `data-title` = title, `data-subtitle` = subtitle,
    span(class = "sidebar-icon", icon_svg(icon)), span(title)
  )
}

car_silhouette <- function(class = "car-silhouette") {
  tags$svg(
    viewBox = "0 0 200 80", class = class, fill = "currentColor", `aria-hidden` = "true",
    tags$path(d = "M10 56c0-4 2-7 6-8l20-3 18-19c3-3 7-5 11-5h58c4 0 8 2 11 5l16 18 32 5c5 1 8 4 8 8v8c0 3-2 5-5 5h-12a14 14 0 0 0-28 0H62a14 14 0 0 0-28 0H15c-3 0-5-2-5-5v-9z"),
    tags$circle(cx = "48", cy = "64", r = "9", fill = "#0e1f35"),
    tags$circle(cx = "48", cy = "64", r = "3", fill = "white", opacity = ".5"),
    tags$circle(cx = "148", cy = "64", r = "9", fill = "#0e1f35"),
    tags$circle(cx = "148", cy = "64", r = "3", fill = "white", opacity = ".5")
  )
}

gauge_arc <- function(value, label, caption, size = 180) {
  clamped <- max(0, min(1, value))
  radius <- (size - 24) / 2
  cx <- size / 2
  cy <- size / 2 + 8
  start <- pi
  end <- 0
  angle <- start + (end - start) * clamped
  x0 <- cx + radius * cos(start)
  y0 <- cy + radius * sin(start)
  x1 <- cx + radius * cos(end)
  y1 <- cy + radius * sin(end)
  xe <- cx + radius * cos(angle)
  ye <- cy + radius * sin(angle)
  large <- ifelse(clamped > 0.5, 1, 0)
  ticks <- lapply(0:10, function(i) {
    t <- i / 10
    a <- start + (end - start) * t
    r1 <- radius + 10
    r2 <- radius + 16
    tags$line(
      x1 = cx + r1 * cos(a), y1 = cy + r1 * sin(a),
      x2 = cx + r2 * cos(a), y2 = cy + r2 * sin(a),
      stroke = "rgba(255,255,255,.55)", `stroke-width` = ifelse(i %% 5 == 0, 2, 1)
    )
  })
  div(
    class = "gauge-wrap",
    tags$svg(
      width = size, height = size / 2 + 28, viewBox = paste(0, 0, size, size / 2 + 28),
      tags$defs(tags$linearGradient(id = paste0("gauge-grad-", size, "-", round(value * 100)), x1 = "0%", y1 = "0%", x2 = "100%", y2 = "0%",
        tags$stop(offset = "0%", `stop-color` = "#5dd4e9"),
        tags$stop(offset = "60%", `stop-color` = "#0f78c7"),
        tags$stop(offset = "100%", `stop-color` = "#173b70")
      )),
      tags$path(d = sprintf("M %s %s A %s %s 0 1 1 %s %s", x0, y0, radius, radius, x1, y1), fill = "none", stroke = "rgba(255,255,255,.18)", `stroke-width` = 14, `stroke-linecap` = "round"),
      tags$path(d = sprintf("M %s %s A %s %s 0 %s 1 %s %s", x0, y0, radius, radius, large, xe, ye), fill = "none", stroke = paste0("url(#gauge-grad-", size, "-", round(value * 100), ")"), `stroke-width` = 14, `stroke-linecap` = "round"),
      ticks,
      tags$circle(cx = cx, cy = cy, r = 6, fill = "white"),
      tags$line(x1 = cx, y1 = cy, x2 = cx + (radius - 6) * cos(angle), y2 = cy + (radius - 6) * sin(angle), stroke = "white", `stroke-width` = 3, `stroke-linecap` = "round")
    ),
    div(class = "gauge-label", label),
    div(class = "gauge-caption", caption)
  )
}

kpi_card <- function(label, value_id, icon, accent = "ocean", hint = NULL, trend = NULL, direction = "up") {
  div(
    class = "kpi-card",
    div(class = "kpi-glow"),
    div(
      class = "kpi-top",
      div(
        class = "kpi-copy",
        div(class = "kpi-label", label),
        div(class = "kpi-value", textOutput(value_id, inline = TRUE)),
        if (!is.null(hint)) div(class = "kpi-hint", hint)
      ),
      div(class = paste("kpi-icon", accent), icon_svg(icon))
    ),
    if (!is.null(trend)) {
      div(class = "kpi-trend", span(class = paste("trend-icon", direction), icon_svg(ifelse(direction == "down", "down", "up"))), trend)
    }
  )
}

section_card <- function(title, description = NULL, icon = NULL, ..., class = "", body_class = "", actions = NULL) {
  tags$section(
    class = paste("section-card", class),
    tags$header(
      class = "section-header",
      div(
        class = "section-title-wrap",
        if (!is.null(icon)) span(class = "section-icon", icon_svg(icon)),
        div(
          class = "section-copy",
          h3(title),
          if (!is.null(description)) p(description)
        )
      ),
      if (!is.null(actions)) div(class = "section-actions", actions)
    ),
    div(class = paste("section-body", body_class), ...)
  )
}

field <- function(label, control) div(class = "field", tags$label(label), control)

select_field <- function(id, choices, selected = NULL) {
  selectInput(id, NULL, choices = choices, selected = selected, width = "100%")
}

filter_select <- function(label, id, choices, selected = NULL) {
  field(label, select_field(id, choices, selected))
}

download_button <- function(id, label, icon = "download", class = "btn-outline") {
  downloadButton(id, label = tagList(span(class = "btn-ico", icon_svg(icon)), label), class = paste("btn", class))
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("HCMUTE AutoInsight — Used Car Market Intelligence"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Space+Grotesk:wght@500;600;700&display=swap"),
    tags$style(HTML("
      :root {
        --radius: 0.875rem;
        --background: #f4f8fb;
        --foreground: #20314f;
        --card: #ffffff;
        --primary: #0f78c7;
        --primary-foreground: #fbfdff;
        --secondary: #edf5fb;
        --muted-foreground: #647087;
        --border: #dce8f2;
        --navy: #173b70;
        --ocean: #0f78c7;
        --cyan: #5dd4e9;
        --success: #22b07d;
        --warning: #e9a826;
        --destructive: #cf3f36;
        --gradient-ocean: linear-gradient(135deg, #173b70, #0f78c7 55%, #5dd4e9);
        --gradient-surface: linear-gradient(180deg, #fbfdff, #eef6fc);
        --gradient-glass: linear-gradient(140deg, rgba(255,255,255,.85), rgba(238,246,252,.65));
        --shadow-elevated: 0 10px 30px -12px rgba(25,89,155,.24), 0 2px 6px -2px rgba(25,89,155,.12);
        --shadow-soft: 0 4px 16px -6px rgba(25,89,155,.15);
      }
      * { box-sizing: border-box; }
      html, body { margin: 0; background: var(--background); color: var(--foreground); font-family: Inter, system-ui, sans-serif; font-feature-settings: 'cv11','ss01'; -webkit-font-smoothing: antialiased; }
      .container-fluid { padding: 0; }
      h1, h2, h3, h4, .font-display { font-family: 'Space Grotesk', Inter, system-ui, sans-serif; letter-spacing: -0.01em; }
      .ui-icon-svg { width: 1em; height: 1em; }
      .app-root { display: flex; min-height: 100vh; width: 100%; background: var(--background); }
      .sidebar { position: sticky; top: 0; width: 270px; min-height: 100vh; flex: 0 0 270px; border-right: 1px solid var(--border); background: rgba(255,255,255,.92); }
      .sidebar-head { display: flex; align-items: center; gap: 12px; min-height: 65px; padding: 12px 14px; border-bottom: 1px solid var(--border); }
      .brand-mark { position: relative; display: grid; place-items: center; width: 40px; height: 40px; flex: 0 0 auto; border-radius: 14px; background: var(--gradient-ocean); color: #fff; box-shadow: var(--shadow-soft); }
      .brand-mark:after { content: ''; position: absolute; left: 50%; bottom: -2px; width: 24px; height: 4px; transform: translateX(-50%); border-radius: 999px; background: rgba(93,212,233,.7); }
      .brand-title { max-width: 178px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-family: 'Space Grotesk'; font-size: 16px; font-weight: 700; color: var(--navy); }
      .brand-subtitle { margin-top: 1px; max-width: 178px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 11px; text-transform: uppercase; letter-spacing: .08em; color: var(--muted-foreground); }
      .sidebar-content { padding: 12px; }
      .sidebar-item { display: flex; align-items: center; gap: 12px; min-height: 38px; margin: 2px 0; padding: 8px 12px; border-radius: 10px; color: #344863; font-size: 14px; font-weight: 500; text-decoration: none; transition: background .15s, color .15s; }
      .sidebar-item:hover, .sidebar-item.active { background: rgba(15,120,199,.10); color: var(--primary); text-decoration: none; font-weight: 650; }
      .sidebar-icon { display: grid; place-items: center; width: 16px; height: 16px; color: currentColor; }
      .main { min-width: 0; flex: 1; display: flex; flex-direction: column; }
      .app-header { position: sticky; top: 0; z-index: 30; border-bottom: 1px solid var(--border); background: rgba(244,248,251,.86); backdrop-filter: blur(12px); }
      .header-inner { display: flex; align-items: center; gap: 12px; height: 64px; padding: 0 24px; }
      .menu-trigger { display: none; width: 36px; height: 36px; border: 1px solid var(--border); border-radius: 10px; background: var(--card); color: var(--muted-foreground); flex-direction: column; align-items: center; justify-content: center; gap: 4px; }
      .menu-trigger span, .menu-trigger:before, .menu-trigger:after { content: ''; display: block; width: 16px; height: 2px; border-radius: 999px; background: currentColor; }
      .header-title { min-width: 0; display: none; }
      @media (min-width: 640px) { .header-title { display: block; } }
      .header-title h1 { margin: 0; max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 18px; line-height: 1.1; font-weight: 650; color: var(--navy); }
      .header-title p { margin: 2px 0 0; max-width: 360px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 12px; color: var(--muted-foreground); }
      .header-actions { margin-left: auto; display: flex; align-items: center; justify-content: flex-end; gap: 10px; }
      .searchbox { position: relative; display: none; width: 320px; }
      @media (min-width: 880px) { .searchbox { display: block; } }
      .searchbox svg { position: absolute; left: 12px; top: 50%; transform: translateY(-50%); width: 16px; height: 16px; color: var(--muted-foreground); pointer-events: none; }
      .searchbox input { width: 100%; height: 36px; border: 1px solid var(--border); border-radius: 14px; background: rgba(237,245,251,.62); padding: 0 12px 0 36px; outline: none; font-size: 14px; }
      .badge-demo { display: inline-flex; align-items: center; gap: 6px; height: 28px; padding: 0 10px; border: 1px solid rgba(233,168,38,.4); border-radius: 999px; background: rgba(233,168,38,.10); color: rgba(32,49,79,.88); font-size: 11px; font-weight: 650; white-space: nowrap; }
      .badge-demo svg { color: var(--warning); width: 12px; height: 12px; }
      .btn, .btn-default, .btn-outline, .btn-primary, .download-button { display: inline-flex !important; align-items: center; justify-content: center; gap: 6px; min-height: 36px; border-radius: 14px !important; padding: 0 12px !important; border: 1px solid var(--border) !important; background: var(--card) !important; color: rgba(32,49,79,.84) !important; font-size: 13px !important; font-weight: 600 !important; box-shadow: none !important; text-decoration: none !important; }
      .btn-primary { border-color: var(--primary) !important; background: var(--primary) !important; color: var(--primary-foreground) !important; box-shadow: var(--shadow-soft) !important; }
      .btn-ghost { border-color: transparent !important; background: transparent !important; }
      .btn-icon { width: 32px; min-height: 32px; padding: 0 !important; }
      .btn-ico svg { width: 16px; height: 16px; }
      .content { flex: 1; padding: 24px 32px 32px; }
      .page { display: none; }
      .page.active { display: block; }
      .space-y > * + * { margin-top: 24px; }
      .hero { position: relative; overflow: hidden; border-radius: 28px; background: var(--gradient-ocean); padding: 24px; color: #fff; box-shadow: var(--shadow-elevated); }
      @media (min-width: 1024px) { .hero { padding: 32px; } }
      .hero-grid { position: relative; display: grid; gap: 24px; }
      @media (min-width: 1024px) { .hero-grid { grid-template-columns: 1fr auto; align-items: end; } }
      .hero-kicker { display: inline-flex; align-items: center; gap: 8px; border-radius: 999px; background: rgba(255,255,255,.15); padding: 4px 12px; font-size: 11px; font-weight: 700; letter-spacing: .08em; text-transform: uppercase; color: rgba(255,255,255,.9); backdrop-filter: blur(8px); }
      .hero-kicker:before { content: ''; width: 6px; height: 6px; border-radius: 999px; background: var(--cyan); }
      .hero h2 { margin: 12px 0 0; max-width: 740px; font-size: 28px; line-height: 1.15; font-weight: 650; }
      @media (min-width: 1024px) { .hero h2 { font-size: 34px; } }
      .hero p { margin: 12px 0 0; max-width: 640px; color: rgba(255,255,255,.85); font-size: 15px; line-height: 1.55; }
      .hero-pills { margin-top: 20px; display: flex; flex-wrap: wrap; gap: 8px; color: rgba(255,255,255,.8); font-size: 11px; letter-spacing: .08em; text-transform: uppercase; }
      .hero-pills span { border: 1px solid rgba(255,255,255,.25); border-radius: 999px; padding: 4px 10px; }
      .road-lane { position: absolute; inset: auto 0 0 0; height: 64px; opacity: .25; background-image: repeating-linear-gradient(90deg, transparent 0, transparent 28px, rgba(255,255,255,.5) 28px, rgba(255,255,255,.5) 48px); }
      .car-silhouette { position: absolute; right: 24px; bottom: -6px; width: 320px; height: 128px; color: rgba(255,255,255,.18); }
      .gauge-wrap { display: inline-flex; flex-direction: column; align-items: center; }
      .gauge-label { margin-top: 4px; font-family: 'Space Grotesk'; font-size: 22px; font-weight: 650; color: #fff; text-align: center; }
      .gauge-caption { font-size: 11px; letter-spacing: .08em; text-transform: uppercase; color: rgba(255,255,255,.7); text-align: center; }
      .grid { display: grid; gap: 16px; }
      .grid-kpi { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      @media (min-width: 768px) { .grid-kpi { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
      @media (min-width: 1280px) { .grid-kpi { grid-template-columns: repeat(4, minmax(0, 1fr)); } }
      .kpi-card { position: relative; overflow: hidden; border: 1px solid var(--border); border-radius: 20px; background: var(--card); padding: 20px; box-shadow: var(--shadow-soft); transition: box-shadow .15s; }
      .kpi-card:hover { box-shadow: var(--shadow-elevated); }
      .kpi-glow { position: absolute; right: -32px; top: -32px; width: 112px; height: 112px; border-radius: 999px; background: var(--gradient-glass); opacity: .5; filter: blur(20px); pointer-events: none; }
      .kpi-top { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; }
      .kpi-copy { min-width: 0; }
      .kpi-label { font-size: 12px; font-weight: 500; text-transform: uppercase; letter-spacing: .08em; color: var(--muted-foreground); }
      .kpi-value { margin-top: 8px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-family: 'Space Grotesk'; font-size: 24px; line-height: 1.1; font-weight: 650; color: var(--navy); }
      .kpi-hint { margin-top: 4px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 12px; color: var(--muted-foreground); }
      .kpi-icon { display: grid; place-items: center; width: 44px; height: 44px; flex: 0 0 auto; border-radius: 14px; background: linear-gradient(135deg, rgba(15,120,199,.15), rgba(93,212,233,.15)); color: var(--primary); }
      .kpi-icon.navy { background: linear-gradient(135deg, rgba(23,59,112,.15), rgba(15,120,199,.1)); color: var(--navy); }
      .kpi-icon.cyan { background: linear-gradient(135deg, rgba(93,212,233,.25), rgba(15,120,199,.1)); color: var(--primary); }
      .kpi-icon.success { background: linear-gradient(135deg, rgba(34,176,125,.2), rgba(93,212,233,.15)); color: var(--success); }
      .kpi-icon svg { width: 20px; height: 20px; }
      .kpi-trend { margin-top: 12px; display: inline-flex; align-items: center; gap: 4px; border-radius: 999px; background: rgba(237,245,251,.7); padding: 2px 8px; font-size: 11px; font-weight: 500; color: rgba(32,49,79,.82); }
      .trend-icon svg { width: 12px; height: 12px; }
      .trend-icon.up { color: var(--success); }
      .trend-icon.down { color: var(--destructive); }
      .section-card { border: 1px solid var(--border); border-radius: 20px; background: var(--card); box-shadow: var(--shadow-soft); overflow: hidden; }
      .section-header { display: flex; align-items: flex-start; justify-content: space-between; gap: 12px; border-bottom: 1px solid rgba(220,232,242,.75); padding: 16px 20px; }
      .section-title-wrap { display: flex; align-items: center; gap: 10px; min-width: 0; }
      .section-icon { display: grid; place-items: center; width: 32px; height: 32px; flex: 0 0 auto; border-radius: 10px; background: rgba(15,120,199,.10); color: var(--primary); }
      .section-icon svg { width: 16px; height: 16px; }
      .section-copy { min-width: 0; }
      .section-copy h3 { margin: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 14px; font-weight: 650; color: var(--navy); }
      .section-copy p { margin: 2px 0 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 12px; color: var(--muted-foreground); }
      .section-body { padding: 20px; }
      .section-body.flush { padding: 0; }
      .market-layout { display: grid; gap: 24px; }
      @media (min-width: 1280px) { .market-layout { grid-template-columns: 300px 1fr; } .filter-sticky { position: sticky; top: 80px; align-self: start; } }
      .field { margin-bottom: 16px; }
      .field > label, .control-label { display: block; margin: 0 0 6px; font-size: 12px; font-weight: 500; color: rgba(32,49,79,.82); }
      .form-control, .selectize-input { min-height: 36px !important; border: 1px solid var(--border) !important; border-radius: 10px !important; background: rgba(237,245,251,.55) !important; box-shadow: none !important; color: var(--foreground) !important; font-size: 14px !important; }
      .selectize-dropdown { border-color: var(--border); border-radius: 10px; box-shadow: var(--shadow-elevated); }
      .filter-grid-2, .form-grid-2 { display: grid; grid-template-columns: repeat(2, minmax(0,1fr)); gap: 12px; }
      .slider-box { margin-bottom: 16px; }
      .slider-head { display: flex; align-items: center; justify-content: space-between; gap: 8px; margin-bottom: 6px; }
      .slider-head label { margin: 0; font-size: 12px; font-weight: 500; color: rgba(32,49,79,.82); }
      .slider-head span { font-size: 11px; color: var(--muted-foreground); }
      .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--primary) !important; border-color: var(--primary) !important; }
      .chart-h { height: 300px; }
      .chart-h-sm { height: 260px; }
      .chart-grid-2 { display: grid; gap: 16px; }
      @media (min-width: 1024px) { .chart-grid-2 { grid-template-columns: repeat(2, minmax(0,1fr)); } .chart-grid-3 { grid-template-columns: repeat(3, minmax(0,1fr)); } .lg-span-2 { grid-column: span 2 / span 2; } }
      .chart-grid-3 { display: grid; gap: 16px; }
      .plotly-wrap,
      .plotly-output,
      .gg-output,
      .plotly-output > .plotly,
      .plotly-output > .html-widget-output { width: 100%; min-width: 0; }
      .plotly-output .main-svg,
      .plotly-output .svg-container svg { max-width: none; }
      .empty-state { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 12px; padding: 64px 20px; text-align: center; }
      .empty-icon { display: grid; place-items: center; width: 56px; height: 56px; border-radius: 999px; background: var(--secondary); color: var(--muted-foreground); }
      .region-layout { display: grid; gap: 16px; }
      @media (min-width: 1024px) { .region-layout { grid-template-columns: 1fr 1.4fr; } }
      .region-map { position: relative; width: 100%; aspect-ratio: 2/3; overflow: hidden; border-radius: 14px; background: var(--gradient-surface); }
      .region-cards { display: grid; gap: 12px; }
      @media (min-width: 640px) { .region-cards { grid-template-columns: repeat(2, minmax(0,1fr)); } }
      .region-card { border: 1px solid var(--border); border-radius: 20px; background: var(--card); padding: 16px; box-shadow: var(--shadow-soft); transition: box-shadow .15s; }
      .region-card:hover { box-shadow: var(--shadow-elevated); }
      .region-chip { display: inline-flex; border-radius: 999px; background: rgba(15,120,199,.1); padding: 2px 8px; font-size: 11px; font-weight: 650; color: var(--primary); }
      .stat-grid { display: grid; grid-template-columns: repeat(2, minmax(0,1fr)); gap: 8px; margin-top: 12px; font-size: 12px; }
      .stat-grid dt { color: var(--muted-foreground); }
      .stat-grid dd { margin: 2px 0 0; font-weight: 650; color: var(--navy); }
      .progress { width: 100%; height: 6px; overflow: hidden; border-radius: 999px; background: var(--secondary); }
      .progress > span { display: block; height: 100%; border-radius: 999px; background: var(--gradient-ocean); }
      .plain-table { width: 100%; border-collapse: collapse; font-size: 14px; }
      .plain-table th { padding: 12px 20px; border-bottom: 1px solid var(--border); background: rgba(237,245,251,.45); text-align: left; font-size: 11px; text-transform: uppercase; letter-spacing: .08em; color: var(--muted-foreground); }
      .plain-table td { padding: 12px 20px; border-bottom: 1px solid rgba(220,232,242,.65); }
      .plain-table tr:hover td { background: rgba(237,245,251,.45); }
      .compare-grid { display: grid; gap: 16px; }
      @media (min-width: 768px) { .compare-grid { grid-template-columns: repeat(2, minmax(0,1fr)); } }
      @media (min-width: 1280px) { .compare-grid { grid-template-columns: repeat(3, minmax(0,1fr)); } }
      .compare-card { position: relative; overflow: hidden; border: 1px solid var(--border); border-radius: 20px; background: var(--card); box-shadow: var(--shadow-soft); }
      .compare-line { height: 6px; width: 100%; }
      .compare-body { padding: 20px; }
      .slot-badge { display: inline-flex; border-radius: 999px; padding: 2px 10px; font-size: 11px; font-weight: 650; color: #fff; text-transform: uppercase; letter-spacing: .08em; }
      .spec-grid { display: flex; flex-direction: column; gap: 10px; border-radius: 14px; background: rgba(237,245,251,.45); padding: 14px; font-size: 12.5px; }
      .spec-grid > div { display: flex; justify-content: space-between; align-items: center; border-bottom: 1px dashed rgba(220,232,242,.75); padding-bottom: 8px; }
      .spec-grid > div:last-child { border-bottom: none; padding-bottom: 0; }
      .spec-grid dt { color: var(--muted-foreground); margin: 0; font-weight: 500; text-align: left; }
      .spec-grid dd { margin: 0; font-weight: 650; color: var(--navy); text-align: right; }
      .estimate-layout { display: grid; gap: 24px; }
      @media (min-width: 1024px) { .estimate-layout { grid-template-columns: 1fr 1.1fr; } }
      .demo-banner { display: inline-flex; align-items: center; gap: 8px; border: 1px solid rgba(233,168,38,.4); border-radius: 999px; background: rgba(233,168,38,.10); padding: 8px 12px; color: rgba(32,49,79,.84); font-size: 12px; font-weight: 650; }
      .mt-actions { margin-top: 20px; display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }
      .sample-note { margin-left: auto; align-self: center; color: var(--muted-foreground); font-size: 12px; }
      .estimate-hero { position: relative; overflow: hidden; border-radius: 20px; background: var(--gradient-ocean); padding: 24px; color: #fff; box-shadow: var(--shadow-elevated); }
      .estimate-hero .road-lane { height: 48px; }
      .estimate-grid { position: relative; display: grid; gap: 24px; }
      @media (min-width: 640px) { .estimate-grid { grid-template-columns: 1fr auto; align-items: center; } }
      .estimate-label { font-size: 11px; letter-spacing: .08em; text-transform: uppercase; color: rgba(255,255,255,.75); }
      .estimate-price { margin-top: 4px; font-family: 'Space Grotesk'; font-size: 34px; line-height: 1.1; font-weight: 650; color: #fff; }
      .estimate-range { margin-top: 8px; display: flex; flex-wrap: wrap; gap: 10px; color: rgba(255,255,255,.85); font-size: 12px; }
      .estimate-range span { display: inline-flex; align-items: center; gap: 4px; border-radius: 999px; background: rgba(255,255,255,.15); padding: 4px 10px; }
      .feature-row { margin-bottom: 12px; }
      .feature-head { display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px; font-size: 12px; color: rgba(32,49,79,.86); }
      .feature-track { height: 8px; width: 100%; overflow: hidden; border-radius: 999px; background: var(--secondary); }
      .feature-fill { height: 100%; border-radius: 999px; background: var(--gradient-ocean); }
      .insight-list { margin: 0; padding: 0; list-style: none; }
      .insight-list li { display: flex; gap: 8px; margin: 8px 0; color: rgba(32,49,79,.85); font-size: 14px; line-height: 1.45; }
      .insight-list li:before { content: ''; width: 6px; height: 6px; margin-top: 7px; flex: 0 0 auto; border-radius: 999px; background: var(--primary); }
      .data-toolbar { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; border-bottom: 1px solid var(--border); background: rgba(237,245,251,.35); padding: 12px 16px; }
      .toolbar-search { position: relative; min-width: 220px; flex: 1 1 260px; }
      .toolbar-search svg { position: absolute; left: 12px; top: 50%; transform: translateY(-50%); width: 16px; height: 16px; color: var(--muted-foreground); }
      .toolbar-search input { width: 100%; height: 36px; border: 1px solid var(--border); border-radius: 10px; background: var(--card); padding: 0 10px 0 36px; outline: none; }
      .table-wrapper { overflow-x: auto; }
      .pill-select { display: flex; align-items: center; gap: 6px; }
      .pill-select > span { font-size: 11px; text-transform: uppercase; letter-spacing: .08em; color: var(--muted-foreground); font-weight: 500; }
      .pill-select .form-group { margin: 0; }
      .pill-select .selectize-input { min-width: 120px !important; min-height: 32px !important; background: var(--card) !important; font-size: 12px !important; }
      .report-hero { display: flex; flex-wrap: wrap; align-items: flex-start; justify-content: space-between; gap: 12px; border: 1px solid var(--border); border-radius: 20px; background: var(--card); padding: 20px; box-shadow: var(--shadow-soft); }
      .report-badge { display: inline-flex; align-items: center; gap: 6px; border-radius: 999px; background: rgba(15,120,199,.10); padding: 4px 10px; color: var(--primary); font-size: 11px; font-weight: 650; text-transform: uppercase; letter-spacing: .08em; }
      .report-hero h2 { margin: 8px 0 0; font-size: 22px; font-weight: 650; color: var(--navy); }
      .report-hero p { margin: 4px 0 0; color: var(--muted-foreground); font-size: 14px; }
      .report-actions { display: flex; flex-wrap: wrap; gap: 8px; }
      .insight-item { display: flex; gap: 12px; border-radius: 14px; background: rgba(237,245,251,.45); padding: 12px; color: rgba(32,49,79,.86); font-size: 14px; }
      .insight-num { display: grid; place-items: center; width: 24px; height: 24px; flex: 0 0 auto; border-radius: 999px; background: rgba(15,120,199,.15); color: var(--primary); font-size: 11px; font-weight: 650; }
      .dataTables_wrapper { padding: 14px 16px; font-size: 13px; }
      table.dataTable { border-collapse: collapse !important; width: 100% !important; }
      table.dataTable thead th { border-bottom: 1px solid var(--border) !important; background: var(--background); color: var(--muted-foreground); font-size: 11px; text-transform: uppercase; letter-spacing: .08em; }
      table.dataTable tbody td { border-top: 1px solid rgba(220,232,242,.65); color: var(--foreground); vertical-align: middle; }
      table.dataTable.display tbody tr:hover, table.dataTable.hover tbody tr:hover { background: rgba(15,120,199,.05) !important; }
      .toast-lite { position: fixed; right: 18px; top: 84px; z-index: 1000; display: none; max-width: 320px; border: 1px solid var(--border); border-radius: 14px; background: var(--card); box-shadow: var(--shadow-elevated); padding: 12px 14px; color: var(--foreground); font-size: 13px; }
      .toast-lite.show { display: block; animation: toastIn .18s ease; }
      @keyframes toastIn { from { transform: translateY(-6px); opacity: 0; } to { transform: translateY(0); opacity: 1; } }
      @media (max-width: 920px) {
        .app-root { display: block; }
        .sidebar { position: fixed; left: 0; top: 0; z-index: 60; transform: translateX(-100%); transition: transform .2s ease; box-shadow: 20px 0 35px rgba(23,59,112,.18); }
        body.sidebar-open .sidebar { transform: translateX(0); }
        .menu-trigger { display: flex; }
        .content { padding: 18px 14px; }
        .header-inner { padding: 0 14px; }
        .hide-sm { display: none !important; }
      }
      @media print {
        .sidebar, .app-header, .report-actions, .btn { display: none !important; }
        .app-root { display: block; }
        .content { padding: 0; }
        .page { display: block !important; }
      }
    ")),
    tags$script(HTML("
      $(function() {
        function toast(message) {
          var el = $('#toast-lite');
          el.text(message).addClass('show');
          clearTimeout(window.__toastTimer);
          window.__toastTimer = setTimeout(function(){ el.removeClass('show'); }, 2200);
        }
        function resizePlots() {
          setTimeout(function() {
            $('.js-plotly-plot').each(function() {
              if (window.Plotly) Plotly.Plots.resize(this);
            });
            window.dispatchEvent(new Event('resize'));
          }, 180);
        }
        function setTab(tab, title, subtitle) {
          $('.page.active').removeClass('active').trigger('hidden');
          $('#' + tab).addClass('active').trigger('shown');
          $('.sidebar-item').removeClass('active');
          $('.sidebar-item[data-tab=\"' + tab + '\"]').addClass('active');
          $('#header-title').text(title || $('.sidebar-item[data-tab=\"' + tab + '\"]').data('title'));
          $('#header-subtitle').text(subtitle || $('.sidebar-item[data-tab=\"' + tab + '\"]').data('subtitle'));
          $('body').removeClass('sidebar-open');
          if (window.Shiny) Shiny.setInputValue('active_tab', tab, {priority: 'event'});
          var tabHash = '#tab=' + tab;
          if (window.history && window.location.hash !== tabHash) {
            window.history.replaceState(null, '', tabHash);
          }
          $(document).trigger('shiny:visualchange');
          resizePlots();
          window.scrollTo(0, 0);
          setTimeout(function(){ window.scrollTo(0, 0); }, 80);
          setTimeout(function(){ window.scrollTo(0, 0); }, 400);
        }
        function activateFromHash() {
          var tab = (window.location.hash || '#tab=overview').replace(/^#/, '');
          if (tab.indexOf('tab=') === 0) tab = tab.slice(4);
          var item = $('.sidebar-item[data-tab=\"' + tab + '\"]');
          if (!item.length) item = $('.sidebar-item[data-tab=\"overview\"]');
          setTab(item.data('tab'), item.data('title'), item.data('subtitle'));
        }
        function syncActiveTab() {
          var tab = $('.page.active').attr('id') || 'overview';
          if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('active_tab', tab, {priority: 'event'});
          resizePlots();
        }
        $('.sidebar-item').on('click', function(e) {
          e.preventDefault();
          setTab($(this).data('tab'), $(this).data('title'), $(this).data('subtitle'));
        });
        $(window).on('hashchange', activateFromHash);
        $('.menu-trigger').on('click', function(){ $('body').toggleClass('sidebar-open'); resizePlots(); });
        $('#refresh-demo').on('click', function(){ toast('Dữ liệu bonbanh đã sẵn sàng'); });
        $('#export-demo').on('click', function(){ toast('Mở báo cáo tổng hợp'); setTab('report','Báo cáo','Tổng hợp nhận xét tự động'); });
        $('#copy-insights').on('click', function(){
          var text = $('#insight-copy-source').text();
          if (navigator.clipboard) navigator.clipboard.writeText(text);
          toast('Đã sao chép nhận xét vào clipboard');
        });
        activateFromHash();
        $(document).on('shiny:connected', syncActiveTab);
        setTimeout(syncActiveTab, 300);
        setTimeout(syncActiveTab, 1200);
        setTimeout(syncActiveTab, 2500);
        $(document).on('shiny:value shiny:bound', resizePlots);
      });
    ")),
    tags$style(HTML(".plotly-output { display: block; } .gg-output { display: none; }"))
  ),
  div(id = "toast-lite", class = "toast-lite"),
  div(
    class = "app-root",
    tags$aside(
      class = "sidebar",
      div(
        class = "sidebar-head",
        div(class = "brand-mark", icon_svg("gauge")),
        div(div(class = "brand-title", "HCMUTE AutoInsight"), div(class = "brand-subtitle", "Thị trường xe cũ"))
      ),
      div(
        class = "sidebar-content",
        nav_item("overview", "Tổng quan", "layout", "Bức tranh thị trường xe cũ Việt Nam"),
        nav_item("market", "Phân tích thị trường", "chart", "Khám phá tương quan giữa giá, hãng và odo"),
        nav_item("visuals", "Trực quan hóa", "activity", "Các biểu đồ tổng hợp từ dữ liệu sạch"),
        nav_item("regions", "Bản đồ khu vực", "map", "So sánh hoạt động giao dịch theo vùng"),
        nav_item("compare", "So sánh xe", "scale", "Đặt 2-3 cấu hình cạnh nhau để cân nhắc"),
        nav_item("estimate", "Dự toán giá", "calculator", "Ước tính giá tham khảo cho một cấu hình xe"),
        nav_item("models", "Mô hình ML", "cog", "Hiệu năng hồi quy, cây quyết định và K-Means"),
        nav_item("data", "Dữ liệu bonbanh", "table", "Duyệt nhanh bộ dữ liệu đã làm sạch"),
        nav_item("report", "Báo cáo", "report", "Tổng hợp nhận xét tự động")
      )
    ),
    div(
      class = "main",
      tags$header(
        class = "app-header",
        div(
          class = "header-inner",
          tags$button(class = "menu-trigger", type = "button", span()),
          div(class = "header-title", h1(id = "header-title", "Tổng quan"), p(id = "header-subtitle", "Bức tranh thị trường xe cũ Việt Nam")),
          div(
            class = "header-actions",
            div(class = "searchbox", icon_svg("search"), tags$input(type = "text", placeholder = "Tìm hãng xe, dòng xe…")),
            div(class = "badge-demo", icon_svg("database"), "Bonbanh data + ML"),
            tags$button(id = "export-demo", class = "btn btn-outline hide-sm", type = "button", span(class = "btn-ico", icon_svg("download")), "Xuất báo cáo"),
            tags$button(id = "refresh-demo", class = "btn btn-primary", type = "button", span(class = "btn-ico", icon_svg("refresh")), "Cập nhật dữ liệu")
          )
        )
      ),
      tags$main(
        class = "content",
        tags$section(
          id = "overview", class = "page active space-y",
          div(
            class = "hero",
            car_silhouette(),
            div(class = "road-lane"),
            div(
              class = "hero-grid",
              div(
                class = "hero-copy",
                span(class = "hero-kicker", "HCMUTE R Project"),
                h2("Hệ Thống Phân Tích & Dự Toán Thị Trường Ô Tô Cũ Việt Nam"),
                p("Dashboard phân tích dữ liệu xe cũ, trực quan hóa xu hướng thị trường và hỗ trợ dự toán giá tham khảo cho người mua, người bán và nhà nghiên cứu."),
                div(class = "hero-pills", span(textOutput("hero_total", inline = TRUE)), span(textOutput("hero_regions", inline = TRUE)), span("Tích hợp mô hình ML"))
              ),
              div(class = "hide-sm", uiOutput("overview_gauge"))
            )
          ),
          div(
            class = "grid grid-kpi",
            kpi_card("Tổng số mẫu xe", "kpi_total", "car", "ocean", trend = "Dữ liệu đã lọc và chuẩn hóa"),
            kpi_card("Giá trung vị", "kpi_median", "dollar", "navy", trend = "Theo dữ liệu bonbanh"),
            kpi_card("Hãng phổ biến nhất", "kpi_top_brand", "trophy", "success", hint = "Theo số lượng tin"),
            kpi_card("Khu vực sôi động nhất", "kpi_top_region", "map", "ocean", hint = "Lượng tin nhiều nhất")
          ),
          div(
            class = "chart-grid-3",
            section_card("Xu hướng giá theo năm sản xuất", "Trung vị và trung bình theo từng năm", "trend",
              div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("overview_price_year", height = "260px")), div(class='gg-output', plotOutput("overview_price_year_gg", height = "260px")))
              , class = "lg-span-2"),
            section_card("Cơ cấu nhiên liệu", "Tỷ trọng theo loại nhiên liệu", NULL,
              div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("overview_fuel", height = "260px")), div(class='gg-output', plotOutput("overview_fuel_gg", height = "260px"))))
          ),
          section_card("Top hãng theo số lượng tin", "8 hãng có lượng tin lớn nhất trong bộ dữ liệu", NULL,
            div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("overview_top_brands", height = "280px")), div(class='gg-output', plotOutput("overview_top_brands_gg", height = "280px"))))
        ),
        tags$section(
          id = "market", class = "page",
          div(
            class = "market-layout",
            div(
              class = "filter-sticky",
              section_card(
                "Bộ lọc", textOutput("market_filter_desc", inline = TRUE), "filter",
                actions = actionButton("market_reset", "Đặt lại", class = "btn btn-ghost"),
                filter_select("Hãng xe", "market_brand", c("Tất cả hãng" = "all", BRANDS), "all"),
                field("Dòng xe", selectizeInput("market_model", NULL, choices = c("Tất cả dòng" = "all"), selected = "all", width = "100%")),
                div(class = "filter-grid-2", filter_select("Từ năm", "market_year_from", setNames(YEARS, YEARS), min(YEARS)), filter_select("Đến năm", "market_year_to", setNames(YEARS, YEARS), max(YEARS))),
                div(class = "slider-box", div(class = "slider-head", tags$label("Khoảng giá"), span(textOutput("market_price_label", inline = TRUE))), sliderInput("market_price", NULL, min = 0, max = PRICE_MAX, value = c(0, PRICE_MAX), step = 50000000, ticks = FALSE, sep = ".")),
                div(class = "slider-box", div(class = "slider-head", tags$label("Số km đã đi"), span(textOutput("market_km_label", inline = TRUE))), sliderInput("market_km", NULL, min = 0, max = KM_MAX, value = c(0, KM_MAX), step = 5000, ticks = FALSE, sep = ".")),
                filter_select("Nhiên liệu", "market_fuel", c("Tất cả" = "all", FUELS), "all"),
                filter_select("Hộp số", "market_transmission", c("Tất cả" = "all", TRANSMISSIONS), "all"),
                filter_select("Khu vực", "market_region", c("Toàn quốc" = "all", REGIONS), "all")
              )
            ),
            div(
              class = "space-y",
              uiOutput("market_empty"),
              div(
                id = "market_charts",
                div(
                  class = "chart-grid-2",
                  section_card("Giá bán vs Số km đã đi", "Mỗi điểm là một mẫu xe trong tập đã lọc", "activity",
                    div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_scatter", height = "300px")), div(class='gg-output', plotOutput("market_scatter_gg", height = "300px")))),
                  section_card("Phân phối giá theo hãng", "Hộp giá min/Q1/median/Q3/max", "chart",
                    div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_box", height = "300px")), div(class='gg-output', plotOutput("market_box_gg", height = "300px")))))
                ),
                div(
                  class = "chart-grid-3",
                  section_card("Top hãng theo số lượng tin", NULL, "chart",
                    div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_top_brands", height = "280px")), div(class='gg-output', plotOutput("market_top_brands_gg", height = "280px")))),
                  section_card("Cơ cấu nhiên liệu", NULL, "fuel",
                    div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_fuel_chart", height = "280px")), div(class='gg-output', plotOutput("market_fuel_chart_gg", height = "280px")))),
                  section_card("Cơ cấu hộp số", NULL, "gauge",
                    div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_transmission_chart", height = "280px")), div(class='gg-output', plotOutput("market_transmission_chart_gg", height = "280px"))))
                ),
                section_card("Xu hướng giá theo năm sản xuất", "Median (đường đặc) và mean (đường đứt)", "trend",
                  div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("market_price_year", height = "300px")), div(class='gg-output', plotOutput("market_price_year_gg", height = "300px"))))
              )
            )
          ),
        tags$section(
          id = "visuals", class = "page space-y",
          div(class = "demo-banner", icon_svg("activity"), paste0("Trực quan hóa từ ", num(nrow(data_clean)), " mẫu xe đã làm sạch")),
          div(
            class = "chart-grid-2",
            section_card("Hãng xe phổ biến", "Top 12 theo số lượng tin, tooltip có giá trung vị", "chart",
              div(class = "plotly-wrap", div(class = "plotly-output", plotlyOutput("viz_overview", height = "420px")))),
            section_card("Phân phối giá theo hãng", "Top 12 hãng nhiều tin nhất, trục giá dùng thang log", "scale",
              div(class = "plotly-wrap", div(class = "plotly-output", plotlyOutput("viz_what", height = "420px"))))
          ),
          div(
            class = "chart-grid-2",
            section_card("Giá theo năm sản xuất", "Đường trung vị và vùng Q1-Q3 cho các năm đủ mẫu", "trend",
              div(class = "plotly-wrap", div(class = "plotly-output", plotlyOutput("viz_when", height = "420px")))),
            section_card("Odo và hộp số", "Giá trung vị theo nhóm số km đã đi", "activity",
              div(class = "plotly-wrap", div(class = "plotly-output", plotlyOutput("viz_why", height = "420px"))))
          )
        ),
        tags$section(
          id = "regions", class = "page space-y",
          div(class = "region-layout", section_card("Bản đồ khu vực", "Bố cục cách điệu, kích thước tỷ lệ theo số lượng tin", "map", uiOutput("region_map"), body_class = "flush"), uiOutput("region_cards")),
          section_card("Xếp hạng khu vực", "Sắp xếp giảm dần theo số lượng tin", "trophy", uiOutput("region_table"), body_class = "flush")
        ),
        tags$section(
          id = "compare", class = "page space-y",
          uiOutput("compare_slots"),
          div(
            class = "chart-grid-2",
            section_card("Radar so sánh", "Năm trục: Giá, Độ mới, Odo thấp, Thanh khoản, Tiết kiệm nhiên liệu", "scale",
              div(class = 'plotly-wrap', div(class='plotly-output', plotlyOutput("compare_radar", height = "340px")), div(class='gg-output', plotOutput("compare_radar_gg", height = "340px")))),
            section_card("Điểm số chi tiết", "Thang điểm chuẩn hoá 0–100 (cao hơn = tốt hơn)", NULL, uiOutput("compare_scores"))
          )
        ),
        tags$section(
          id = "estimate", class = "page space-y",
          div(class = "demo-banner", icon_svg("database"), "Định giá bằng mô hình Linear Regression, phân khúc bằng Decision Tree và cụm xe bằng K-Means."),
          div(
            class = "estimate-layout",
            section_card(
              "Cấu hình xe cần định giá", "Điền thông tin để hệ thống ước tính giá tham khảo", "calculator",
              div(
                class = "form-grid-2",
                filter_select("Hãng xe", "est_brand", BRANDS, DEFAULT_BRAND),
                filter_select("Dòng xe", "est_model", models_for_brand(DEFAULT_BRAND)),
                filter_select("Năm sản xuất", "est_year", setNames(rev(YEARS), rev(YEARS)), DEFAULT_YEAR),
                field("Số km đã đi", numericInput("est_km", NULL, value = 60000, min = 0, step = 1000, width = "100%")),
                filter_select("Hộp số", "est_transmission", TRANSMISSIONS, "Tự động"),
                filter_select("Nhiên liệu", "est_fuel", FUELS, "Xăng"),
                field("Dung tích động cơ (L)", numericInput("est_engine", NULL, value = 2.0, min = 0.4, max = 12.7, step = 0.1, width = "100%")),
                field("Số chỗ ngồi", numericInput("est_seats", NULL, value = 5, min = 2, max = 47, step = 1, width = "100%")),
                filter_select("Nguồn gốc", "est_origin", ORIGINS, "Trong nước"),
                filter_select("Khu vực", "est_region", REGIONS, DEFAULT_REGION),
                filter_select("Tình trạng xe", "est_condition", CONDITIONS, "Tốt")
              ),
              div(
                class = "mt-actions",
                actionButton("estimate_run", tagList(span(class = "btn-ico", icon_svg("sparkles")), "Dự đoán"), class = "btn btn-primary"),
                actionButton("estimate_reset", "Đặt lại", class = "btn btn-outline"),
                span(class = "sample-note", textOutput("estimate_sample", inline = TRUE))
              )
            ),
            div(
              class = "space-y",
              div(class = "estimate-hero", div(class = "road-lane"), div(class = "estimate-grid", uiOutput("estimate_result"), uiOutput("estimate_gauge"))),
              section_card("Các yếu tố ảnh hưởng đến giá", "Theo feature importance của Decision Tree", "sparkles", uiOutput("feature_importance")),
              section_card("Diễn giải nhanh", NULL, "gauge", uiOutput("estimate_explain"))
            )
          )
        ),
        tags$section(
          id = "models", class = "page space-y",
          div(class = "demo-banner", icon_svg("database"), paste0("Nguồn model: ", MODEL_SOURCE_LABEL)),
          div(
            class = "grid grid-kpi",
            kpi_card("R² hồi quy", "model_r2", "trend", "ocean", hint = "Linear Regression"),
            kpi_card("RMSE dự đoán", "model_rmse", "gauge", "navy", hint = "Đơn vị: tỷ VNĐ"),
            kpi_card("Accuracy cây quyết định", "model_tree_acc", "trophy", "success", hint = "Price segment"),
            kpi_card("Silhouette K-Means", "model_silhouette", "activity", "cyan", hint = "Độ tách cụm")
          ),
          div(
            class = "chart-grid-2",
            section_card("Tầm quan trọng biến", "Theo Decision Tree phân loại phân khúc giá", "sparkles", uiOutput("model_feature_importance")),
            section_card("Cụm K-Means", "Profile các nhóm xe từ output_models.RData", "database", DTOutput("model_cluster_table"))
          ),
          section_card("Hệ số Linear Regression", "Mô hình dự đoán log(price)", "trend", DTOutput("model_coef_table")),
          section_card("Ma trận Decision Tree", "Số lần dự đoán đúng/sai theo phân khúc giá", "table", DTOutput("model_conf_table"))
        ),
        tags$section(
          id = "data", class = "page",
          section_card(
            "Bộ dữ liệu bonbanh đã làm sạch", textOutput("data_desc", inline = TRUE), "table", body_class = "flush",
            div(
              class = "data-toolbar",
              div(class = "toolbar-search", icon_svg("search"), textInput("data_query", NULL, placeholder = "Tìm hãng, dòng, phiên bản…", width = "100%")),
              div(class = "pill-select", span("Nhiên liệu"), select_field("data_fuel", c("Tất cả" = "all", FUELS), "all")),
              div(class = "pill-select", span("Hộp số"), select_field("data_transmission", c("Tất cả" = "all", TRANSMISSIONS), "all")),
              div(class = "pill-select", span("Khu vực"), select_field("data_region", c("Toàn quốc" = "all", REGIONS), "all"))
            ),
            DTOutput("data_table")
          )
        ),
        tags$section(
          id = "report", class = "page space-y",
          div(
            class = "report-hero",
            div(span(class = "report-badge", icon_svg("file"), "Báo cáo tự động"), h2("Tổng hợp nhận xét — dữ liệu bonbanh"), p(textOutput("report_intro", inline = TRUE))),
            div(class = "report-actions", tags$button(class = "btn btn-outline", type = "button", onclick = "window.print()", span(class = "btn-ico", icon_svg("download")), "Tải PDF"), download_button("download_csv", "Tải CSV"), tags$button(id = "copy-insights", class = "btn btn-primary", type = "button", span(class = "btn-ico", icon_svg("copy")), "Sao chép nhận xét"))
          ),
          div(
            class = "grid",
            style = "grid-template-columns: repeat(4, minmax(0, 1fr));",
            kpi_card("Tổng số mẫu", "report_total", "car", "ocean"),
            kpi_card("Giá trung vị", "report_median", "dollar", "navy"),
            kpi_card("Hãng phổ biến", "report_brand", "trophy", "success"),
            kpi_card("Khu vực sôi động", "report_region", "map", "cyan")
          ),
          section_card("Nhận xét tự động", "Sinh từ thống kê mô tả và kết quả mô hình ML", "lightbulb", uiOutput("report_insights")),
          section_card("Phương pháp", "Dữ liệu, mô hình và giới hạn", NULL, uiOutput("report_method"))
        )
      )
    )
  )
)

# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  base_kpis <- reactive(kpis(data_clean))
  is_tab <- function(tab) identical(input$active_tab, tab)

  session$onFlushed(function() {
    updateSelectizeInput(session, "market_model", choices = c("Tất cả dòng" = "all", models_for_brand("all")), selected = "all", server = TRUE)
  }, once = TRUE)

  observeEvent(input$market_brand, {
    updateSelectizeInput(session, "market_model", choices = c("Tất cả dòng" = "all", models_for_brand(input$market_brand)), selected = "all", server = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$est_brand, {
    models <- models_for_brand(input$est_brand)
    updateSelectInput(session, "est_model", choices = models, selected = models[1])
    updateNumericInput(session, "est_engine", value = median_lookup(input$est_brand, models[1], "engine_size", 2))
    updateNumericInput(session, "est_seats", value = median_lookup(input$est_brand, models[1], "seat_count", 5))
  }, ignoreInit = TRUE)

  observeEvent(input$est_model, {
    updateNumericInput(session, "est_engine", value = median_lookup(input$est_brand, input$est_model, "engine_size", 2))
    updateNumericInput(session, "est_seats", value = median_lookup(input$est_brand, input$est_model, "seat_count", 5))
  }, ignoreInit = TRUE)

  for (i in 1:3) {
    local({
      idx <- i
      observeEvent(input[[paste0("cmp_brand_", idx)]], {
        models <- models_for_brand(input[[paste0("cmp_brand_", idx)]])
        updateSelectInput(session, paste0("cmp_model_", idx), choices = models, selected = models[1])
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$market_reset, {
    updateSelectInput(session, "market_brand", selected = "all")
    updateSelectizeInput(session, "market_model", choices = c("Tất cả dòng" = "all", models_for_brand("all")), selected = "all", server = TRUE)
    updateSelectInput(session, "market_year_from", selected = min(YEARS))
    updateSelectInput(session, "market_year_to", selected = max(YEARS))
    updateSliderInput(session, "market_price", value = c(0, PRICE_MAX))
    updateSliderInput(session, "market_km", value = c(0, KM_MAX))
    updateSelectInput(session, "market_fuel", selected = "all")
    updateSelectInput(session, "market_transmission", selected = "all")
    updateSelectInput(session, "market_region", selected = "all")
  })

  observeEvent(input$estimate_reset, {
    default_model <- models_for_brand(DEFAULT_BRAND)[1]
    updateSelectInput(session, "est_brand", selected = DEFAULT_BRAND)
    updateSelectInput(session, "est_model", choices = models_for_brand(DEFAULT_BRAND), selected = default_model)
    updateSelectInput(session, "est_year", selected = DEFAULT_YEAR)
    updateNumericInput(session, "est_km", value = 60000)
    updateSelectInput(session, "est_transmission", selected = "Tự động")
    updateSelectInput(session, "est_fuel", selected = "Xăng")
    updateNumericInput(session, "est_engine", value = median_lookup(DEFAULT_BRAND, default_model, "engine_size", 2))
    updateNumericInput(session, "est_seats", value = median_lookup(DEFAULT_BRAND, default_model, "seat_count", 5))
    updateSelectInput(session, "est_origin", selected = "Trong nước")
    updateSelectInput(session, "est_region", selected = DEFAULT_REGION)
    updateSelectInput(session, "est_condition", selected = "Tốt")
  })

  market_data <- reactive({
    df <- data_clean
    if (!is.null(input$market_brand) && input$market_brand != "all") df <- df %>% filter(brand == input$market_brand)
    if (!is.null(input$market_model) && input$market_model != "all") df <- df %>% filter(model == input$market_model)
    y1 <- min(as.integer(input$market_year_from), as.integer(input$market_year_to), na.rm = TRUE)
    y2 <- max(as.integer(input$market_year_from), as.integer(input$market_year_to), na.rm = TRUE)
    if (is.finite(y1) && is.finite(y2)) df <- df %>% filter(year >= y1, year <= y2)
    if (!is.null(input$market_price)) df <- df %>% filter(price >= input$market_price[1], price <= input$market_price[2])
    if (!is.null(input$market_km)) df <- df %>% filter(km >= input$market_km[1], km <= input$market_km[2])
    if (!is.null(input$market_fuel) && input$market_fuel != "all") df <- df %>% filter(fuel == input$market_fuel)
    if (!is.null(input$market_transmission) && input$market_transmission != "all") df <- df %>% filter(transmission == input$market_transmission)
    if (!is.null(input$market_region) && input$market_region != "all") df <- df %>% filter(region == input$market_region)
    df
  })

  output$hero_total <- renderText(paste0(num(base_kpis()$total), " mẫu xe"))
  output$hero_regions <- renderText(paste0(num(length(REGIONS)), " tỉnh/thành"))
  output$overview_gauge <- renderUI(gauge_arc(base_kpis()$automaticRatio, paste0(round(base_kpis()$automaticRatio * 100), "%"), "Tự động/CVT"))
  output$kpi_total <- renderText(num(base_kpis()$total))
  output$kpi_median <- renderText(format_vnd(base_kpis()$medianPrice))
  output$kpi_km <- renderText(format_km(base_kpis()$meanKm))
  output$kpi_top_brand <- renderText(base_kpis()$topBrand)
  output$kpi_top_region <- renderText(base_kpis()$topRegion)
  output$kpi_auto_ratio <- renderText(paste0(round(base_kpis()$automaticRatio * 100), "%"))

  vn_font <- list(family = "Inter, 'Space Grotesk', system-ui, -apple-system, sans-serif")

  line_chart <- function(df) {
    p <- price_by_year(df)
    plot_ly(p, x = ~year) %>%
      add_trace(y = ~median, name = "Trung vị", type = "scatter", mode = "lines+markers",
        line = list(color = CHART_COLORS[1], width = 3, shape = "spline"),
        marker = list(color = CHART_COLORS[1], size = 7, line = list(color = "#fff", width = 2)),
        fill = "tozeroy", fillcolor = "rgba(15,120,199,0.08)",
        hovertemplate = "<b>%{x}</b><br>Trung vị: %{y:,.0f} ₫<extra></extra>") %>%
      add_trace(y = ~mean, name = "Trung bình", type = "scatter", mode = "lines+markers",
        line = list(color = CHART_COLORS[2], width = 2, dash = "dash", shape = "spline"),
        marker = list(color = CHART_COLORS[2], size = 5),
        hovertemplate = "<b>%{x}</b><br>Trung bình: %{y:,.0f} ₫<extra></extra>") %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", gridcolor = "rgba(220,232,242,0.6)", gridwidth = 1, zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = "", tickformat = ".2s", gridcolor = "rgba(220,232,242,0.6)", gridwidth = 1, zeroline = FALSE, tickfont = vn_font),
        legend = list(orientation = "h", x = 0, y = -0.18, font = vn_font),
        margin = list(l = 60, r = 16, t = 8, b = 54),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  donut_chart <- function(df, col, center = "Mẫu") {
    d <- breakdown(df, col)
    plot_ly(d, labels = ~name, values = ~value, type = "pie", hole = .58, sort = FALSE,
      marker = list(colors = CHART_COLORS, line = list(color = "#ffffff", width = 2)),
      textinfo = "label+percent", textfont = list(family = vn_font$family, size = 12),
      hovertemplate = "<b>%{label}</b><br>%{value} mẫu (%{percent})<extra></extra>") %>%
      layout(
        font = vn_font,
        annotations = list(list(text = center, x = .5, y = .5, showarrow = FALSE, font = list(size = 14, color = "#647087", family = vn_font$family))),
        legend = list(orientation = "h", x = 0, y = -0.12, font = vn_font),
        margin = list(l = 8, r = 8, t = 8, b = 48),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  top_brand_chart <- function(df, n = 8) {
    d <- top_brands(df, n) %>% arrange(n)
    # Create a gradient-like effect using varying opacity colors
    n_bars <- nrow(d)
    bar_colors <- colorRampPalette(c("#5dd4e9", "#0f78c7", "#173b70"))(n_bars)
    plot_ly(d, x = ~n, y = ~reorder(brand, n), type = "bar", orientation = "h",
      marker = list(color = bar_colors, line = list(width = 0, color = "rgba(0,0,0,0)"),
        opacity = 0.9),
      text = ~paste0(n, " mẫu"), textposition = "outside", textfont = list(family = vn_font$family, size = 11, color = "#647087"),
      hovertemplate = "<b>%{y}</b><br>%{x} mẫu<extra></extra>") %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", gridcolor = "rgba(220,232,242,0.6)", gridwidth = 1, zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = "", tickfont = list(family = vn_font$family, size = 12)),
        margin = list(l = 100, r = 50, t = 8, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  brand_box_chart <- function(df, n = 10) {
    top <- top_brands(df, n)$brand
    df_filtered <- df %>% filter(brand %in% top)
    plot_ly(df_filtered, x = ~brand, y = ~price, type = "box",
      color = I(CHART_COLORS[1]),
      marker = list(color = CHART_COLORS[2], opacity = .42, size = 4),
      line = list(color = CHART_COLORS[1], width = 2),
      fillcolor = "rgba(15,120,199,0.12)",
      hovertemplate = "<b>Hãng: %{x}</b><br>Giá: %{y:,.0f} ₫<extra></extra>") %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", tickangle = -25, gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = list(text = "Giá (VNĐ)", font = vn_font), tickformat = ".2s", gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        margin = list(l = 60, r = 18, t = 8, b = 74),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  odo_transmission_chart <- function(df) {
    plot_ly(df, x = ~km, y = ~price, type = "scatter", mode = "markers",
      color = ~transmission, colors = CHART_COLORS[c(1, 4, 2)],
      text = ~paste0("<b>", brand, " ", model, "</b><br>Hộp số: ", transmission, "<br>Odo: ", format_km(km), "<br>Giá: ", format_vnd(price)),
      hoverinfo = "text",
      marker = list(size = 7, opacity = .58, line = list(width = 1, color = "rgba(255,255,255,0.55)"))) %>%
      layout(
        font = vn_font,
        xaxis = list(title = list(text = "Số km đã đi", font = vn_font), gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = list(text = "Giá (VNĐ)", font = vn_font), tickformat = ".2s", gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        legend = list(orientation = "h", x = 0, y = -0.18, font = vn_font),
        margin = list(l = 60, r = 18, t = 8, b = 64),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  empty_plotly <- function(message = "Không có dữ liệu phù hợp") {
    plot_ly() %>%
      layout(
        font = vn_font,
        xaxis = list(visible = FALSE, zeroline = FALSE),
        yaxis = list(visible = FALSE, zeroline = FALSE),
        annotations = list(list(
          text = message, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(family = vn_font$family, size = 14, color = "#647087")
        )),
        margin = list(l = 12, r = 12, t = 12, b = 12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  viz_brand_overview_chart <- function(df, n = 12) {
    d <- df %>%
      group_by(brand) %>%
      summarise(
        count = n(),
        medianPrice = median_safe(price),
        meanPrice = mean_safe(price),
        .groups = "drop"
      ) %>%
      arrange(desc(count), brand) %>%
      slice_head(n = n) %>%
      arrange(count) %>%
      mutate(
        brand = factor(brand, levels = brand),
        countLabel = paste0(num(count), " mẫu"),
        hover = paste0(
          "<b>", brand, "</b><br>",
          "Số tin: ", num(count), "<br>",
          "Giá trung vị: ", format_vnd(medianPrice), "<br>",
          "Giá trung bình: ", format_vnd(meanPrice)
        )
      )
    if (!nrow(d)) return(empty_plotly())
    bar_colors <- colorRampPalette(c("#5dd4e9", "#0f78c7", "#173b70"))(nrow(d))
    plot_ly(
      d, x = ~count, y = ~brand, type = "bar", orientation = "h",
      marker = list(color = bar_colors, line = list(width = 0)),
      text = ~countLabel, textposition = "outside",
      textfont = list(family = vn_font$family, size = 11, color = "#647087"),
      hovertext = ~hover, hoverinfo = "text", cliponaxis = FALSE
    ) %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = "", tickfont = list(family = vn_font$family, size = 12, color = "#344863")),
        margin = list(l = 110, r = 74, t = 10, b = 44),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  viz_price_distribution_chart <- function(df, n = 12) {
    brands <- df %>%
      group_by(brand) %>%
      summarise(count = n(), medianPrice = median_safe(price), .groups = "drop") %>%
      arrange(desc(count), brand) %>%
      slice_head(n = n) %>%
      arrange(medianPrice) %>%
      pull(brand)
    d <- df %>%
      filter(.data$brand %in% brands, is.finite(price), price > 0) %>%
      mutate(brand = factor(brand, levels = brands))
    if (!nrow(d)) return(empty_plotly())
    ticks <- c(1e8, 2e8, 5e8, 1e9, 2e9, 5e9, 1e10)
    plot_ly(
      d, x = ~price, y = ~brand, type = "box", orientation = "h",
      boxpoints = FALSE, boxmean = TRUE,
      marker = list(color = "rgba(34,176,125,0.36)"),
      line = list(color = CHART_COLORS[2], width = 1.6),
      fillcolor = "rgba(34,176,125,0.16)",
      hovertemplate = "<b>%{y}</b><br>Giá: %{x:,.0f} ₫<extra></extra>"
    ) %>%
      layout(
        font = vn_font,
        xaxis = list(
          title = "", type = "log", tickvals = ticks, ticktext = format_vnd(ticks),
          gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = list(family = vn_font$family, size = 11)
        ),
        yaxis = list(title = "", tickfont = list(family = vn_font$family, size = 11, color = "#344863")),
        margin = list(l = 118, r = 24, t = 10, b = 56),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  viz_year_band_chart <- function(df) {
    d <- df %>%
      group_by(year) %>%
      summarise(
        count = n(),
        q1 = q_safe(price, 0.25),
        medianPrice = median_safe(price),
        q3 = q_safe(price, 0.75),
        .groups = "drop"
      ) %>%
      filter(count >= 5, is.finite(medianPrice), medianPrice > 0) %>%
      arrange(year) %>%
      mutate(
        hover = paste0(
          "<b>Năm ", year, "</b><br>",
          "Số mẫu: ", num(count), "<br>",
          "Q1: ", format_vnd(q1), "<br>",
          "Trung vị: ", format_vnd(medianPrice), "<br>",
          "Q3: ", format_vnd(q3)
        )
      )
    if (!nrow(d)) return(empty_plotly())
    plot_ly(d, x = ~year) %>%
      add_trace(
        y = ~q3, type = "scatter", mode = "lines",
        line = list(color = "rgba(15,120,199,0)", width = 0),
        hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        y = ~q1, name = "Vùng Q1-Q3", type = "scatter", mode = "lines",
        fill = "tonexty", fillcolor = "rgba(15,120,199,0.14)",
        line = list(color = "rgba(15,120,199,0)", width = 0),
        hoverinfo = "skip"
      ) %>%
      add_trace(
        y = ~medianPrice, name = "Trung vị", type = "scatter", mode = "lines+markers",
        line = list(color = CHART_COLORS[1], width = 3, shape = "spline"),
        marker = list(color = CHART_COLORS[1], size = 6, line = list(color = "#fff", width = 1.6)),
        hovertext = ~hover, hoverinfo = "text"
      ) %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", range = list(min(d$year) - 0.5, max(d$year) + 0.5), dtick = 2, gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = "", tickformat = ".2s", gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = vn_font),
        legend = list(orientation = "h", x = 0, y = -0.15, font = vn_font),
        margin = list(l = 62, r = 18, t = 10, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  viz_mileage_transmission_chart <- function(df) {
    breaks <- c(0, 25000, 50000, 75000, 100000, 150000, 200000, 300000, Inf)
    labels <- c("0-25k", "25-50k", "50-75k", "75-100k", "100-150k", "150-200k", "200-300k", "300k+")
    d <- df %>%
      filter(is.finite(km), km >= 0, is.finite(price), price > 0, !is.na(transmission), transmission != "") %>%
      mutate(kmBucket = cut(km, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)) %>%
      group_by(kmBucket, transmission) %>%
      summarise(count = n(), medianPrice = median_safe(price), .groups = "drop") %>%
      filter(count >= 5, is.finite(medianPrice), medianPrice > 0) %>%
      mutate(
        kmBucket = factor(kmBucket, levels = labels),
        hover = paste0(
          "<b>", transmission, "</b><br>",
          "Odo: ", kmBucket, " km<br>",
          "Số mẫu: ", num(count), "<br>",
          "Giá trung vị: ", format_vnd(medianPrice)
        )
      ) %>%
      arrange(kmBucket)
    if (!nrow(d)) return(empty_plotly())
    plot_ly(
      d, x = ~kmBucket, y = ~medianPrice, color = ~transmission,
      colors = CHART_COLORS[c(1, 4, 2)], type = "scatter", mode = "lines+markers",
      line = list(width = 3, shape = "spline"),
      marker = list(size = 8, line = list(color = "#fff", width = 1.6)),
      hovertext = ~hover, hoverinfo = "text"
    ) %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = list(family = vn_font$family, size = 11)),
        yaxis = list(title = "", tickformat = ".2s", gridcolor = "rgba(220,232,242,0.65)", zeroline = FALSE, tickfont = vn_font),
        legend = list(orientation = "h", x = 0, y = -0.16, font = vn_font),
        margin = list(l = 62, r = 18, t = 10, b = 64),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  }

  output$overview_price_year <- renderPlotly(line_chart(data_clean))
  output$overview_fuel <- renderPlotly(donut_chart(data_clean, "fuel"))
  output$overview_top_brands <- renderPlotly(top_brand_chart(data_clean, 8))

  output$viz_overview <- renderPlotly(viz_brand_overview_chart(data_clean, 12))
  output$viz_what <- renderPlotly(viz_price_distribution_chart(data_clean, 12))
  output$viz_when <- renderPlotly(viz_year_band_chart(data_clean))
  output$viz_why <- renderPlotly(viz_mileage_transmission_chart(data_clean))

  output$market_filter_desc <- renderText({ req(is_tab("market")); paste0(num(nrow(market_data())), "/", num(nrow(data_clean)), " mẫu phù hợp") })
  output$market_price_label <- renderText({ req(is_tab("market")); paste(format_vnd(input$market_price[1]), "–", format_vnd(input$market_price[2])) })
  output$market_km_label <- renderText({ req(is_tab("market")); paste(format_km(input$market_km[1]), "–", format_km(input$market_km[2])) })

  output$market_empty <- renderUI({
    req(is_tab("market"))
    if (nrow(market_data()) > 0) return(NULL)
    section_card(
      "Không có dữ liệu phù hợp", NULL, "filter",
      div(class = "empty-state", div(class = "empty-icon", icon_svg("filter")), div(class = "font-display", "Bộ lọc không khớp với mẫu nào"), p("Hãy nới rộng khoảng năm, khoảng giá hoặc đặt lại bộ lọc để khám phá lại dữ liệu thị trường."), tags$button(class = "btn btn-outline", type = "button", onclick = "Shiny.setInputValue('market_reset', Math.random(), {priority:'event'})", "Đặt lại bộ lọc"))
    )
  })

  output$market_scatter <- renderPlotly({
    req(is_tab("market"))
    df <- market_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp."))
    if (nrow(df) > 3500) df <- slice_sample(df, n = 3500)
    df$fuel <- factor(df$fuel, levels = unique(df$fuel))
    fuel_levels <- levels(df$fuel)
    fuel_palette <- setNames(
      CHART_COLORS[seq_along(fuel_levels)],
      fuel_levels
    )
    plot_ly(df, x = ~km, y = ~price, type = "scatter", mode = "markers",
      color = ~fuel, colors = fuel_palette,
      text = ~paste0("<b>", brand, " ", model, "</b><br>Odo: ", format_km(km), "<br>Giá: ", format_vnd(price)),
      hoverinfo = "text",
      marker = list(size = 8, opacity = .68, line = list(width = 1, color = "rgba(255,255,255,0.5)"))) %>%
      layout(
        font = vn_font,
        xaxis = list(title = list(text = "Số km đã đi", font = vn_font), gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = list(text = "Giá (VNĐ)", font = vn_font), tickformat = ".2s", gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        legend = list(orientation = "h", x = 0, y = -0.2, font = vn_font),
        margin = list(l = 60, r = 18, t = 8, b = 68),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$market_box <- renderPlotly({
    req(is_tab("market"))
    df <- market_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp."))
    top <- top_brands(df, 8)$brand
    df_filtered <- df %>% filter(brand %in% top)
    plot_ly(df_filtered, x = ~brand, y = ~price, type = "box",
      color = I(CHART_COLORS[1]),
      marker = list(color = CHART_COLORS[2], opacity = .42, size = 4),
      line = list(color = CHART_COLORS[1], width = 2),
      fillcolor = "rgba(15,120,199,0.12)",
      hovertemplate = "<b>Hãng: %{x}</b><br>Giá: %{y:,.0f} ₫<extra></extra>") %>%
      layout(
        font = vn_font,
        xaxis = list(title = "", tickangle = -25, gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        yaxis = list(title = list(text = "Giá (VNĐ)", font = vn_font), tickformat = ".2s", gridcolor = "rgba(220,232,242,0.6)", zeroline = FALSE, tickfont = vn_font),
        margin = list(l = 60, r = 18, t = 8, b = 74),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$market_top_brands <- renderPlotly({ req(is_tab("market")); top_brand_chart(market_data(), 8) })
  output$market_fuel_chart <- renderPlotly({ req(is_tab("market")); donut_chart(market_data(), "fuel") })
  output$market_transmission_chart <- renderPlotly({ req(is_tab("market")); donut_chart(market_data(), "transmission") })
  output$market_price_year <- renderPlotly({ req(is_tab("market")); line_chart(market_data()) })

  # ggplot2 fallbacks for environments where Plotly is not available or fails
  output$overview_price_year_gg <- renderPlot({
    p <- price_by_year(data_clean)
    ggplot(p, aes(x = year)) +
      geom_line(aes(y = median, colour = "median"), linewidth = 1.1) +
      geom_line(aes(y = mean, colour = "mean"), linetype = "dashed", linewidth = 0.9) +
      scale_colour_manual(values = c(median = CHART_COLORS[1], mean = CHART_COLORS[2])) +
      theme_minimal() + labs(x = NULL, y = NULL) + scale_y_continuous(labels = format_vnd) + theme(legend.position = "bottom")
  })

  output$overview_fuel_gg <- renderPlot({
    d <- breakdown(data_clean, "fuel")
    ggplot(d, aes(x = 1, y = value, fill = name)) + geom_col(width = 1) + coord_polar(theta = "y") + theme_void() + theme(legend.position = "bottom")
  })

  output$overview_top_brands_gg <- renderPlot({
    d <- top_brands(data_clean, 8)
    ggplot(d, aes(x = reorder(brand, n), y = n)) + geom_col(fill = CHART_COLORS[1]) + coord_flip() + theme_minimal() + labs(x = NULL, y = NULL)
  })

  output$market_scatter_gg <- renderPlot({
    req(is_tab("market"))
    df <- market_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp."))
    if (nrow(df) > 3500) df <- slice_sample(df, n = 3500)
    ggplot(df, aes(x = km, y = price, color = fuel)) + geom_point(alpha = 0.6, size = 1.6) + theme_minimal() + labs(x = "Số km đã đi", y = "Giá") + scale_y_continuous(labels = format_vnd)
  })

  output$market_box_gg <- renderPlot({
    req(is_tab("market"))
    df <- market_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp."))
    topb <- top_brands(df, 8)$brand
    ggplot(df %>% filter(brand %in% topb), aes(x = brand, y = price)) + geom_boxplot(fill = CHART_COLORS[1], outlier.size = 0.5) + theme_minimal() + labs(x = NULL, y = "Giá") + theme(axis.text.x = element_text(angle = -25, hjust = 0))
  })

  output$market_top_brands_gg <- renderPlot({
    req(is_tab("market"))
    d <- top_brands(market_data(), 8) %>% arrange(n)
    ggplot(d, aes(x = reorder(brand, n), y = n)) + geom_col(fill = CHART_COLORS[1]) + coord_flip() + theme_minimal() + labs(x = NULL, y = NULL)
  })

  output$market_fuel_chart_gg <- renderPlot({
    req(is_tab("market"))
    d <- breakdown(market_data(), "fuel")
    ggplot(d, aes(x = 1, y = value, fill = name)) + geom_col(width = 1) + coord_polar(theta = "y") + theme_void() + theme(legend.position = "bottom")
  })

  output$market_transmission_chart_gg <- renderPlot({
    req(is_tab("market"))
    d <- breakdown(market_data(), "transmission")
    ggplot(d, aes(x = 1, y = value, fill = name)) + geom_col(width = 1) + coord_polar(theta = "y") + theme_void() + theme(legend.position = "bottom")
  })

  output$market_price_year_gg <- renderPlot({
    req(is_tab("market"))
    p <- price_by_year(market_data())
    ggplot(p, aes(x = year)) + geom_line(aes(y = median), color = CHART_COLORS[1], linewidth = 1.1) + geom_line(aes(y = mean), color = CHART_COLORS[2], linetype = "dashed", linewidth = 0.9) + theme_minimal() + labs(x = NULL, y = NULL) + scale_y_continuous(labels = format_vnd)
  })

  output$region_map <- renderUI({
    req(is_tab("regions"))
    stats <- region_stats() %>% slice_head(n = 10)
    pos <- data.frame(
      x = c(200, 260, 235, 170, 145, 205, 125, 285, 90, 250),
      y = c(60, 110, 210, 380, 350, 370, 420, 285, 300, 445)
    )
    max_count <- max(stats$count)
    tags$div(
      class = "region-map",
      tags$svg(
        viewBox = "0 0 400 500", style = "position:absolute;inset:0;width:100%;height:100%;",
        tags$path(d = "M210 30 Q 280 140 240 230 Q 200 320 180 410", stroke = "rgba(93,212,233,.45)", `stroke-width` = 2, fill = "none", `stroke-dasharray` = "4 6"),
        tags$path(d = "M180 30 Q 290 100 250 220 Q 240 270 200 320 Q 170 360 180 420 Q 130 460 110 430 Q 130 380 150 350 Q 180 300 210 230 Q 230 160 180 60 Z", fill = "rgba(93,212,233,.20)", stroke = "rgba(15,120,199,.25)", `stroke-width` = 1.5),
        lapply(seq_len(nrow(stats)), function(i) {
          s <- stats[i, ]
          xy <- c(pos$x[i], pos$y[i])
          r <- 14 + (s$count / max_count) * 22
          tags$g(
            tags$circle(cx = xy[1], cy = xy[2], r = r + 6, fill = "rgba(15,120,199,.15)"),
            tags$circle(cx = xy[1], cy = xy[2], r = r, fill = "#0f78c7", stroke = "white", `stroke-width` = 3),
            tags$text(x = xy[1], y = xy[2] + 4, `text-anchor` = "middle", fill = "white", `font-size` = 11, `font-weight` = 650, s$count),
            tags$text(x = xy[1], y = xy[2] + r + 14, `text-anchor` = "middle", fill = "#173b70", `font-size` = 11, `font-weight` = 650, s$region)
          )
        })
      )
    )
  })

  output$region_cards <- renderUI({
    req(is_tab("regions"))
    stats <- region_stats() %>% slice_head(n = 10)
    max_velocity <- max(stats$velocity)
    div(
      class = "region-cards",
      lapply(seq_len(nrow(stats)), function(i) {
        s <- stats[i, ]
        tags$article(
          class = "region-card",
          div(style = "display:flex;align-items:flex-start;justify-content:space-between;gap:8px;",
            div(div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.08em;color:var(--muted-foreground);", "Khu vực"), div(class = "font-display", style = "font-size:16px;font-weight:650;color:var(--navy);", s$region)),
            span(class = "region-chip", paste(num(s$count), "tin"))
          ),
          tags$dl(class = "stat-grid",
            div(tags$dt("Giá trung vị"), tags$dd(format_vnd(s$medianPrice))),
            div(tags$dt("Hãng phổ biến"), tags$dd(s$topBrand))
          ),
          div(style = "margin-top:12px;",
            div(style = "display:flex;justify-content:space-between;font-size:11px;margin-bottom:4px;", span(style = "color:var(--muted-foreground);", "Chỉ số hoạt động"), span(style = "font-weight:650;color:var(--primary);", s$velocity)),
            div(class = "progress", span(style = paste0("width:", min(100, s$velocity / max_velocity * 100), "%;")))
          )
        )
      })
    )
  })

  output$region_table <- renderUI({
    req(is_tab("regions"))
    stats <- region_stats() %>% slice_head(n = 15)
    tags$div(
      class = "table-wrapper",
      tags$table(
        class = "plain-table",
        tags$thead(tags$tr(tags$th("#"), tags$th("Khu vực"), tags$th("Số lượng tin"), tags$th("Giá trung vị"), tags$th("Hãng phổ biến"), tags$th("Chỉ số hoạt động"))),
        tags$tbody(lapply(seq_len(nrow(stats)), function(i) {
          s <- stats[i, ]
          tags$tr(tags$td(sprintf("%02d", i)), tags$td(style = "font-weight:650;color:var(--navy);", s$region), tags$td(num(s$count)), tags$td(format_vnd(s$medianPrice)), tags$td(s$topBrand), tags$td(style = "color:var(--success);font-weight:650;", paste("↗", s$velocity)))
        }))
      )
    )
  })

  output$compare_slots <- renderUI({
    req(is_tab("compare"))
    defaults <- list(
      list(brand = DEFAULT_BRAND, year = DEFAULT_YEAR),
      list(brand = if ("HYUNDAI" %in% BRANDS) "HYUNDAI" else BRANDS[min(2, length(BRANDS))], year = DEFAULT_YEAR),
      list(brand = if ("MAZDA" %in% BRANDS) "MAZDA" else BRANDS[min(3, length(BRANDS))], year = max(min(YEARS), DEFAULT_YEAR - 1))
    )
    colors <- CHART_COLORS[1:3]
    div(
      class = "compare-grid",
      lapply(1:3, function(i) {
        b <- defaults[[i]]$brand
        models <- models_for_brand(b)
        tags$article(
          class = "compare-card",
          div(class = "compare-line", style = paste0("background:", colors[i], ";")),
          div(
            class = "compare-body",
            div(style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:16px;", span(class = "slot-badge", style = paste0("background:", colors[i], ";"), paste("Xe", i))),
            filter_select("Hãng", paste0("cmp_brand_", i), BRANDS, b),
            filter_select("Dòng xe", paste0("cmp_model_", i), models, models[1]),
            filter_select("Năm sản xuất", paste0("cmp_year_", i), setNames(YEARS, YEARS), defaults[[i]]$year),
            uiOutput(paste0("cmp_specs_", i))
          )
        )
      })
    )
  })

  compare_cars <- reactive({
    lapply(1:3, function(i) {
      representative_car(input[[paste0("cmp_brand_", i)]], input[[paste0("cmp_model_", i)]], input[[paste0("cmp_year_", i)]])
    })
  })

  for (i in 1:3) {
    local({
      idx <- i
      output[[paste0("cmp_specs_", idx)]] <- renderUI({
        car <- compare_cars()[[idx]]
        if (is.null(car)) {
          return(div(style = "border-radius:14px;background:rgba(233,168,38,.10);padding:12px;font-size:12px;", "Không tìm thấy mẫu phù hợp trong dữ liệu. Hãy chọn năm khác."))
        }
        score <- score_of(car)
        tags$dl(
          class = "spec-grid",
          div(tags$dt("Giá tham khảo"), tags$dd(format_vnd(car$price))),
          div(tags$dt("Odo"), tags$dd(format_km(car$km))),
          div(tags$dt("Nhiên liệu"), tags$dd(car$fuel)),
          div(tags$dt("Hộp số"), tags$dd(car$transmission)),
          div(tags$dt("Điểm giữ giá"), tags$dd(paste0(score[["Thanh khoản"]], "/100"))),
          div(tags$dt("Ước tính chi phí vận hành/năm"), tags$dd(format_vnd(car$price * .06)))
        )
      })
    })
  }

  output$compare_radar <- renderPlotly({
    req(is_tab("compare"))
    cars <- compare_cars()
    axes <- c("Giá", "Độ mới", "Odo thấp", "Thanh khoản", "Tiết kiệm NL")
    axes_full <- c("Giá", "Độ mới", "Odo thấp", "Thanh khoản", "Tiết kiệm nhiên liệu")
    fig <- plot_ly(type = "scatterpolar", mode = "lines+markers")
    for (i in seq_along(cars)) {
      car <- cars[[i]]
      if (is.null(car)) next
      s <- score_of(car)
      vals <- c(as.numeric(s[axes_full]), as.numeric(s[[axes_full[1]]]))
      labs <- c(axes, axes[1])
      fig <- add_trace(fig, r = vals, theta = labs, fill = "toself",
        name = paste(car$brand, car$model, car$year),
        line = list(color = CHART_COLORS[i], width = 2),
        marker = list(color = CHART_COLORS[i], size = 7, line = list(color = "#fff", width = 2)),
        fillcolor = paste0(substr(CHART_COLORS[i], 1, 7), "22"),
        hovertemplate = paste0("<b>", paste(car$brand, car$model, car$year), "</b><br>%{theta}: %{r}/100<extra></extra>"))
    }
    fig %>% layout(
      font = vn_font,
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100), gridcolor = "rgba(220,232,242,0.6)", tickfont = list(family = vn_font$family, size = 10)),
        angularaxis = list(tickfont = list(family = vn_font$family, size = 12, color = "#344863"))
      ),
      legend = list(orientation = "h", x = 0, y = -0.12, font = vn_font),
      margin = list(l = 50, r = 50, t = 20, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = "#173b70", font = list(family = vn_font$family, color = "#fff", size = 13), bordercolor = "transparent")
    ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$compare_radar_gg <- renderPlot({
    req(is_tab("compare"))
    cars <- compare_cars()
    axes <- c("Giá", "Độ mới", "Odo thấp", "Thanh khoản", "Tiết kiệm nhiên liệu")
    df_list <- list()
    for (i in seq_along(cars)) {
      car <- cars[[i]]
      if (is.null(car)) next
      s <- score_of(car)
      vals <- as.numeric(s[axes])
      df_list[[length(df_list) + 1]] <- data.frame(axis = axes, value = vals, name = paste(car$brand, car$model, car$year), stringsAsFactors = FALSE)
    }
    if (!length(df_list)) {
      plot.new(); text(0.5,0.5,"Không có mẫu để so sánh", cex=1.2)
      return()
    }
    d <- do.call(rbind, df_list)
    d$axis_f <- factor(d$axis, levels = axes)
    ggplot(d, aes(x = axis_f, y = value, group = name, fill = name, colour = name)) +
      geom_polygon(alpha = 0.25) + geom_line(linewidth = 0.8) + geom_point(size = 2) +
      coord_polar() + theme_minimal() + labs(x = NULL, y = NULL) + theme(legend.position = "bottom")
  })

  output$compare_scores <- renderUI({
    req(is_tab("compare"))
    cars <- compare_cars()
    axes <- c("Giá", "Độ mới", "Odo thấp", "Thanh khoản", "Tiết kiệm nhiên liệu")
    tagList(lapply(axes, function(axis) {
      div(
        class = "feature-row",
        div(class = "feature-head", span(axis)),
        lapply(seq_along(cars), function(i) {
          car <- cars[[i]]
          if (is.null(car)) return(NULL)
          score <- score_of(car)[[axis]]
          div(
            style = "display:flex;align-items:center;gap:8px;margin:6px 0;",
            span(style = "width:160px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;font-size:11px;color:var(--muted-foreground);", paste(car$brand, car$model, car$year)),
            div(class = "feature-track", span(class = "feature-fill", style = paste0("width:", score, "%;background:", CHART_COLORS[i], ";"))),
            span(style = "width:28px;text-align:right;font-size:11px;", score)
          )
        })
      )
    }))
  })

  empty_dt <- function(message) {
    datatable(data.frame(`Trạng thái` = message), rownames = FALSE, options = list(dom = "t"))
  }

  output$model_r2 <- renderText({
    req(is_tab("models"))
    format_metric(artifact("reg_metrics", list())$r_squared, 3)
  })
  output$model_rmse <- renderText({
    req(is_tab("models"))
    format_metric(artifact("reg_metrics", list())$rmse_billion, 3)
  })
  output$model_tree_acc <- renderText({
    req(is_tab("models"))
    format_metric(artifact("tree_accuracy", NA), 3)
  })
  output$model_silhouette <- renderText({
    req(is_tab("models"))
    format_metric(artifact("avg_silhouette", NA), 3)
  })

  output$model_feature_importance <- renderUI({
    req(is_tab("models"))
    items <- artifact("feat_imp")
    if (is.null(items) || !nrow(items)) {
      return(div(class = "empty-state", div(class = "empty-icon", icon_svg("sparkles")), "Chưa có feature importance trong artifact model."))
    }
    label_col <- if ("feature_vn" %in% names(items)) "feature_vn" else "feature"
    value_col <- if ("importance_pct" %in% names(items)) "importance_pct" else "importance"
    items <- items %>%
      transmute(label = .data[[label_col]], value = round(as.numeric(.data[[value_col]]), 1)) %>%
      arrange(desc(value))
    if (max(items$value, na.rm = TRUE) > 100) {
      items$value <- round(items$value / sum(items$value, na.rm = TRUE) * 100, 1)
    }
    tagList(lapply(seq_len(nrow(items)), function(i) {
      div(class = "feature-row", div(class = "feature-head", span(items$label[i]), span(paste0(items$value[i], "%"))), div(class = "feature-track", span(class = "feature-fill", style = paste0("width:", min(items$value[i], 100), "%;"))))
    }))
  })

  output$model_cluster_table <- renderDT({
    req(is_tab("models"))
    clusters <- artifact("cluster_profiles_raw")
    if (is.null(clusters) || !nrow(clusters)) return(empty_dt("Chưa có profile K-Means."))
    clusters <- as.data.frame(clusters)
    datatable(clusters, rownames = FALSE, class = "display hover stripe", options = list(pageLength = 6, lengthChange = FALSE, scrollX = TRUE, dom = "tip"))
  })

  output$model_coef_table <- renderDT({
    req(is_tab("models"))
    coef_tbl <- artifact("coef_df")
    if (is.null(coef_tbl) || !nrow(coef_tbl)) return(empty_dt("Chưa có bảng hệ số hồi quy."))
    coef_tbl <- as.data.frame(coef_tbl)
    datatable(coef_tbl, rownames = FALSE, class = "display hover stripe", options = list(pageLength = 8, lengthChange = FALSE, scrollX = TRUE, dom = "tip"))
  })

  output$model_conf_table <- renderDT({
    req(is_tab("models"))
    conf_tbl <- artifact("conf_table")
    if (is.null(conf_tbl) || !nrow(conf_tbl)) return(empty_dt("Chưa có confusion matrix."))
    conf_tbl <- as.data.frame(conf_tbl)
    datatable(conf_tbl, rownames = FALSE, class = "display hover stripe", options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE, dom = "tip"))
  })

  estimate_form <- reactive({
    brand <- input$est_brand %||% DEFAULT_BRAND
    model <- input$est_model %||% models_for_brand(brand)[1]
    list(
      brand = brand,
      model = model,
      year = as.integer(input$est_year %||% DEFAULT_YEAR),
      km = as.numeric(input$est_km %||% 60000),
      transmission = input$est_transmission %||% "Tự động",
      fuel = input$est_fuel %||% "Xăng",
      engine_size = as.numeric(input$est_engine %||% median_lookup(brand, model, "engine_size", 2)),
      seat_count = as.numeric(input$est_seats %||% median_lookup(brand, model, "seat_count", 5)),
      origin = input$est_origin %||% "Trong nước",
      region = input$est_region %||% DEFAULT_REGION,
      condition = input$est_condition %||% "Tốt"
    )
  })

  estimate_result <- eventReactive(input$estimate_run, {
    estimate_price(estimate_form())
  }, ignoreNULL = FALSE)

  output$estimate_sample <- renderText({
    req(is_tab("estimate"))
    paste0("Kết quả mới nhất | ", estimate_result()$sampleSize, " mẫu tương tự | ", estimate_result()$source)
  })
  output$estimate_result <- renderUI({
    req(is_tab("estimate"))
    res <- estimate_result()
    div(
      div(class = "estimate-label", "Giá dự đoán bằng ML"),
      div(class = "estimate-price", format_vnd(res$point)),
      div(
        class = "estimate-range",
        span(icon_svg("down"), paste("Thấp:", format_vnd(res$low))),
        span(icon_svg("up"), paste("Cao:", format_vnd(res$high))),
        span(icon_svg("trophy"), paste("Phân khúc:", res$segment))
      )
    )
  })
  output$estimate_gauge <- renderUI({ req(is_tab("estimate")); gauge_arc(estimate_result()$confidence, paste0(round(estimate_result()$confidence * 100), "%"), "Độ tin cậy mô hình", size = 150) })
  output$feature_importance <- renderUI({
    req(is_tab("estimate"))
    items <- artifact("feat_imp")
    if (is.null(items) || !nrow(items)) {
      items <- data.frame(feature_vn = c("Tuổi xe", "Số km đã đi", "Dung tích động cơ", "Hộp số tự động"), importance_pct = c(35, 25, 25, 15))
    }
    items <- items %>%
      transmute(label = feature_vn, value = round(importance_pct, 1)) %>%
      arrange(desc(value))
    tagList(lapply(seq_len(nrow(items)), function(i) {
      div(class = "feature-row", div(class = "feature-head", span(items$label[i]), span(paste0(items$value[i], "%"))), div(class = "feature-track", span(class = "feature-fill", style = paste0("width:", min(items$value[i], 100), "%;"))))
    }))
  })
  output$estimate_explain <- renderUI({
    req(is_tab("estimate"))
    f <- estimate_form()
    r <- estimate_result()
    tags$ul(
      class = "insight-list",
      tags$li(paste0(f$brand, " ", f$model, " đời ", f$year, " được dự đoán ở phân khúc ", r$segment, ", khoảng tham khảo ", format_vnd(r$low), " - ", format_vnd(r$high), ".")),
      tags$li(paste0("Cấu hình đưa vào model: ", format_km(f$km), ", động cơ ", f$engine_size, "L, ", f$seat_count, " chỗ, ", f$origin, ", ", f$transmission, ".")),
      tags$li(paste0("K-Means xếp cấu hình này gần nhóm: ", r$clusterName, ".")),
      tags$li(paste0("Kết quả dùng ", r$source, "; khoảng giá đã cộng thêm sai số từ RMSE và độ phủ mẫu tương tự."))
    )
  })

  data_rows <- reactive({
    q <- tolower(trimws(input$data_query %||% ""))
    df <- data_clean
    if (!is.null(input$data_fuel) && input$data_fuel != "all") df <- df %>% filter(fuel == input$data_fuel)
    if (!is.null(input$data_transmission) && input$data_transmission != "all") df <- df %>% filter(transmission == input$data_transmission)
    if (!is.null(input$data_region) && input$data_region != "all") df <- df %>% filter(region == input$data_region)
    if (nzchar(q)) {
      hay <- tolower(paste(df$brand, df$model, df$version))
      df <- df[grepl(q, hay, fixed = TRUE), ]
    }
    df
  })

  output$data_desc <- renderText({ req(is_tab("data")); paste0(num(nrow(data_rows())), " / ", num(nrow(data_clean)), " dòng phù hợp") })
  output$data_table <- renderDT({
    req(is_tab("data"))
    df <- data_rows() %>%
      transmute(
        `Hãng` = brand,
        `Dòng xe` = model,
        `Phiên bản` = version,
        `Năm` = year,
        `Giá` = format_vnd(price),
        `Số km` = num(km),
        `Động cơ` = paste0(num(engine_size, 1), "L"),
        `Số chỗ` = seat_count,
        `Hộp số` = transmission,
        `Nhiên liệu` = fuel,
        `Nguồn gốc` = origin,
        `Khu vực` = region,
        `Phân khúc` = as.character(price_segment),
        `Cụm ML` = cluster_name
      )
    datatable(df, rownames = FALSE, class = "display hover stripe", options = list(pageLength = 14, lengthChange = FALSE, scrollX = TRUE, dom = "tip", language = list(paginate = list(previous = "Trước", `next` = "Sau"), zeroRecords = "Không có dòng nào khớp với bộ lọc.", info = "Trang _PAGE_ / _PAGES_ · _TOTAL_ dòng")))
  })

  report_k <- reactive(kpis(data_clean))
  output$report_intro <- renderText({ req(is_tab("report")); paste0("Báo cáo tổng hợp ", num(nrow(data_clean)), " mẫu xe từ ", APP_DATA_SOURCE_LABEL, " và mô hình từ ", MODEL_SOURCE_LABEL, ".") })
  output$report_total <- renderText({ req(is_tab("report")); num(report_k()$total) })
  output$report_median <- renderText({ req(is_tab("report")); format_vnd(report_k()$medianPrice) })
  output$report_brand <- renderText({ req(is_tab("report")); report_k()$topBrand })
  output$report_region <- renderText({ req(is_tab("report")); report_k()$topRegion })

  insights <- reactive(c(
    paste0(report_k()$topBrand, " có số lượng mẫu cao nhất trong dữ liệu, phản ánh độ phổ biến nổi bật trên tập tin bonbanh đã làm sạch."),
    paste0("Mô hình hồi quy đạt R² khoảng ", artifact("reg_metrics", list(r_squared = NA))$r_squared, ", dùng các biến tuổi xe, odo, dung tích động cơ, hộp số, nguồn gốc và số chỗ."),
    "Số km đã đi là một trong các yếu tố ảnh hưởng mạnh đến giá, đặc biệt khi vượt mốc 100.000 km.",
    paste0("Phân khúc xe số tự động chiếm ", round(report_k()$automaticRatio * 100), "%, cho thấy xu hướng ưu tiên trải nghiệm lái thuận tiện."),
    paste0(report_k()$topRegion, " là khu vực có lượng tin lớn nhất; K-Means đang chia thị trường thành ", artifact("OPTIMAL_K", 4), " cụm hành vi giá/tuổi/odo/động cơ.")
  ))

  output$report_insights <- renderUI({
    req(is_tab("report"))
    div(
      id = "insight-copy-source",
      lapply(seq_along(insights()), function(i) {
        div(class = "insight-item", span(class = "insight-num", i), span(insights()[i]))
      })
    )
  })

  output$report_method <- renderUI({
    req(is_tab("report"))
    div(
      style = "font-size:14px;color:rgba(32,49,79,.82);line-height:1.55;",
      p(paste0("Dữ liệu lấy từ ", APP_DATA_SOURCE_LABEL, ", lọc các dòng hợp lệ theo năm, giá, odo, dung tích động cơ và số chỗ; sau chuẩn hóa còn ", num(nrow(data_clean)), " mẫu xe.")),
      p(HTML(paste0("<strong>Mô hình:</strong> Linear Regression dự đoán log(price), Decision Tree phân loại phân khúc giá, K-Means gom cụm xe. Accuracy cây quyết định: ", artifact("tree_accuracy", NA), "; silhouette K-Means: ", artifact("avg_silhouette", NA), "."))),
      p(HTML("<strong>Giới hạn:</strong> giá dự đoán là tham khảo học thuật, chưa thay thế thẩm định xe thực tế, lịch sử bảo dưỡng, tình trạng tai nạn/ngập nước hoặc thương lượng giao dịch."))
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() "hcmute-autoinsight-bonbanh.csv",
    content = function(file) {
      write.csv(data_clean, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  lapply(
    c(
      "overview_price_year", "overview_fuel", "overview_top_brands",
      "viz_overview", "viz_what", "viz_when", "viz_why",
      "market_scatter", "market_box", "market_top_brands",
      "market_fuel_chart", "market_transmission_chart", "market_price_year",
      "compare_radar",
      "overview_price_year_gg", "overview_fuel_gg", "overview_top_brands_gg",
      "market_scatter_gg", "market_box_gg", "market_top_brands_gg",
      "market_fuel_chart_gg", "market_transmission_chart_gg",
      "market_price_year_gg", "compare_radar_gg"
    ),
    function(id) outputOptions(output, id, suspendWhenHidden = FALSE)
  )

  # Custom sidebar tabs use CSS visibility, so chart outputs stay warm and
  # refresh correctly when a page becomes visible.
}

shinyApp(ui = ui, server = server)
