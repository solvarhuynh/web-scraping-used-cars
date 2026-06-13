# ==============================================================================
# Script: utils.R
# Purpose: Shared utility functions for all clean_{source}.R scripts
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

# ── Schema ────────────────────────────────────────────────────────────────────
CANONICAL_COLS <- c(
  "brand", "model", "trim", "year", "body_type", "fuel_type",
  "transmission", "engine_size", "seat_count", "drivetrain",
  "price", "mileage", "origin", "color", "city",
  "posted_date", "source", "url"
)

LOG_FILE <- "web_scraping/log.txt"

# ── Paths ─────────────────────────────────────────────────────────────────────
project_path <- function(...) {
  file.path(...)
}

ensure_directories <- function() {
  dirs <- c(
    "web_scraping/data/raw",
    "web_scraping/data/clean",
    "web_scraping/data/init_db",
    "web_scraping/data/quality_report",
    "insights/visualization/plots",
    "machine_learning"
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

# ── Logging ───────────────────────────────────────────────────────────────────
log_message <- function(script_name, msg, level = "INFO") {
  dir.create(dirname(LOG_FILE), recursive = TRUE, showWarnings = FALSE)
  entry <- sprintf("[%s] [%s] - %s: %s\n",
                   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   script_name, level, msg)
  cat(entry)
  cat(entry, file = LOG_FILE, append = TRUE)
}

# ── NA normalisation ──────────────────────────────────────────────────────────
# Chuyển empty string, "NA", "N/A", "-", khoảng trắng → NA thật sự
normalize_na <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  x[str_trim(x) %in% c("", "NA", "N/A", "-", "Đang cập nhật",
                         "Không rõ", "Chưa cập nhật", "nan")] <- NA_character_
  str_squish(x)
}

# ── Price → numeric VND ───────────────────────────────────────────────────────
# Xử lý: "340.000.000 đ", "300 triệu", "1.5 tỷ", "1 tỷ 250 triệu",
#         "300,000,000", "8.49e+08"
# Dùng numeric (double) thay vì integer để chứa được giá trị > 2.1 tỷ
clean_price <- function(x) {
  x <- normalize_na(x)
  result <- sapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    v <- str_to_lower(str_trim(v))

    # Loại bỏ ký tự tiền tệ và khoảng trắng thừa
    v <- str_remove_all(v, "[đ$]")
    v <- str_trim(v)

    # Giá đã ở dạng số hoặc scientific notation, ví dụ "849000000", "8.49e+08"
    direct_num <- suppressWarnings(as.numeric(v))
    if (!is.na(direct_num) && direct_num > 0 && !str_detect(v, "tỷ|ty|triệu|trieu|tr\\b")) {
      return(direct_num)
    }

    # "1.5 tỷ", "1,5 tỷ", "1 tỷ 250 triệu"
    if (str_detect(v, "tỷ|ty")) {
      val_ty <- suppressWarnings(as.numeric(str_replace_all(
        str_extract(v, "[0-9]+[.,]?[0-9]*(?=\\s*(tỷ|ty))"), ",", ".")))
      val_tr <- suppressWarnings(as.numeric(str_replace_all(
        str_extract(v, "[0-9]+[.,]?[0-9]*(?=\\s*(triệu|trieu|tr\\b))"), ",", ".")))
      total <- coalesce(val_ty, 0) * 1e9 + coalesce(val_tr, 0) * 1e6
      if (total > 0) return(round(total))
    }

    # "300 triệu" hoặc "300tr"
    if (str_detect(v, "triệu|trieu|tr\\b")) {
      num <- suppressWarnings(as.numeric(str_replace_all(
        str_extract(v, "[0-9]+[.,]?[0-9]*"), ",", ".")))
      return(round(num * 1e6))
    }

    # "340.000.000" hoặc "340,000,000" (dấu chấm/phẩy là phân cách nghìn)
    v_clean <- str_remove_all(v, "[.,\\s]")
    num <- suppressWarnings(as.numeric(v_clean))
    if (!is.na(num) && num > 0) return(num)

    NA_real_
  }, USE.NAMES = FALSE)
  as.numeric(result)
}

# ── Mileage → integer km ──────────────────────────────────────────────────────
clean_mileage <- function(x) {
  x <- normalize_na(x)
  result <- sapply(x, function(v) {
    if (is.na(v)) return(NA_integer_)
    v <- str_to_lower(str_trim(v))
    # Xử lý "5 vạn km" → 50000
    if (str_detect(v, "vạn|van")) {
      num <- suppressWarnings(as.numeric(str_extract(v, "[0-9]+[.,]?[0-9]*")))
      return(as.integer(round(num * 10000)))
    }
    # Loại bỏ "km", dấu chấm/phẩy
    v <- str_remove_all(v, "km|,|\\.")
    v <- str_extract(v, "[0-9]+")
    as.integer(v)
  }, USE.NAMES = FALSE)
  as.integer(result)
}

# ── Engine size → numeric (lít) ───────────────────────────────────────────────
clean_engine_size <- function(x) {
  x <- normalize_na(x)
  result <- sapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    v <- str_to_lower(str_trim(v))
    # "1.5L", "1500cc", "1,5 l"
    if (str_detect(v, "cc|cm3")) {
      num <- suppressWarnings(as.numeric(str_extract(v, "[0-9]+")))
      if (!is.na(num)) return(round(num / 1000, 2))
    }
    num <- suppressWarnings(as.numeric(str_replace_all(
      str_extract(v, "[0-9]+[.,][0-9]+|[0-9]+"), ",", ".")))
    # Nếu số lớn hơn 100 → có thể là cc, không phải lít
    if (!is.na(num) && num > 100) return(round(num / 1000, 2))
    num
  }, USE.NAMES = FALSE)
  as.numeric(result)
}

# ── Year & seat_count → integer ───────────────────────────────────────────────
clean_year       <- function(x) suppressWarnings(as.integer(normalize_na(as.character(x))))
clean_seat_count <- function(x) {
  x <- normalize_na(as.character(x))
  as.integer(str_extract(x, "[0-9]+"))
}

# ── posted_date → Date (DD-MM-YYYY) ──────────────────────────────────────────
# Xử lý:
#   "12 giờ trước" / "3 phút trước"  → ngày hiện tại
#   "1 ngày trước" / "2 ngày trước"  → trừ N ngày
#   "1 tuần trước"                   → trừ 7 ngày
#   "1 tháng trước"                  → trừ 30 ngày
#   "hôm nay"                        → hôm nay
#   "hôm qua"                        → hôm qua
#   "DD/MM/YYYY" hoặc "YYYY-MM-DD"   → parse trực tiếp
clean_posted_date <- function(x) {
  x <- normalize_na(as.character(x))
  now_date <- Sys.Date()

  result <- sapply(x, function(v) {
    if (is.na(v)) return(format(now_date, "%d-%m-%Y"))
    v <- str_to_lower(str_trim(v))

    if (str_detect(v, "giờ|phút|giay|giây|hôm nay")) {
      return(format(now_date, "%d-%m-%Y"))
    }
    if (str_detect(v, "hôm qua")) {
      return(format(now_date - 1, "%d-%m-%Y"))
    }
    if (str_detect(v, "ngày")) {
      n <- suppressWarnings(as.integer(str_extract(v, "[0-9]+")))
      if (is.na(n)) n <- 1L
      return(format(now_date - n, "%d-%m-%Y"))
    }
    if (str_detect(v, "tuần")) {
      n <- suppressWarnings(as.integer(str_extract(v, "[0-9]+")))
      if (is.na(n)) n <- 1L
      return(format(now_date - n * 7L, "%d-%m-%Y"))
    }
    if (str_detect(v, "tháng")) {
      n <- suppressWarnings(as.integer(str_extract(v, "[0-9]+")))
      if (is.na(n)) n <- 1L
      return(format(now_date - n * 30L, "%d-%m-%Y"))
    }
    if (str_detect(v, "năm")) {
      n <- suppressWarnings(as.integer(str_extract(v, "[0-9]+")))
      if (is.na(n)) n <- 1L
      return(format(now_date - n * 365L, "%d-%m-%Y"))
    }
    # Thử parse các format phổ biến
    parsed <- tryCatch({
      d <- parse_date_time(v, orders = c("dmy", "ymd", "mdy", "d/m/Y", "Y-m-d"), quiet = TRUE)
      if (!is.na(d)) return(format(as.Date(d), "%d-%m-%Y"))
      NULL
    }, error = function(e) NULL)
    if (!is.null(parsed)) return(parsed)

    # Không parse được → dùng ngày hiện tại
    format(now_date, "%d-%m-%Y")
  }, USE.NAMES = FALSE)

  as.character(result)
}

# ── City: chỉ giữ tên tỉnh/thành phố (phần cuối cùng sau dấu phẩy) ───────────
# VD: "Phường Tiến Thành, Thành phố Đồng Xoài, Bình Phước" → "Bình Phước"
# VD: "Phường Thới An, Quận 12, Tp Hồ Chí Minh"            → "Tp Hồ Chí Minh"
clean_city <- function(x) {
  x <- normalize_na(x)
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    parts <- str_split(v, ",")[[1]]
    city  <- str_trim(parts[length(parts)])
    if (city == "" || is.na(city)) return(NA_character_)
    city
  }, USE.NAMES = FALSE)
}

# ── Fuel type → chuẩn tiếng Việt ─────────────────────────────────────────────
clean_fuel_type <- function(x) {
  x <- normalize_na(x)
  dplyr::case_when(
    str_detect(x, regex("petrol|xăng|gasoline", ignore_case = TRUE))     ~ "Xăng",
    str_detect(x, regex("diesel|dầu", ignore_case = TRUE))               ~ "Dầu",
    str_detect(x, regex("hybrid", ignore_case = TRUE))                   ~ "Hybrid",
    str_detect(x, regex("electric|điện|ev", ignore_case = TRUE))         ~ "Điện",
    str_detect(x, regex("lpg|gas|cng", ignore_case = TRUE))              ~ "Gas",
    !is.na(x)                                                            ~ x,
    TRUE                                                                 ~ NA_character_
  )
}

# ── Transmission → chuẩn tiếng Việt ──────────────────────────────────────────
clean_transmission <- function(x) {
  x <- normalize_na(x)
  dplyr::case_when(
    str_detect(x, regex("automatic|tự động|auto|at\\b", ignore_case = TRUE)) ~ "Tự động",
    str_detect(x, regex("manual|số sàn|sàn|mt\\b", ignore_case = TRUE))      ~ "Số sàn",
    str_detect(x, regex("cvt", ignore_case = TRUE))                           ~ "CVT",
    str_detect(x, regex("semi|bán tự động", ignore_case = TRUE))              ~ "Bán tự động",
    !is.na(x)                                                                ~ x,
    TRUE                                                                     ~ NA_character_
  )
}

# ── Body type → chuẩn ─────────────────────────────────────────────────────────
clean_body_type <- function(x) {
  x <- normalize_na(x)
  dplyr::case_when(
    str_detect(x, regex("sedan", ignore_case = TRUE))                        ~ "Sedan",
    str_detect(x, regex("suv", ignore_case = TRUE))                          ~ "SUV",
    str_detect(x, regex("hatchback", ignore_case = TRUE))                    ~ "Hatchback",
    str_detect(x, regex("crossover", ignore_case = TRUE))                    ~ "Crossover",
    str_detect(x, regex("coupe|coupé", ignore_case = TRUE))                  ~ "Coupe",
    str_detect(x, regex("mpv|minivan|mini van", ignore_case = TRUE))         ~ "MPV",
    str_detect(x, regex("van|minibus", ignore_case = TRUE))                  ~ "Van/Minibus",
    str_detect(x, regex("pickup|bán tải", ignore_case = TRUE))               ~ "Bán tải",
    str_detect(x, regex("convertible|mui trần", ignore_case = TRUE))         ~ "Mui trần",
    str_detect(x, regex("wagon|estate", ignore_case = TRUE))                 ~ "Wagon",
    !is.na(x)                                                                ~ x,
    TRUE                                                                     ~ NA_character_
  )
}

# ── Drivetrain → chuẩn ────────────────────────────────────────────────────────
clean_drivetrain <- function(x) {
  x <- normalize_na(x)
  dplyr::case_when(
    str_detect(x, regex("4wd|4x4", ignore_case = TRUE))                     ~ "4WD",
    str_detect(x, regex("awd", ignore_case = TRUE))                         ~ "AWD",
    str_detect(x, regex("fwd|cầu trước|2wd.*front|front", ignore_case = TRUE)) ~ "FWD",
    str_detect(x, regex("rwd|rfd|cầu sau|rear", ignore_case = TRUE))        ~ "RWD",
    !is.na(x)                                                               ~ x,
    TRUE                                                                    ~ NA_character_
  )
}

# ── Origin → chuẩn ────────────────────────────────────────────────────────────
clean_origin <- function(x) {
  x <- normalize_na(x)   # "Đang cập nhật" đã bị normalize_na → NA
  dplyr::case_when(
    str_detect(x, regex("trong nước|việt nam|vn\\b", ignore_case = TRUE))    ~ "Trong nước",
    str_detect(x, regex("nhập|import|nước ngoài|nước khác|foreign",
                         ignore_case = TRUE))                                 ~ "Nhập khẩu",
    # Quốc gia cụ thể → Nhập khẩu
    str_detect(x, regex("nhật|japan|hàn|korea|đức|germany|mỹ|usa|america|
                          anh|uk|trung quốc|china|thái|thailand|ý|italy",
                         ignore_case = TRUE))                                 ~ "Nhập khẩu",
    !is.na(x)                                                                ~ x,
    TRUE                                                                     ~ NA_character_
  )
}

# ── Brand & Model → UPPERCASE ─────────────────────────────────────────────────
clean_brand <- function(x) {
  x <- normalize_na(x)
  toupper(x)
}

clean_model <- function(x) {
  x <- normalize_na(x)
  toupper(x)
}

align_schema <- function(df) {
  for (col in CANONICAL_COLS) {
    if (!col %in% names(df)) df[[col]] <- rep(NA_character_, nrow(df))
  }
  df[, CANONICAL_COLS]
}

normalize_city_name <- function(x) {
  x <- normalize_na(x)
  dplyr::case_when(
    str_detect(x, regex("hồ chí minh|ho chi minh|hcm|tp hcm|tp\\. hcm", ignore_case = TRUE)) ~ "Tp Hồ Chí Minh",
    TRUE ~ x
  )
}

# ── Áp dụng toàn bộ cleaning chung cho dataframe ──────────────────────────────
standardize_car_data <- function(df) {
  df <- align_schema(df)

  df %>%
    mutate(
      # NA normalisation trước cho tất cả cột ký tự
      across(where(is.character), normalize_na),

      # Numeric columns
      price       = clean_price(price),
      mileage     = clean_mileage(mileage),
      engine_size = clean_engine_size(engine_size),
      year        = clean_year(year),
      seat_count  = clean_seat_count(seat_count),

      # Date
      posted_date = clean_posted_date(posted_date),

      # Categorical
      fuel_type    = clean_fuel_type(fuel_type),
      transmission = clean_transmission(transmission),
      body_type    = clean_body_type(body_type),
      drivetrain   = clean_drivetrain(drivetrain),
      origin       = clean_origin(origin),

      # City: lấy tỉnh/thành cuối cùng
      city = normalize_city_name(clean_city(city)),

      # Text casing
      brand = clean_brand(brand),
      model = clean_model(model),

      # trim & color: chỉ squish + normalize NA, không đổi nội dung
      trim  = normalize_na(trim),
      color = normalize_na(color)
    ) %>%
    align_schema()
}

apply_business_rules <- function(df,
                                 min_year = 1990L,
                                 max_year = as.integer(format(Sys.Date(), "%Y")),
                                 min_price = 5e7,
                                 max_price = 1.5e10,
                                 max_mileage = 1e6) {
  df %>%
    filter(
      !is.na(brand), brand != "",
      !is.na(model), model != "",
      !is.na(url), url != "",
      !is.na(year), year >= min_year, year <= max_year,
      !is.na(price), price >= min_price, price <= max_price,
      is.na(mileage) | (mileage >= 0 & mileage <= max_mileage)
    ) %>%
    distinct(url, .keep_all = TRUE) %>%
    align_schema()
}

read_clean_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    align_schema()
}

read_master_data <- function(master_file = "web_scraping/data/master_data.csv",
                             clean_dir = "web_scraping/data/clean") {
  if (file.exists(master_file)) {
    return(read_clean_csv(master_file))
  }

  clean_files <- list.files(clean_dir, pattern = "^data_.*_clean\\.csv$", full.names = TRUE)
  if (!length(clean_files)) {
    stop("No master_data.csv or clean CSV files found.")
  }

  bind_rows(lapply(clean_files, read_clean_csv))
}

# ── Safe write CSV ─────────────────────────────────────────────────────────────
safe_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
  log_message("utils.R", sprintf("Written %d rows to: %s", nrow(df), path))
}
