# ============================================================
# FILE: Probability_statistics.R
# File này dùng để thống kê mô tả và tính xác suất
# ============================================================

OUTPUT_DIR <- "insights/descriptive_analytics/output_probability_statistics"
CLEAN_FILE <- file.path(OUTPUT_DIR, "00_data_da_lam_sach.csv")

if (!file.exists(CLEAN_FILE)) {
  stop("Không tìm thấy tệp dữ liệu sạch. Vui lòng chạy file Cleaning_Data.R trước!")
}

# CÁC HÀM TIỆN ÍCH ĐỊNH DẠNG BẢNG BIỂU
format_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE))
}

format_pct <- function(x, digits = 2) {
  ifelse(is.na(x), "", paste0(round(x * 100, digits), "%"))
}

to_bool <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("true", "1", "yes")
}

save_csv <- function(df, file_name) {
  write.csv(df, file.path(OUTPUT_DIR, file_name), row.names = FALSE, fileEncoding = "UTF-8")
}

print_line <- function(widths) {
  cat("+", paste(sapply(widths, function(w) paste(rep("-", w + 2), collapse = "")), collapse = "+"), "+\n", sep = "")
}

print_table <- function(df, title = NULL, max_rows = 30) {
  if (!is.null(title)) cat("\n", title, "\n", sep = "")
  if (is.null(df) || nrow(df) == 0) { cat("(No data)\n"); return(invisible(NULL)) }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(df) > max_rows) df <- df[1:max_rows, , drop = FALSE]
  df[] <- lapply(df, as.character)
  headers <- names(df)
  widths <- pmax(nchar(headers), sapply(df, function(col) max(nchar(col), na.rm = TRUE)))
  widths <- pmax(pmin(widths, 28), 4)

  trim_cell <- function(value, width) {
    value <- as.character(value)
    if (nchar(value) > width) paste0(substr(value, 1, width - 3), "...") else value
  }

  print_line(widths)
  cat("|")
  for (i in seq_along(headers)) cat(" ", sprintf(paste0("%-", widths[i], "s"), trim_cell(headers[i], widths[i])), " |", sep = "")
  cat("\n")
  print_line(widths)
  for (r in seq_len(nrow(df))) {
    cat("|")
    for (c in seq_along(headers)) cat(" ", sprintf(paste0("%-", widths[c], "s"), trim_cell(df[r, c], widths[c])), " |", sep = "")
    cat("\n")
  }
  print_line(widths)
}

format_numbers_df <- function(df) {
  out <- df
  for (col in names(out)) if (is.numeric(out[[col]])) out[[col]] <- format_num(out[[col]])
  out
}

show_export <- function(df, title, file_name, max_rows = 30) {
  print_table(format_numbers_df(df), title, max_rows)
  save_csv(df, file_name)
}

safe_var <- function(x) if (length(x) > 1) var(x) else NA_real_
safe_sd <- function(x) if (length(x) > 1) sd(x) else NA_real_

group_numeric_stats <- function(data, group_col, value_col) {
  groups <- split(data[[value_col]], data[[group_col]])
  result <- do.call(rbind, lapply(names(groups), function(g) {
    x <- groups[[g]]
    x <- x[!is.na(x)]
    data.frame(group = g, so_luong = length(x), trung_binh = mean(x), trung_vi = median(x), nho_nhat = min(x), lon_nhat = max(x), phuong_sai = safe_var(x), do_lech_chuan = safe_sd(x), stringsAsFactors = FALSE)
  }))
  names(result)[1] <- group_col
  result <- result[order(-result$so_luong, -result$trung_binh), ]
  row.names(result) <- NULL
  result
}

freq_table <- function(x, col_name) {
  tab <- as.data.frame(table(x, useNA = "no"), stringsAsFactors = FALSE)
  names(tab) <- c(col_name, "so_luong")
  tab <- tab[tab$so_luong > 0, ]
  tab$ty_le <- tab$so_luong / sum(tab$so_luong)
  tab$ty_le_pct <- format_pct(tab$ty_le)
  tab[order(-tab$so_luong), ]
}

probability_table <- function(data, group_col, event_col) {
  tab <- as.data.frame(table(data[[group_col]], data[[event_col]], useNA = "no"), stringsAsFactors = FALSE)
  names(tab) <- c(group_col, event_col, "so_luong")
  tab <- tab[tab$so_luong > 0, ]
  totals <- aggregate(so_luong ~ key, data = data.frame(key = tab[[group_col]], so_luong = tab$so_luong), FUN = sum)
  names(totals) <- c(group_col, "tong_nhom")
  result <- merge(tab, totals, by = group_col, all.x = TRUE)
  result$xac_suat <- result$so_luong / result$tong_nhom
  result$xac_suat_pct <- format_pct(result$xac_suat)
  result <- result[order(result[[group_col]], -result$xac_suat), ]
  row.names(result) <- NULL
  result
}

# NẠP VÀ ÉP KIỂU BIẾN SỐ SẠCH
data_clean <- read.csv(CLEAN_FILE, stringsAsFactors = FALSE)

numeric_cols <- c("year", "price", "price_raw", "mileage", "age")
for (col in numeric_cols) data_clean[[col]] <- as.numeric(data_clean[[col]])

data_clean$is_high_price <- to_bool(data_clean$is_high_price)
data_clean$is_high_mileage <- to_bool(data_clean$is_high_mileage)

age_order <- c("0-3 nam", "4-7 nam", "8-12 nam", "Tren 12 nam")
data_clean$age_group <- factor(data_clean$age_group, levels = age_order)

n <- nrow(data_clean)
price_q75 <- quantile(data_clean$price, 0.75, na.rm = TRUE)
mileage_q75 <- quantile(data_clean$mileage, 0.75, na.rm = TRUE)

# TÍNH TOÁN CÁC BẢNG SỐ LIỆU HỌC THUẬT
overview <- data.frame(
  chi_so = c("So dong da lam sach", "So hang xe", "So loai hop so", "So nguon du lieu", "Nam cu nhat", "Nam moi nhat", "Gia Q75", "Mileage Q75"),
  gia_tri = c(n, length(unique(data_clean$brand)), length(unique(data_clean$transmission)), length(unique(data_clean$source)), min(data_clean$year, na.rm = TRUE), max(data_clean$year, na.rm = TRUE), round(as.numeric(price_q75), 2), round(as.numeric(mileage_q75), 2))
)

numeric_summary <- do.call(rbind, lapply(c("price", "mileage", "year", "age"), function(v) {
  x <- data_clean[[v]]; x <- x[!is.na(x)]
  data.frame(bien = v, so_luong = length(x), trung_binh = mean(x), trung_vi = median(x), nho_nhat = min(x), lon_nhat = max(x), phuong_sai = safe_var(x), do_lech_chuan = safe_sd(x), stringsAsFactors = FALSE)
}))

brand_stats <- group_numeric_stats(data_clean, "brand", "price")
transmission_stats <- group_numeric_stats(data_clean, "transmission", "price")

freq_transmission <- freq_table(data_clean$transmission, "transmission")
freq_fuel <- freq_table(data_clean$fuel_type, "fuel_type")
freq_age_group <- freq_table(data_clean$age_group, "age_group")
freq_price_scale <- freq_table(data_clean$price_scale, "price_scale")

basic_prob <- data.frame(
  bien_co = c("P(Tự động)", "P(Số sàn)", "P(CVT)", "P(Gia cao >= Q75)", "P(Mileage cao >= Q75)", "P(Xe moi 0-3 nam)", "P(Xe tren 12 nam)"),
  so_luong = c(sum(data_clean$transmission == "Tự động", na.rm = TRUE), sum(data_clean$transmission == "Số sàn", na.rm = TRUE), sum(data_clean$transmission == "CVT", na.rm = TRUE), sum(data_clean$is_high_price, na.rm = TRUE), sum(data_clean$is_high_mileage, na.rm = TRUE), sum(data_clean$age_group == "0-3 nam", na.rm = TRUE), sum(data_clean$age_group == "Tren 12 nam", na.rm = TRUE))
)
basic_prob$xac_suat <- basic_prob$so_luong / n
basic_prob$xac_suat_pct <- format_pct(basic_prob$xac_suat)

prob_transmission_by_age <- probability_table(data_clean, "age_group", "transmission")
prob_fuel_by_transmission <- probability_table(data_clean, "transmission", "fuel_type")
prob_high_price_by_transmission <- probability_table(data_clean, "transmission", "is_high_price")

prob_high_price_by_brand <- probability_table(data_clean, "brand", "is_high_price")
prob_high_price_by_brand <- prob_high_price_by_brand[as.character(prob_high_price_by_brand$is_high_price) == "TRUE", ]
prob_high_price_by_brand <- prob_high_price_by_brand[order(-prob_high_price_by_brand$xac_suat, -prob_high_price_by_brand$so_luong), ]

joint_tab <- as.data.frame(table(data_clean$age_group, data_clean$transmission), stringsAsFactors = FALSE)
names(joint_tab) <- c("age_group", "transmission", "so_luong")
joint_tab$xac_suat_ket_hop <- joint_tab$so_luong / n
joint_tab$xac_suat_ket_hop_pct <- format_pct(joint_tab$xac_suat_ket_hop)

bayes_auto_new <- data.frame(
  cong_thuc = "P(0-3 nam | Tự động)",
  so_xe_automatic_va_moi = sum(data_clean$transmission == "Tự động" & data_clean$age_group == "0-3 nam", na.rm = TRUE),
  tong_xe_automatic = sum(data_clean$transmission == "Tự động", na.rm = TRUE)
)
bayes_auto_new$xac_suat <- ifelse(bayes_auto_new$tong_xe_automatic == 0, NA, bayes_auto_new$so_xe_automatic_va_moi / bayes_auto_new$tong_xe_automatic)
bayes_auto_new$xac_suat_pct <- format_pct(bayes_auto_new$xac_suat)

# KIỂM ĐỊNH THỐNG KÊ GIẢ THUYẾT
test_results <- data.frame(kiem_dinh = character(), thong_ke = character(), p_value = numeric(), ket_luan = character(), stringsAsFactors = FALSE)
data_ttest <- data_clean[data_clean$transmission %in% c("Tự động", "Số sàn"), ]

if (length(unique(data_ttest$transmission)) == 2 && all(table(data_ttest$transmission) >= 2)) {
  ttest <- t.test(price ~ transmission, data = data_ttest)
  test_results <- rbind(test_results, data.frame(kiem_dinh = "T-test: price ~ transmission", thong_ke = paste0("t = ", round(ttest$statistic, 4)), p_value = ttest$p.value, ket_luan = ifelse(ttest$p.value < 0.05, "Co khac biet gia trung binh", "Chua du bang chung khac biet"), stringsAsFactors = FALSE))
}

chisq_table <- table(data_clean$transmission, data_clean$age_group)
if (all(dim(chisq_table) > 1)) {
  chisq <- suppressWarnings(chisq.test(chisq_table))
  test_results <- rbind(test_results, data.frame(kiem_dinh = "Chi-square: transmission vs age_group", thong_ke = paste0("X2 = ", round(chisq$statistic, 4)), p_value = chisq$p.value, ket_luan = ifelse(chisq$p.value < 0.05, "Co moi lien he thong ke", "Chua du bang chung co moi lien he"), stringsAsFactors = FALSE))
}

if (length(unique(data_clean$price)) > 1 && length(unique(data_clean$mileage)) > 1) {
  cor_pm <- cor.test(data_clean$price, data_clean$mileage, method = "pearson")
  test_results <- rbind(test_results, data.frame(kiem_dinh = "Pearson correlation: price vs mileage", thong_ke = paste0("r = ", round(cor_pm$estimate, 4)), p_value = cor_pm$p.value, ket_luan = ifelse(cor_pm$p.value < 0.05, "Co tuong quan thong ke", "Chua du bang chung co tuong quan"), stringsAsFactors = FALSE))
}
test_results_fmt <- test_results
test_results_fmt$p_value <- format_num(test_results_fmt$p_value, 6)

# HIỂN THỊ TERMINAL VÀ ĐỒNG LOẠT IN FILE CSV VÀO OUTPUT_PROBABILITY_STATISTICS
show_export(overview, "BANG 1. TONG QUAN DU LIEU", "01_tong_quan_du_lieu.csv")
show_export(numeric_summary, "BANG 2. THONG KE MO TA TONG QUAT", "02_thong_ke_mo_ta_tong_quat.csv")
show_export(brand_stats, "BANG 3. THONG KE GIA THEO HANG XE - TOP 30", "03_thong_ke_gia_theo_hang_xe.csv", 30)
show_export(transmission_stats, "BANG 4. THONG KE GIA THEO HOP SO", "04_thong_ke_gia_theo_hop_so.csv")
show_export(freq_transmission, "BANG 5. PHAN PHOI HOP SO", "05_phan_phoi_hop_so.csv")
show_export(freq_fuel, "BANG 6. PHAN PHOI NHIEN LIEU", "06_phan_phoi_nhien_lieu.csv")
show_export(freq_age_group, "BANG 7. PHAN PHOI NHOM TUOI XE", "07_phan_phoi_nhom_tuoi_xe.csv")
show_export(freq_price_scale, "BANG 8. PHAN PHOI SCALE GIA", "08_phan_phoi_scale_gia.csv")
show_export(basic_prob, "BANG 9. XAC SUAT CO BAN", "09_xac_suat_co_ban.csv")
show_export(prob_transmission_by_age, "BANG 10. P(HOP SO | NHOM TUOI XE)", "10_xac_suat_hop_so_theo_nhom_tuoi.csv")
show_export(prob_fuel_by_transmission, "BANG 11. P(NHIEN LIEU | HOP SO)", "11_xac_suat_nhien_lieu_theo_hop_so.csv")
show_export(prob_high_price_by_transmission, "BANG 12. P(GIA CAO | HOP SO)", "12_xac_suat_gia_cao_theo_hop_so.csv")
show_export(prob_high_price_by_brand, "BANG 13. P(GIA CAO | HANG XE) - TOP 30", "13_xac_suat_gia_cao_theo_hang_xe.csv", 30)
show_export(joint_tab, "BANG 14. XAC SUAT KET HOP", "14_xac_suat_ket_hop_tuoi_va_hop_so.csv")
show_export(bayes_auto_new, "BANG 15. BAYES DON GIAN", "15_bayes_p_xe_moi_khi_automatic.csv")
save_csv(test_results, "16_kiem_dinh_thong_ke.csv")
print_table(test_results_fmt, "BANG 16. KIEM DINH THONG KE")

# XUẤT FILE BÁO CÁO ĐỊNH DẠNG TEXT (.TXT) TỔNG HỢP
report_path <- file.path(OUTPUT_DIR, "Bao_Cao_Xac_Suat_Thong_Ke.txt")
report_lines <- capture.output({
  cat("BAO CAO XAC SUAT - THONG KE MO TA TONG HOP\n")
  cat("Ngay chay he thong: ", as.character(Sys.Date()), "\n", sep = "")
  print_table(format_numbers_df(overview), "BANG 1. TONG QUAN DU LIEU")
  print_table(format_numbers_df(numeric_summary), "BANG 2. THONG KE MO TA TONG QUAT")
  print_table(format_numbers_df(brand_stats), "BANG 3. THONG KE GIA THEO HANG XE - TOP 30", 30)
  print_table(format_numbers_df(transmission_stats), "BANG 4. THONG KE GIA THEO HOP SO")
  print_table(format_numbers_df(basic_prob), "BANG 9. XAC SUAT CO BAN")
  print_table(format_numbers_df(prob_transmission_by_age), "BANG 10. P(HOP SO | NHOM TUOI XE)")
  print_table(format_numbers_df(prob_high_price_by_transmission), "BANG 12. P(GIA CAO | HOP SO)")
  print_table(format_numbers_df(joint_tab), "BANG 14. XAC SUAT KET HOP")
  print_table(format_numbers_df(bayes_auto_new), "BANG 15. BAYES DON GIAN")
  print_table(test_results_fmt, "BANG 16. KIEM DINH THONG KE")
})
writeLines(report_lines, report_path, useBytes = TRUE)

cat("\n=== PHÂN TÍCH THỐNG KÊ HOÀN TẤT ===\n")
cat("Tất cả tệp báo cáo đã được chuyển về vùng: ", OUTPUT_DIR, "\n", sep = "")
