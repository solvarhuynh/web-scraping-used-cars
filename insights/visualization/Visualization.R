# ==============================================================================
# Script: Visualization.R
# Purpose: Generate market visualizations from repository master data
# Output: insights/visualization/plots
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(plotly)
  library(scales)
})

source("web_scraping/script/utils.R")

PLOT_DIR <- "insights/visualization/plots"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

normalize_transmission_vis <- function(x) {
  y <- str_to_lower(str_squish(as.character(x)))
  dplyr::case_when(
    y %in% c("automatic", "auto", "at", "số tự động", "so tu dong", "tự động", "tu dong") ~ "Tự động",
    y %in% c("manual", "mt", "số sàn", "so san", "sàn", "san", "số tay") ~ "Số sàn",
    y == "cvt" ~ "CVT",
    TRUE ~ NA_character_
  )
}

data_raw <- read_master_data()

data_clean <- data_raw %>%
  mutate(
    price = suppressWarnings(as.numeric(price)),
    year = suppressWarnings(as.integer(year)),
    mileage = suppressWarnings(as.numeric(mileage)),
    brand = str_squish(as.character(brand)),
    body_type = str_squish(as.character(body_type)),
    fuel_type = str_squish(as.character(fuel_type)),
    city = str_squish(as.character(city)),
    transmission = normalize_transmission_vis(transmission)
  ) %>%
  filter(
    !is.na(price), price >= 5e7, price <= 1.5e10,
    !is.na(brand), brand != "",
    !is.na(year), year >= 1990, year <= as.integer(format(Sys.Date(), "%Y"))
  )

if (!nrow(data_clean)) {
  stop("No valid data available for visualization.")
}

cat("Đã nạp dữ liệu visualization:", nrow(data_clean), "dòng.\n")

# 00. Overview volume and median price by brand
top_50_combo <- data_clean %>%
  group_by(brand) %>%
  summarise(
    so_luong = n(),
    gia_trung_vi = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(so_luong)) %>%
  slice_head(n = 50) %>%
  mutate(brand = factor(brand, levels = brand))

coeff <- max(top_50_combo$gia_trung_vi, na.rm = TRUE) / max(top_50_combo$so_luong, na.rm = TRUE)
if (!is.finite(coeff) || coeff <= 0) coeff <- 1

p0 <- ggplot(top_50_combo, aes(x = brand)) +
  geom_col(aes(y = so_luong), fill = "#2f80ed", alpha = 0.86) +
  geom_line(aes(y = gia_trung_vi / coeff, group = 1), color = "#d94841", linewidth = 1.1) +
  geom_point(aes(y = gia_trung_vi / coeff), color = "#9d2f2a", size = 2) +
  scale_y_continuous(
    name = "Số lượng xe rao bán",
    sec.axis = sec_axis(~ . * coeff, name = "Giá trung vị (VNĐ)", labels = label_number())
  ) +
  labs(
    title = "[TỔNG QUAN] Top 50 hãng xe theo lượng tin và giá trung vị",
    x = "Hãng xe"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8, face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(PLOT_DIR, "00_OVERVIEW_Bar-Line_top50.png"), p0, width = 15, height = 7, dpi = 300)

# 01. Brand price boxplot
brand_top <- data_clean %>%
  group_by(brand) %>%
  summarise(n = n(), med_price = median(price, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n), desc(med_price)) %>%
  slice_head(n = 50) %>%
  arrange(desc(med_price)) %>%
  mutate(brand_label = factor(paste0(row_number(), ". ", brand), levels = rev(paste0(row_number(), ". ", brand))))

data_plot1 <- data_clean %>% inner_join(brand_top %>% select(brand, brand_label), by = "brand")
plot_height <- max(10, n_distinct(data_plot1$brand) * 0.22)

p1 <- ggplot(data_plot1, aes(x = brand_label, y = price, fill = brand)) +
  geom_boxplot(alpha = 0.72, outlier.colour = "#d94841", outlier.shape = 1, outlier.alpha = 0.35) +
  coord_flip() +
  scale_y_log10(labels = label_number()) +
  labs(
    title = "[WHAT] Phân bổ giá theo hãng xe",
    x = "Hãng xe",
    y = "Giá bán (VNĐ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(PLOT_DIR, "01_WHAT_boxplot_gia_theo_hang.png"), p1, width = 10, height = plot_height, dpi = 300)

# 02. Year-price trend
year_summary <- data_clean %>%
  group_by(year) %>%
  summarise(med_price = median(price, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(data_clean, aes(x = year, y = price)) +
  geom_jitter(alpha = 0.18, color = "#26374a", width = 0.25, size = 1.1) +
  geom_smooth(data = year_summary, aes(x = year, y = med_price), method = "loess",
              color = "#d94841", fill = "#f5a6a1", alpha = 0.35, linewidth = 1.1) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(breaks = seq(min(data_clean$year, na.rm = TRUE), max(data_clean$year, na.rm = TRUE), by = 2)) +
  labs(
    title = "[WHEN] Xu hướng giá xe cũ theo năm sản xuất",
    x = "Năm sản xuất",
    y = "Giá bán (VNĐ)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14), axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(PLOT_DIR, "02_WHEN_scatter_trend_khau_hao_nam.png"), p2, width = 10, height = 7, dpi = 300)

# 03. Mileage-price by transmission
data_plot3 <- data_clean %>%
  filter(!is.na(mileage), mileage > 0, mileage <= 500000, transmission %in% c("Tự động", "Số sàn"))

p3 <- ggplot(data_plot3, aes(x = mileage, y = price, color = transmission)) +
  geom_jitter(alpha = 0.25, size = 1.15, width = 0.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.25) +
  scale_color_manual(values = c("Tự động" = "#d94841", "Số sàn" = "#2f80ed")) +
  scale_y_log10(labels = label_number()) +
  scale_x_continuous(labels = label_comma(suffix = " km")) +
  labs(
    title = "[WHY] Số km và hộp số tác động đến giá",
    x = "Số km đã đi",
    y = "Giá bán (VNĐ)",
    color = "Hộp số"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")

ggsave(file.path(PLOT_DIR, "03_WHY_scatter_odo_vs_price.png"), p3, width = 10, height = 7, dpi = 300)

# 04-06. Distribution charts
plot_distribution <- function(data, col, title, fill, filename) {
  df <- data %>%
    filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
    count(.data[[col]], sort = TRUE) %>%
    slice_head(n = 20) %>%
    mutate(name = factor(.data[[col]], levels = rev(.data[[col]])))

  p <- ggplot(df, aes(x = name, y = n)) +
    geom_col(fill = fill, alpha = 0.88) +
    coord_flip() +
    labs(title = title, x = NULL, y = "Số lượng") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14), panel.grid.major.y = element_blank())

  ggsave(file.path(PLOT_DIR, filename), p, width = 9, height = 6, dpi = 300)
  p
}

p4 <- plot_distribution(data_clean, "fuel_type", "[DISTRIBUTION] Phân bổ nhiên liệu", "#22a06b", "04_DIST_fuel_type.png")
p5 <- plot_distribution(data_clean, "body_type", "[DISTRIBUTION] Phân bổ kiểu dáng", "#8a63d2", "05_DIST_body_type.png")
p6 <- plot_distribution(data_clean, "city", "[DISTRIBUTION] Top tỉnh/thành theo lượng tin", "#f59f00", "06_DIST_city.png")

p0_interactive <- ggplotly(p0)
p1_interactive <- ggplotly(p1)
p2_interactive <- ggplotly(p2)
p3_interactive <- ggplotly(p3)
p4_interactive <- ggplotly(p4)
p5_interactive <- ggplotly(p5)
p6_interactive <- ggplotly(p6)

saveRDS(p0_interactive, file.path(PLOT_DIR, "p0_overview_interactive.rds"))
saveRDS(p1_interactive, file.path(PLOT_DIR, "p1_what_interactive.rds"))
saveRDS(p2_interactive, file.path(PLOT_DIR, "p2_when_interactive.rds"))
saveRDS(p3_interactive, file.path(PLOT_DIR, "p3_why_interactive.rds"))
saveRDS(p4_interactive, file.path(PLOT_DIR, "p4_fuel_interactive.rds"))
saveRDS(p5_interactive, file.path(PLOT_DIR, "p5_body_interactive.rds"))
saveRDS(p6_interactive, file.path(PLOT_DIR, "p6_city_interactive.rds"))

cat("Visualization outputs saved to:", PLOT_DIR, "\n")
