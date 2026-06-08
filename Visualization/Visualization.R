# ==========================================
# SETUP & DATA
# ==========================================
library(ggplot2)
library(dplyr)

if (!dir.exists("plots")) {
  dir.create("plots")
}

data_mau <- read.csv("data/data_mau.csv") 

normalize_price <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(!is.na(x_num) & x_num < 1000, x_num * 100000, x_num)
}


# ==========================================
# VISUALIZATION
# ==========================================

# BIỂU ĐỒ 00: TỔNG QUAN VOLUMES & GIÁ (COMBO CHART TOP 50)

top_50_combo <- data_clean %>%
  group_by(brand) %>%
  summarise(
    so_luong = n(),
    gia_trung_vi = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(so_luong)) %>%
  head(50) %>%
  mutate(brand = factor(brand, levels = brand))

max_vol <- max(top_50_combo$so_luong, na.rm = TRUE)
max_price <- max(top_50_combo$gia_trung_vi, na.rm = TRUE)
coeff <- ifelse(max_vol == 0, 1, max_price / max_vol)

p0 <- ggplot(data = top_50_combo, aes(x = brand)) +
  
  geom_col(aes(y = so_luong), fill = "#3498db", alpha = 0.85) +
  geom_line(aes(y = gia_trung_vi / coeff, group = 1), color = "#e74c3c", size = 1.2) +
  geom_point(aes(y = gia_trung_vi / coeff), color = "#c0392b", size = 2) +
  
  scale_y_continuous(
    name = "Số lượng xe bán ra", 
    sec.axis = sec_axis(~ . * coeff, name = "Price($)", labels = scales::label_number())
  ) +
  
  labs(
    title = "[TỔNG QUAN] - Top 50 hãng xe có khối lượng giao dịch lớn nhất và xu hướng giá của chúng",
    subtitle = "Sự đối lập giữa mức độ phổ biến của hãng xe và định vị phân khúc giá trên thị trường.",
    x = "Hãng xe",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8, face = "bold"),
    
    axis.title.y = element_text(color = "#2980b9", face = "bold"),
    axis.text.y = element_text(color = "#2980b9"),
    axis.title.y.right = element_text(color = "#c0392b", face = "bold"),
    axis.text.y.right = element_text(color = "#c0392b"),
    
    panel.grid.major.x = element_blank() # Bỏ lưới dọc để biểu đồ cột nhìn sạch hơn
  )

ggsave(
  filename = "plots/00_OVERVIEW_Bar-Line_top50.png", 
  plot = p0, 
  width = 15, 
  height = 7, 
  dpi = 300
)


# WHAT - GIÁ THEO HÃNG
data_clean <- data_mau %>%
  mutate(
    price = normalize_price(price),
    brand = trimws(brand)
  ) %>%
  filter(!is.na(price), price > 0, !is.na(brand), brand != "")

brand_top <- data_clean %>%
  group_by(brand) %>%
  summarise(
    n = n(),
    med_price = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(med_price)) %>%
  mutate(
    rank = row_number(),
    brand_label = paste0(rank, ". ", brand),
    brand_label = factor(brand_label, levels = rev(brand_label))
  )

data_plot <- data_clean %>%
  inner_join(brand_top, by = "brand")

plot_height <- max(10, n_distinct(brand_top$brand) * 0.18)

# VẼ WHAT
p1 <- ggplot(
  data = data_plot,
  aes(x = brand_label, y = price, fill = brand)
) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.4) +
  coord_flip() +
  scale_y_log10(labels = scales::label_number()) +
  labs(
    title = "[WHAT] - Mức giá xe phân bổ theo các Hãng sản xuất như thế nào?",
    subtitle = "Biểu đồ cho thấy sự chênh lệch lớn về phân khúc giá giữa các thương hiệu.",
    x = "Hãng xe (Brand)",
    y = "Giá bán ($)",
    caption = ""
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  filename = "plots/01_WHAT_boxplot_gia_theo_hang.png",
  plot = p1,
  width = 10,
  height = plot_height,
  dpi = 300
)


# WHEN - XU HƯỚNG KHẤU HAO THEO NĂM
data_clean_when <- data_mau %>%
  mutate(
    price = normalize_price(price),
    year = suppressWarnings(as.numeric(as.character(year)))
  ) %>%
  filter(
    !is.na(price), price > 0,
    !is.na(year), year >= 1990, year <= as.numeric(format(Sys.Date(), "%Y"))
  )

year_summary <- data_clean_when %>%
  group_by(year) %>%
  summarise(
    med_price = median(price, na.rm = TRUE),
    .groups = "drop"
  )

# Vẽ WHEN
p2 <- ggplot(
  data = data_clean_when,
  aes(x = year, y = price)
) +
  geom_jitter(alpha = 0.18, color = "#2c3e50", width = 0.25, size = 1.1) +
  geom_smooth(
    data = year_summary,
    aes(x = year, y = med_price),
    method = "loess",
    color = "#e74c3c",
    fill = "#ffbaba",
    alpha = 0.4,
    size = 1.2
  ) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_continuous(breaks = seq(min(data_clean_when$year, na.rm = TRUE), 
                                  max(data_clean_when$year, na.rm = TRUE), by = 2)) +
  labs(
    title = "[WHEN] - Mức độ khấu hao giá xe theo thời gian diễn ra như thế nào?",
    subtitle = "Đường xu hướng (đỏ) cho thấy tốc độ mất giá trị của xe qua các năm sử dụng.",
    x = "Năm sản xuất (Year)",
    y = "Giá bán ($) - Thang đo Log",
    caption = ""
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "plots/02_WHEN_scatter_trend_khau_hao_nam.png",
  plot = p2,
  width = 10,
  height = 7,
  dpi = 300
)


#  WHY - TÁC ĐỘNG CỦA SỐ KM & HỘP SỐ
data_clean_why <- data_mau %>%
  mutate(
    price = normalize_price(price),
    mileage = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(mileage)))),
    transmission = trimws(transmission)
  ) %>%
  filter(
    !is.na(price), price > 0,
    !is.na(mileage), mileage > 0, mileage < 500000,
    !is.na(transmission), transmission %in% c("Manual", "Automatic")
  )

#Vẽ WHY
p3 <- ggplot(
  data = data_clean_why,
  aes(x = mileage, y = price, color = transmission)
) +
  geom_jitter(alpha = 0.25, size = 1.2, width = 0.2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_color_manual(values = c("Automatic" = "#e74c3c", "Manual" = "#3498db")) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_comma(suffix = " km")) +
  
  labs(
    title = "[WHY] - Số Km đã đi (Odo) và Loại hộp số tác động đến giá như thế nào?",
    subtitle = "Xe số tự động (Automatic) định vị ở phân khúc giá cao hơn và có tốc độ giữ giá khác biệt so với số sàn.",
    x = "Số Km đã đi (Mileage)",
    y = "Giá bán ($) - Thang đo Log",
    color = "Loại Hộp số:",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(
  filename = "plots/03_WHY_scatter_odo_vs_price.png",
  plot = p3,
  width = 10,
  height = 7,
  dpi = 300
)


# Chuyển đổi biểu đồ tĩnh thành đồ thịtương tác
library(plotly)
p0_interactive <- ggplotly(p0)
p1_interactive <- ggplotly(p1)
p2_interactive <- ggplotly(p2)
p3_interactive <- ggplotly(p3)

# lưu thành file .rds dành cho format Web Shiny
saveRDS(p1_interactive, file = "plots/p0_overview_interactive.rds")
saveRDS(p1_interactive, file = "plots/p1_what_interactive.rds")
saveRDS(p2_interactive, file = "plots/p2_when_interactive.rds")
saveRDS(p3_interactive, file = "plots/p3_why_interactive.rds")

p0_interactive
p1_interactive
p2_interactive
p3_interactive