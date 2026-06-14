# web-scraping-used-cars
 
- Đã xong phần data

### Cập nhật Dashboard (app.R)

- **Tích hợp Master Data:** chuyển sang đọc dữ liệu trực tiếp từ `web_scraping/data/master_data.csv`. => dashboard luôn hiển thị dữ liệu tổng hợp mới nhất từ luồng realime.
- **Phân cụm ML Động:** tạo 1 hàm `assign_clusters_vectorized` sử dụng tính toán ma trận để tự động phân cụm cho toàn bộ xe (bao gồm các xe mới cào về) dựa trên tâm cụm (cluster centers) đã lưu. tốc độ nhanh => ko cần chạy lại file ml
- **Đồng bộ Giao diện:** Cập nhật toàn bộ các thẻ (badge), menu, text báo cáo và thông báo UI từ "bonbanh" sang "Master Data + ML", đảm bảo tính nhất quán của hệ thống.
