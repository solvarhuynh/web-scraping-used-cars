suppressPackageStartupMessages({
  library(rpart)
})

df_tree <- df[complete.cases(df[, c("price_segment", "car_age", "mileage_k",
                                     "engine_size", "is_auto", "is_imported",
                                     "seat_count")]),
              c("price_segment", "car_age", "mileage_k", "engine_size",
                "is_auto", "is_imported", "seat_count")]
df_tree$price_segment <- factor(df_tree$price_segment,
  levels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp"))
df_tree <- df_tree[!is.na(df_tree$price_segment), ]

stratified_train_index <- function(y, p = 0.8) {
  idx_by_class <- split(seq_along(y), y)
  unlist(lapply(idx_by_class, function(idx) {
    n_train <- max(1, floor(length(idx) * p))
    if (n_train >= length(idx)) n_train <- max(1, length(idx) - 1)
    sample(idx, n_train)
  }), use.names = FALSE)
}

set.seed(42)
idx_tree <- stratified_train_index(df_tree$price_segment, p = 0.8)
train_tree <- df_tree[idx_tree, ]
test_tree <- df_tree[-idx_tree, ]

model_tree <- rpart(
  price_segment ~ .,
  data = train_tree,
  method = "class",
  control = rpart.control(minsplit = 30, minbucket = 10, maxdepth = 6, cp = 0.001)
)
best_cp <- model_tree$cptable[which.min(model_tree$cptable[, "xerror"]), "CP"]
model_tree <- prune(model_tree, cp = best_cp)

tree_pred <- predict(model_tree, newdata = test_tree, type = "class")
levels_all <- levels(df_tree$price_segment)
conf_mat <- table(
  Prediction = factor(tree_pred, levels = levels_all),
  Reference = factor(test_tree$price_segment, levels = levels_all)
)

total <- sum(conf_mat)
tree_accuracy <- round(sum(diag(conf_mat)) / total, 4)
expected_accuracy <- sum(rowSums(conf_mat) * colSums(conf_mat)) / (total ^ 2)
tree_kappa <- round((tree_accuracy - expected_accuracy) / (1 - expected_accuracy), 4)

conf_table <- as.data.frame(conf_mat)
names(conf_table) <- c("du_doan", "thuc_te", "so_lan")

importance <- model_tree$variable.importance
if (is.null(importance)) {
  importance <- setNames(rep(0, 6), c("car_age", "mileage_k", "engine_size", "is_auto", "is_imported", "seat_count"))
}

feat_imp <- data.frame(
  feature = names(importance),
  importance = as.numeric(importance)
)
feat_imp$importance_pct <- if (sum(feat_imp$importance) > 0) {
  round(feat_imp$importance / sum(feat_imp$importance) * 100, 1)
} else {
  0
}
feat_imp$feature_vn <- c(
  car_age = "Tuổi xe",
  mileage_k = "Số km đã đi",
  engine_size = "Dung tích động cơ",
  is_auto = "Hộp số tự động",
  is_imported = "Nhập khẩu",
  seat_count = "Số chỗ ngồi"
)[feat_imp$feature]
feat_imp <- feat_imp[order(-feat_imp$importance), ]

class_metrics <- do.call(rbind, lapply(levels_all, function(cls) {
  tp <- conf_mat[cls, cls]
  fn <- sum(conf_mat[, cls]) - tp
  fp <- sum(conf_mat[cls, ]) - tp
  tn <- total - tp - fn - fp
  sensitivity <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  specificity <- ifelse(tn + fp == 0, NA_real_, tn / (tn + fp))
  data.frame(
    Sensitivity = round(sensitivity, 4),
    Specificity = round(specificity, 4),
    `Balanced Accuracy` = round(mean(c(sensitivity, specificity), na.rm = TRUE), 4),
    class = cls,
    check.names = FALSE
  )
}))
