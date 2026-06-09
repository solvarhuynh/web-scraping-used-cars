suppressPackageStartupMessages({
  library(rpart)
  library(caret)
})

df_tree <- df[complete.cases(df[, c("price_segment", "car_age", "mileage_k",
                                     "engine_size", "is_auto", "is_imported",
                                     "seat_count")]),
              c("price_segment", "car_age", "mileage_k", "engine_size",
                "is_auto", "is_imported", "seat_count")]
df_tree$price_segment <- factor(df_tree$price_segment,
  levels = c("Phổ thông", "Tầm trung", "Khá", "Cao cấp"))

set.seed(42)
idx_tree   <- createDataPartition(df_tree$price_segment, p = 0.8, list = FALSE)
train_tree <- df_tree[idx_tree, ]
test_tree  <- df_tree[-idx_tree, ]

model_tree <- rpart(
  price_segment ~ .,
  data    = train_tree,
  method  = "class",
  control = rpart.control(minsplit = 30, minbucket = 10, maxdepth = 6, cp = 0.001)
)
best_cp    <- model_tree$cptable[which.min(model_tree$cptable[, "xerror"]), "CP"]
model_tree <- prune(model_tree, cp = best_cp)

tree_pred <- predict(model_tree, newdata = test_tree, type = "class")
conf_mat  <- confusionMatrix(tree_pred, test_tree$price_segment)

tree_accuracy <- round(conf_mat$overall["Accuracy"], 4)
tree_kappa    <- round(conf_mat$overall["Kappa"], 4)

conf_table <- as.data.frame(conf_mat$table)
names(conf_table) <- c("du_doan", "thuc_te", "so_lan")

feat_imp <- data.frame(
  feature    = names(model_tree$variable.importance),
  importance = as.numeric(model_tree$variable.importance)
)
feat_imp$importance_pct <- round(feat_imp$importance / sum(feat_imp$importance) * 100, 1)
feat_imp$feature_vn <- c(
  car_age     = "Tuổi xe",
  mileage_k   = "Số km đã đi",
  engine_size = "Dung tích động cơ",
  is_auto     = "Hộp số tự động",
  is_imported = "Nhập khẩu",
  seat_count  = "Số chỗ ngồi"
)[feat_imp$feature]
feat_imp <- feat_imp[order(-feat_imp$importance), ]

class_metrics <- as.data.frame(conf_mat$byClass)[, c("Sensitivity", "Specificity", "Balanced Accuracy")]
class_metrics  <- round(class_metrics, 4)
class_metrics$class <- rownames(class_metrics)
