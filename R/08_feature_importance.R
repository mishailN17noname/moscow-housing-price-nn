# Важность признаков методом перестановок (permutation importance)
source("R/00_setup.R")

obj <- readRDS(MODEL_PATH)
nn_model <- obj$model; X_test_s <- obj$X_test_s; y_test <- obj$y_test
y_min <- obj$y_min; y_max <- obj$y_max

predict_price <- function(X) {
  as.numeric(neuralnet::compute(nn_model, as.data.frame(X))$net.result) *
    (y_max - y_min) + y_min
}
baseline_mse <- mean((predict_price(X_test_s) - y_test)^2)

# Группы столбцов: one-hot-признаки одной переменной перемешиваются вместе
feature_groups <- list(
  "Area"             = "^Area$",
  "Number of rooms"  = "^Number_of_rooms",
  "Renovation"       = "^Renovation",
  "Region"           = "^Region",
  "Apartment type"   = "^Apartment_type",
  "Floor"            = "^Floor$",
  "Living area"      = "^Living_area",
  "Kitchen area"     = "^Kitchen_area",
  "Minutes to metro" = "^Minutes_to_metro",
  "Number of floors" = "^Number_of_floors"
)

set.seed(42)
perm_imp <- sapply(feature_groups, function(pattern) {
  cols <- grep(pattern, colnames(X_test_s), value = TRUE)
  if (length(cols) == 0) return(0)
  X_perm <- X_test_s
  X_perm[, cols] <- X_perm[sample(nrow(X_perm)), cols]
  max(mean((predict_price(X_perm) - y_test)^2) - baseline_mse, 0)
})
imp_df <- data.frame(feature = names(perm_imp),
                     importance = perm_imp / sum(perm_imp))
imp_df <- imp_df[imp_df$importance > 0, ]
print(imp_df[order(-imp_df$importance), ], row.names = FALSE)

p_imp <- ggplot(imp_df, aes(x = importance, y = reorder(feature, importance))) +
  geom_col(fill = "#4E79A7", colour = "white") +
  geom_text(aes(label = sprintf("%.3f", importance)), hjust = -0.15, size = 3.2) +
  scale_x_continuous(limits = c(0, max(imp_df$importance) * 1.15), expand = c(0, 0)) +
  labs(title = "Важность признаков (permutation importance)",
       x = "Нормализованная важность", y = NULL) +
  base_theme
save_plot(p_imp, "feature_importance.png", height = 4)
