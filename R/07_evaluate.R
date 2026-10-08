# Оценка качества модели на тестовой выборке и графики диагностики
source("R/00_setup.R")

if (!file.exists(MODEL_PATH)) stop("Сначала обучите модель: Rscript R/06_train.R")
obj <- readRDS(MODEL_PATH)
nn_model <- obj$model; X_test_s <- obj$X_test_s; y_test <- obj$y_test
y_min <- obj$y_min; y_max <- obj$y_max

predict_price <- function(X) {
  as.numeric(neuralnet::compute(nn_model, as.data.frame(X))$net.result) *
    (y_max - y_min) + y_min
}
y_pred <- predict_price(X_test_s)

mae  <- mean(abs(y_pred - y_test))
rmse <- sqrt(mean((y_pred - y_test)^2))
mape <- mean(abs((y_pred - y_test) / y_test)) * 100
r2   <- 1 - sum((y_pred - y_test)^2) / sum((y_test - mean(y_test))^2)

cat("Метрики на тестовой выборке:\n")
cat(sprintf("  MAE  = %.3f млн руб.\n", mae))
cat(sprintf("  RMSE = %.3f млн руб.\n", rmse))
cat(sprintf("  MAPE = %.1f %%\n", mape))
cat(sprintf("  R^2  = %.3f\n", r2))

# Предсказанные vs истинные значения
pred_df <- data.frame(true = y_test, pred = y_pred)
mx <- max(c(y_test, y_pred), na.rm = TRUE) * 1.05
p_pred <- ggplot(pred_df, aes(x = true, y = pred)) +
  geom_point(alpha = 0.35, size = 1.3, colour = "#76B7B2") +
  geom_abline(slope = 1, intercept = 0, colour = "#E15759",
              linewidth = 1, linetype = "dashed") +
  coord_cartesian(xlim = c(0, mx), ylim = c(0, mx)) +
  labs(title = "Предсказанные и истинные значения цены",
       x = "Истинная цена, млн руб.", y = "Предсказанная цена, млн руб.") +
  base_theme
save_plot(p_pred, "pred_vs_true.png", width = 7, height = 7)

# Распределение остатков
p_res <- ggplot(data.frame(residual = y_pred - y_test), aes(x = residual)) +
  geom_histogram(bins = 60, fill = "#59A14F", colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "#E15759", linewidth = 1, linetype = "dashed") +
  labs(title = "Распределение остатков модели",
       x = "Остаток (предсказание − истинное), млн руб.", y = "Частота") +
  base_theme
save_plot(p_res, "residuals.png")
