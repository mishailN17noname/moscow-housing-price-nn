# Обучение полносвязной нейронной сети (пакет neuralnet)
source("R/05_prepare_model_data.R")

nn_data_train <- as.data.frame(cbind(y_train_s = y_train_s, X_train_s))
nn_formula <- as.formula(
  paste("y_train_s ~", paste(colnames(X_train_s), collapse = " + "))
)

cat("\nАрхитектура: Input(", ncol(X_train_s), ") -> 64 -> 32 -> Output(1)\n", sep = "")
cat("Обучение модели (может занять несколько минут)...\n")

set.seed(42)
nn_model <- neuralnet(
  nn_formula,
  data          = nn_data_train,
  hidden        = c(64, 32),
  linear.output = TRUE,   # регрессия: линейный выход
  stepmax       = 1e6,
  threshold     = 0.05
)

cat("Обучение завершено. Шагов:", nn_model$result.matrix["steps", 1],
    "| итоговая ошибка:", round(nn_model$result.matrix["error", 1], 4), "\n")

dir.create("models", showWarnings = FALSE)
saveRDS(list(model = nn_model, X_test_s = X_test_s, y_test = y_test,
             y_min = y_min, y_max = y_max), MODEL_PATH)
cat("Модель сохранена в", MODEL_PATH, "\n")
