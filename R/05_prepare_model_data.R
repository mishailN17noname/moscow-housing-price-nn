# Подготовка данных для модели: удаление выбросов, one-hot-кодирование,
# разбиение 80/20 и Min-Max-нормализация.
# Скрипт создаёт объекты X_train_s, X_test_s, y_train, y_test, y_min, y_max.
source("R/00_setup.R")

set.seed(42)
df <- load_data()
df$`Apartment type` <- as.factor(df$`Apartment type`)
df$Region           <- as.factor(df$Region)
df$Renovation       <- as.factor(df$Renovation)

df_model <- df %>%
  filter(Price <= quantile(Price, 0.99, na.rm = TRUE)) %>%
  mutate(PriceM = Price / 1e6) %>%
  select(PriceM, `Apartment type`, `Minutes to metro`, Region,
         `Number of rooms`, Area, `Living area`, `Kitchen area`,
         Floor, `Number of floors`, Renovation) %>%
  na.omit()

# One-hot-кодирование категориальных признаков
dummy_model <- dummyVars("~.", data = df_model %>% select(-PriceM))
X_full <- as.data.frame(predict(dummy_model, df_model %>% select(-PriceM)))

# Имена столбцов без пробелов и спецсимволов (требование формулы neuralnet)
colnames(X_full) <- make.names(colnames(X_full), unique = TRUE)
colnames(X_full) <- gsub("\\.+", "_", colnames(X_full))
colnames(X_full) <- gsub("_+$", "", colnames(X_full))
y_full <- df_model$PriceM

# Разбиение 80/20
idx <- createDataPartition(y_full, p = 0.80, list = FALSE)
X_train <- X_full[idx, ];  X_test <- X_full[-idx, ]
y_train <- y_full[idx];    y_test <- y_full[-idx]

# Min-Max-нормализация признаков (параметры считаются только по обучающей выборке)
preproc   <- preProcess(X_train, method = "range")
X_train_s <- as.matrix(predict(preproc, X_train))
X_test_s  <- as.matrix(predict(preproc, X_test))

# Нормализация целевой переменной
y_min <- min(y_train); y_max <- max(y_train)
y_train_s <- (y_train - y_min) / (y_max - y_min)

cat("Всего наблюдений:       ", nrow(df_model), "\n")
cat("Обучающая выборка (80%):", nrow(X_train), "\n")
cat("Тестовая выборка (20%): ", nrow(X_test), "\n")
cat("Число признаков:        ", ncol(X_train), "\n")
