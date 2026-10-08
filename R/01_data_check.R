# Проверка пропусков и приведение типов данных
source("R/00_setup.R")

df <- load_data()
cat("Размер датасета:", nrow(df), "строк,", ncol(df), "столбцов\n\n")

cat("Число пропущенных значений по каждому признаку:\n")
print(colSums(is.na(df)))
cat("\nИтого пропусков:", sum(is.na(df)), "\n\n")

cat("Типы данных до преобразования:\n")
str(df)

df$`Apartment type` <- as.factor(df$`Apartment type`)
df$Region           <- as.factor(df$Region)
df$Renovation       <- as.factor(df$Renovation)
df$`Metro station`  <- as.factor(df$`Metro station`)

cat("\nТипы данных после преобразования:\n")
str(df)
