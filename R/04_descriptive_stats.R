# Описательная статистика числовых признаков
source("R/00_setup.R")

df <- load_data()

cat("Описательная статистика (summary):\n\n")
print(summary(df[, NUM_COLS]))

describe <- function(x, label) {
  cat("\n", label, "\n", sep = "")
  cat("  Среднее    =", round(mean(x, na.rm = TRUE), 2), "\n")
  cat("  Медиана    =", round(median(x, na.rm = TRUE), 2), "\n")
  cat("  Стд. откл. =", round(sd(x, na.rm = TRUE), 2), "\n")
  cat("  Дисперсия  =", round(var(x, na.rm = TRUE), 2), "\n")
}
describe(df$Area, "Area, м²:")
describe(df$Price / 1e6, "Price, млн руб.:")
