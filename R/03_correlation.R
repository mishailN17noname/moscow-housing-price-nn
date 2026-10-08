# Корреляционный анализ и проверка значимости корреляций
source("R/00_setup.R")

df <- load_data()

corr_mat <- cor(df[, NUM_COLS], use = "complete.obs", method = "pearson")
cat("Матрица корреляции Пирсона:\n")
print(round(corr_mat, 3))

corr_long <- melt(corr_mat)
p_corr <- ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3.5,
            colour = ifelse(abs(corr_long$value) > 0.5, "white", "black")) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Коэффициент\nкорреляции") +
  labs(title = "Матрица корреляции Пирсона", x = NULL, y = NULL) +
  base_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_corr, "correlation_matrix.png", width = 8, height = 7)

# Значимость корреляции каждого признака с ценой (alpha = 0.05)
features <- setdiff(NUM_COLS, "Price")
sig_df <- do.call(rbind, lapply(features, function(col) {
  test <- cor.test(df[[col]], df$Price, method = "pearson")
  data.frame(feature = col,
             r = round(unname(test$estimate), 3),
             p_value = formatC(test$p.value, format = "e", digits = 2),
             significant = ifelse(test$p.value < 0.05, "да", "нет"))
}))
cat("\nКорреляция признаков с Price (alpha = 0.05):\n")
print(sig_df, row.names = FALSE)
