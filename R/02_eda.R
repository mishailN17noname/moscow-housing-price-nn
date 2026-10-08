# Разведочный анализ: распределения и выбросы
source("R/00_setup.R")

df <- load_data()

# Гистограмма цены (без объектов выше 99-го перцентиля)
df_clean <- df %>%
  filter(Price <= quantile(Price, 0.99, na.rm = TRUE)) %>%
  mutate(PriceM = Price / 1e6)

p_hist <- ggplot(df_clean, aes(x = PriceM)) +
  geom_histogram(bins = 50, fill = "#4E79A7", colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = median(df_clean$PriceM),
             colour = "#E15759", linewidth = 1, linetype = "dashed") +
  labs(title = "Распределение цен на недвижимость",
       x = "Цена, млн руб.", y = "Количество объектов") +
  base_theme
save_plot(p_hist, "price_hist.png")

# Боксплот площади по типу жилья
df_box <- df %>% filter(Area <= quantile(Area, 0.99, na.rm = TRUE))
p_box <- ggplot(df_box, aes(x = `Apartment type`, y = Area, fill = `Apartment type`)) +
  geom_boxplot(width = 0.55, outlier.size = 1.5, outlier.alpha = 0.4,
               outlier.colour = "gray40") +
  scale_fill_manual(values = c("Secondary" = "#F28E2B", "New building" = "#76B7B2")) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Площадь по типам жилья", x = "Тип жилья", y = "Общая площадь, м²") +
  base_theme + theme(legend.position = "none")
save_plot(p_box, "area_boxplot.png")

# Диаграмма рассеяния «площадь – цена» (случайные 3000 объектов)
set.seed(42)
df_sc <- df %>%
  filter(Price <= quantile(Price, 0.99, na.rm = TRUE), Area <= 250) %>%
  mutate(PriceM = Price / 1e6)
df_sc <- df_sc[sample(nrow(df_sc), min(3000, nrow(df_sc))), ]
p_sc <- ggplot(df_sc, aes(x = Area, y = PriceM)) +
  geom_point(alpha = 0.4, size = 1.5, colour = "#59A14F") +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#E15759", se = FALSE, linewidth = 1) +
  labs(title = "Зависимость цены от площади",
       x = "Общая площадь, м²", y = "Цена, млн руб.") +
  base_theme
save_plot(p_sc, "area_vs_price.png")

# Распределение объектов по типу ремонта
ren_df <- df %>% count(Renovation) %>% arrange(desc(n))
p_ren <- ggplot(ren_df, aes(x = reorder(Renovation, -n), y = n, fill = Renovation)) +
  geom_col(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  labs(title = "Распределение объектов по типу ремонта",
       x = "Тип ремонта", y = "Количество объектов") +
  base_theme +
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(p_ren, "renovation.png")

cat("99-й перцентиль цены:", round(quantile(df$Price, 0.99) / 1e6, 1), "млн руб.\n")
cat("Графики сохранены в папку", FIG_DIR, "\n")
