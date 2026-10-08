# Общие настройки проекта: пакеты, пути, загрузка данных.
# Подключается в начале каждого скрипта: source("R/00_setup.R")

required <- c("dplyr", "ggplot2", "reshape2", "caret", "neuralnet")
missing <- required[!required %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(caret)
  library(neuralnet)
})

DATA_PATH   <- "data/data.csv"
FIG_DIR     <- "figures"
MODEL_PATH  <- "models/nn_model.rds"

NUM_COLS <- c("Price", "Minutes to metro", "Number of rooms",
              "Area", "Living area", "Kitchen area",
              "Floor", "Number of floors")

load_data <- function(path = DATA_PATH) {
  if (!file.exists(path)) {
    stop("Не найден ", path, ". Скачайте датасет по инструкции в data/README.md")
  }
  read.csv(path, check.names = FALSE)
}

save_plot <- function(plot, name, width = 8, height = 5) {
  dir.create(FIG_DIR, showWarnings = FALSE)
  ggsave(file.path(FIG_DIR, name), plot, width = width, height = height, dpi = 150)
}

base_theme <- theme_gray(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
