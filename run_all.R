# Полный запуск проекта: Rscript run_all.R
scripts <- c("R/01_data_check.R", "R/02_eda.R", "R/03_correlation.R",
             "R/04_descriptive_stats.R", "R/06_train.R",
             "R/07_evaluate.R", "R/08_feature_importance.R")
for (s in scripts) {
  cat("\n==========", s, "==========\n")
  source(s, local = new.env())
}
