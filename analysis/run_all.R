# Runs the whole survey analysis. Works from anywhere inside the project
# (paths are resolved from Feedback_AI_Advice.Rproj via here).
library(here)
here::i_am("analysis/run_all.R")
source(here("analysis", "01_import_clean.R"))
for (f in c("02_pilot_analysis.Rmd", "03_exploratory_analysis.Rmd"))
  rmarkdown::render(here("analysis", f), output_dir = here("analysis", "reports"),
                    envir = new.env(), quiet = TRUE)
cat("Reports written to analysis/reports/\n")
