# =============================================================================
# 03_select_trials.R  —  Draw 40 random trials from the test set
# -----------------------------------------------------------------------------
# Unlike Wang & Yin (2021), trials are NOT selected by model confidence.
# 40 cases are drawn at random from the 1,000 hold-out test cases, so the trial
# set reflects the model's normal behaviour.
#
# stratify = TRUE (default): stratified random sampling. Cases are drawn at
#   random within the four groups "reoffended yes/no x model correct yes/no",
#   in the same proportions as in the test set. The 40 trials then have
#   (almost exactly) the test set's base rate and model accuracy.
# stratify = FALSE: simple random sample; base rate and accuracy can then
#   differ from the test set by chance.
#
# Input:  data/test_predictions.rds
# Output: data/trials.csv, data/trials.rds
# =============================================================================

suppressPackageStartupMessages(library(dplyr))
set.seed(720)

test     <- readRDS("data/test_predictions.rds")
n_trials <- 40
stratify <- TRUE

if (stratify) {
  # Number of cases per group, proportional to the test set (largest remainder)
  cells <- test %>%
    count(reoffend, correct) %>%
    mutate(exact = n_trials * n / sum(n), k = floor(exact))
  left <- n_trials - sum(cells$k)
  top  <- order(cells$exact - cells$k, decreasing = TRUE)[seq_len(left)]
  cells$k[top] <- cells$k[top] + 1

  picked <- bind_rows(lapply(seq_len(nrow(cells)), function(j) {
    test %>%
      filter(reoffend == cells$reoffend[j], correct == cells$correct[j]) %>%
      slice_sample(n = cells$k[j])
  }))
} else {
  picked <- slice_sample(test, n = n_trials)
}

# Shuffle so trial IDs do not reveal outcome or model correctness
trials <- picked %>%
  slice_sample(prop = 1) %>%
  mutate(trial_id = sprintf("T%02d", row_number())) %>%
  select(trial_id, id, confidence, conf_group, p_reoffend, prediction, reoffend,
         correct, race, sex, age, prior_count, charge_name, charge_label,
         charge_label_raw, charge_degree, days_in_custody)

# --- Check: are the 40 trials representative of the test set? --------------------
summ <- function(d) c(n = nrow(d), base_rate = mean(d$reoffend), ai_accuracy = mean(d$correct),
                      ai_predicts_yes = mean(d$prediction), high_conf = mean(d$conf_group == "high"))
cat("Trials vs test set:\n")
print(round(rbind(trials = summ(trials), test_set = summ(test)), 3))
cat("\nAI errors in the trials:", sum(!trials$correct), "of", nrow(trials), "\n")
cat("Charges in the trials:\n"); print(sort(table(trials$charge_label), decreasing = TRUE))

write.csv(trials, "data/trials.csv", row.names = FALSE)
saveRDS(trials, "data/trials.rds")
