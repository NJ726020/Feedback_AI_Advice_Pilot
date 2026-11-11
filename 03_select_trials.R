# =============================================================================
# 03_select_trials.R  —  Select 16 high- and 16 low-confidence trials
# -----------------------------------------------------------------------------
# Wang & Yin (2022, Sec. 3.2.3): test cases were projected onto the two most
# predictive features (prior count and age). Within the low- and the
# high-confidence group, 16 "prototypes" were chosen that cover the centres of
# that group's distribution (MMD-critic; Kim, Khanna & Koyejo, 2016).
# This script implements greedy MMD-critic prototype selection in base R.
#
# Input:  data/test_predictions.rds
# Output: data/trials.csv (32 trials), data/trials.rds
# =============================================================================

suppressPackageStartupMessages(library(dplyr))
set.seed(72)

test <- readRDS("data/test_predictions.rds")
n_per_group <- 16

# Wang & Yin only report selecting prototypes. With 16 trials per group, the
# model's accuracy on the selected trials then varies a lot by chance, and the
# confidence cue can become invalid (e.g., equal accuracy for high and low).
# match_accuracy = TRUE selects prototypes separately among correct and
# incorrect cases, so each group's accuracy matches the test-set accuracy of
# that confidence group. Set to FALSE for the original procedure.
match_accuracy <- TRUE

# --- Greedy MMD-critic prototype selection ----------------------------------------
# Picks points whose distribution best matches the full group (maximum mean
# discrepancy with an RBF kernel). Features are standardised first.
select_prototypes <- function(X, m) {
  X  <- scale(X)
  D2 <- as.matrix(dist(X))^2
  sigma2 <- median(D2[upper.tri(D2)])          # median heuristic for kernel width
  K  <- exp(-D2 / sigma2)
  n  <- nrow(K)
  colmean <- colMeans(K)                       # mean similarity of each point to all points
  S <- integer(0)
  for (step in seq_len(m)) {
    cand <- setdiff(seq_len(n), S)
    gain <- vapply(cand, function(j) {
      Sj <- c(S, j)
      2 * mean(colmean[Sj]) - mean(K[Sj, Sj])  # MMD objective J(S) to maximise
    }, numeric(1))
    S <- c(S, cand[which.max(gain)])
  }
  S
}

proto <- function(g, m) {
  idx <- select_prototypes(as.matrix(g[, c("prior_count", "age")]), m)
  g[idx, ]
}

pick <- function(group) {
  g <- filter(test, conf_group == group)
  if (!match_accuracy) return(proto(g, n_per_group))
  n_correct <- round(n_per_group * mean(g$correct))
  bind_rows(proto(filter(g, correct), n_correct),
            proto(filter(g, !correct), n_per_group - n_correct))
}

# Shuffle so trial IDs do not reveal confidence group or correctness
trials <- bind_rows(pick("high"), pick("low")) %>%
  slice_sample(prop = 1) %>%
  mutate(trial_id = sprintf("T%02d", row_number())) %>%
  select(trial_id, id, conf_group, confidence, p_reoffend, prediction, reoffend,
         correct, race, sex, age, prior_count, charge_name, charge_label, charge_degree,
         days_in_custody)

# --- Checks ----------------------------------------------------------------------
cat("Model accuracy on the selected trials:\n")
print(as.data.frame(trials %>% group_by(conf_group) %>%
        summarise(n = n(), accuracy = mean(correct),
                  mean_conf = mean(confidence), .groups = "drop")), digits = 3)
cat(sprintf("Overall accuracy on 32 trials: %.1f%%\n", 100 * mean(trials$correct)))
cat("Predicted 'will reoffend':", sum(trials$prediction), "of 32\n")
cat("Actually re-offended:     ", sum(trials$reoffend), "of 32\n")

# The AI's confidence should be a valid cue: accuracy on high-confidence trials
# should clearly exceed accuracy on low-confidence trials. With only 16 trials
# per group, the gap can be small by chance; if so, try another seed in 01_data.R.
gap <- with(trials, mean(correct[conf_group == "high"]) - mean(correct[conf_group == "low"]))
if (gap < 0.10) warning(sprintf(
  "Accuracy gap high vs. low confidence is only %.0f points; consider another seed.", 100 * gap))

write.csv(trials, "data/trials.csv", row.names = FALSE)
saveRDS(trials, "data/trials.rds")
