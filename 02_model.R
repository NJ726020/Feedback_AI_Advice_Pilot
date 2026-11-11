# =============================================================================
# 02_model.R  —  Train the logistic regression and check its performance
# -----------------------------------------------------------------------------
# Wang & Yin (2022, Sec. 3.2.1): logistic regression trained on a subset of
# COMPAS; on 1,000 hold-out cases accuracy = 69.1%, AUC = 0.749.
# Numeric features enter linearly; categorical features use the reference
# defendant's value as their reference level (set in 01_data.R).
#
# Input:  data/compas_clean.rds
# Output: data/model.rds, data/test_predictions.rds
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

d     <- readRDS("data/compas_clean.rds")
train <- filter(d, set == "train")
test  <- filter(d, set == "test")

model <- glm(reoffend ~ race + sex + age + prior_count + charge_name +
               charge_degree + days_in_custody,
             data = train, family = binomial())

# --- Performance on the hold-out test set ----------------------------------------
test$p_reoffend <- predict(model, newdata = test, type = "response")
test$prediction <- as.integer(test$p_reoffend > 0.5)
test$correct    <- test$prediction == test$reoffend

# AUC via the rank (Mann-Whitney) formula, so no extra package is needed
auc <- function(score, label) {
  r  <- rank(score)
  n1 <- sum(label == 1); n0 <- sum(label == 0)
  (sum(r[label == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

cat(sprintf("Test accuracy: %.1f%%  (Wang & Yin: 69.1%%)\n", 100 * mean(test$correct)))
cat(sprintf("Test AUC:      %.3f   (Wang & Yin: 0.749)\n", auc(test$p_reoffend, test$reoffend)))

# --- Confidence -----------------------------------------------------------------
# Confidence = probability of the predicted label (range 0.5-1).
# High confidence: > 0.7 (Wang & Yin: 35.6% of their test set).
# Risk is differently categorized by the Users of the COMPAS data. 
test <- test %>%
  mutate(confidence = pmax(p_reoffend, 1 - p_reoffend),
         conf_group = if_else(confidence > 0.7, "high", "low"))

calib <- test %>%
  group_by(conf_group) %>%
  summarise(n = n(), share = n() / nrow(test), accuracy = mean(correct), .groups = "drop")
cat("\nConfidence groups in the test set (accuracy should be higher for 'high'):\n")
print(as.data.frame(calib), digits = 3)

# Coefficients (log-odds) — these are what the explanation charts display
cat("\nCoefficients (log-odds):\n")
print(round(coef(model), 3))

saveRDS(model, "data/model.rds")
saveRDS(test,  "data/test_predictions.rds")
