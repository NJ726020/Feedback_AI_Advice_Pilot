# =============================================================================
# 04_explanations.R  —  Feature-contribution explanations for each trial
# -----------------------------------------------------------------------------
# Wang & Yin (2022, Sec. 3.2.2) showed each feature's contribution in log-odds,
# relative to a reference defendant (White, female, 31, misdemeanor, arrest
# case no charge, 0 priors, 0 days in custody); the grey "base rate" bar was
# the log-odds of that reference defendant.
#
# This version shows the same explanation on the PROBABILITY scale (percentage
# points), which is easier to read:
#   - Grey bar "Starting point": the model's predicted chance of reoffending
#     for the reference defendant.
#   - Red/blue bars: how much each feature raises/lowers that chance.
#   - Starting point + all bars = the model's predicted chance for this person.
# Contributions are exact baseline Shapley values on the probability output
# (Sundararajan & Najmi, 2020), with the reference defendant as baseline.
# Because the model is additive in log-odds, every bar keeps the sign of the
# original log-odds contribution; only the sizes are converted.
# Set scale <- "logodds" to reproduce Wang & Yin's original charts.
#
# Input:  data/model.rds, data/trials.rds
# Output: stimuli/T01/explanation.png ... stimuli/T40/explanation.png,
#         stimuli/example/explanation.png (instructions example; not a trial),
#         data/contributions.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

scale <- "logodds"                                # "logodds" (as Wang & Yin) or "probability"
start_label <- "base case"                        # label of the grey bar (Wang & Yin: "base rate")

model  <- readRDS("data/model.rds")
trials <- readRDS("data/trials.rds")
b      <- coef(model)
dir.create("stimuli", showWarnings = FALSE)

ref_age <- 31                                     # reference defendant's age
coef_or_zero <- function(name) if (name %in% names(b)) unname(b[name]) else 0

# --- Log-odds contributions (as in Wang & Yin) ---------------------------------------
logodds_contributions <- function(row) {
  tibble(
    feature = c(
      paste("Race -", row$race),
      paste("Gender -", row$sex),
      paste("Age -", row$age),
      paste("Prior Count -", row$prior_count),
      paste("Charge Name -", row$charge_label),
      paste("Charge Degree -", row$charge_degree),
      paste("Days in Custody -", row$days_in_custody)
    ),
    value = c(
      coef_or_zero(paste0("race", row$race)),
      coef_or_zero(paste0("sex", row$sex)),
      b["age"] * (row$age - ref_age),
      b["prior_count"] * row$prior_count,
      coef_or_zero(paste0("charge_name", row$charge_name)),
      coef_or_zero(paste0("charge_degree", row$charge_degree)),
      b["days_in_custody"] * row$days_in_custody
    )
  )
}
start_logodds <- unname(b["(Intercept)"] + b["age"] * ref_age)

# --- Baseline Shapley values on the probability scale ------------------------------
# Value of a coalition S = predicted probability when only the features in S
# take this person's values (others stay at the reference defendant's values).
shapley_prob <- function(c_lo, L0) {
  n <- length(c_lo); phi <- numeric(n)
  subsets <- expand.grid(rep(list(c(FALSE, TRUE)), n - 1))
  for (i in seq_len(n)) {
    others <- c_lo[-i]
    for (s in seq_len(nrow(subsets))) {
      S <- unlist(subsets[s, ]); k <- sum(S)
      w <- factorial(k) * factorial(n - k - 1) / factorial(n)
      base <- L0 + sum(others[S])
      phi[i] <- phi[i] + w * (plogis(base + c_lo[i]) - plogis(base))
    }
  }
  phi
}

contributions <- function(row) {
  lo <- logodds_contributions(row)
  if (scale == "logodds") {
    contr <- lo; start <- start_logodds
  } else {
    contr <- mutate(lo, value = 100 * shapley_prob(lo$value, start_logodds))
    start <- 100 * plogis(start_logodds)
  }
  contr %>%
    arrange(desc(value)) %>%
    bind_rows(tibble(feature = start_label, value = start))
}

# --- Chart in the style of Wang & Yin (Fig. 1c) ------------------------------------
plot_explanation <- function(contr, lims) {
  if (is.null(lims)) {                            # log-odds: symmetric axis per chart, at least -2..2
    lim <- max(2, ceiling(max(abs(contr$value)))); lims <- c(-lim, lim)
  }
  contr <- contr %>%
    mutate(feature = factor(feature, levels = rev(feature)),
           fill = case_when(feature == start_label ~ "base",
                            value >= 0             ~ "up",
                            TRUE                   ~ "down"))
  step <- if (scale == "logodds") 1 else 20
  labs_fun <- if (scale == "logodds") waiver() else
    function(x) ifelse(x > 0, paste0("+", x, "%"), paste0(x, "%"))
  ggplot(contr, aes(x = value, y = feature, fill = fill)) +
    geom_col(width = 0.7) +
    { if (scale == "probability")
        geom_text(data = filter(contr, feature == start_label),
                  aes(label = paste0(round(value), "%")), hjust = -0.2, size = 3.6,
                  colour = "grey25") } +
    geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.4) +
    scale_fill_manual(values = c(up = "#D9665F", down = "#7EA6EA", base = "#A6A6A6"),
                      guide = "none") +
    scale_x_continuous(limits = lims, breaks = seq(lims[1], lims[2], step),
                       labels = labs_fun, position = "top") +
    annotate("text", x = lims[2], y = nlevels(contr$feature) + 0.6, label = "Increase chances",
             hjust = 1, colour = "#C8102E", fontface = "bold", size = 3.6) +
    annotate("text", x = -diff(lims) * 0.01, y = 0.2, label = "Decrease chances",
             hjust = 1, colour = "#1F4FD1", fontface = "bold", size = 3.6) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.background  = element_rect(fill = "#FAFAD2", colour = NA),
          panel.background = element_rect(fill = "#FAFAD2", colour = "grey60"),
          panel.grid.major.y = element_line(colour = "grey85"),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_line(colour = "grey70"),
          axis.text.y = element_text(colour = "grey10", hjust = 1),
          plot.margin = margin(18, 16, 14, 10))
}

# --- Generate all stimuli -----------------------------------------------------------
# One common axis for all charts, so bar lengths are comparable across cases
all_contr <- lapply(seq_len(nrow(trials)), function(i) contributions(trials[i, ]))
vals <- unlist(lapply(all_contr, `[[`, "value"))
if (scale == "logodds") {
  lims <- NULL                                    # per chart, as in Wang & Yin (see below)
} else {                                          # asymmetric, in steps of 20 points
  lims <- c(-20 * max(1, ceiling(-min(vals) / 20)), 20 * ceiling(max(vals) / 20))
}

for (i in seq_len(nrow(trials))) {
  row   <- trials[i, ]
  contr <- all_contr[[i]]

  # Sanity check: starting point + bars must equal the model's prediction
  target <- if (scale == "logodds") qlogis(row$p_reoffend) else 100 * row$p_reoffend
  stopifnot(abs(sum(contr$value) - target) < 1e-6)

  out_dir <- file.path("stimuli", row$trial_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(out_dir, "explanation.png"), plot_explanation(contr, lims),
         width = 7.6, height = 3.8, dpi = 200, bg = "#FAFAD2")
  all_contr[[i]] <- mutate(contr, trial_id = row$trial_id, .before = 1)
}

write.csv(bind_rows(all_contr), "data/contributions.csv", row.names = FALSE)

# --- Example chart for the instructions -------------------------------------------
# A test case that is NOT one of the trials, with at least one red and one blue
# bar, so the example shows both directions without giving away a real trial.
test <- readRDS("data/test_predictions.rds")
pool <- test[!test$id %in% trials$id, ]
set.seed(7)
for (j in sample(nrow(pool))) {
  ex_contr <- contributions(pool[j, ])
  feat <- ex_contr$value[ex_contr$feature != start_label]
  if (sum(feat > 0.1) >= 2 && any(feat < -0.4)) break
}
dir.create("stimuli/example", showWarnings = FALSE)
ggsave("stimuli/example/explanation.png", plot_explanation(ex_contr, lims),
       width = 7.6, height = 3.8, dpi = 200, bg = "#FAFAD2")
write.csv(pool[j, c("id", "race", "sex", "age", "prior_count", "charge_label",
                    "charge_degree", "days_in_custody", "p_reoffend")],
          "data/example_case.csv", row.names = FALSE)
cat("Example chart: test case id", pool$id[j], "\n"); print(ex_contr)
cat("Saved", nrow(trials), "explanation charts (", scale, "scale )\n")
