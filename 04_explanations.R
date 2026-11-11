# =============================================================================
# 04_explanations.R  —  Feature-contribution explanations for each trial
# -----------------------------------------------------------------------------
# Wang & Yin (2022, Sec. 3.2.2): each feature's contribution is its log-odds
# influence relative to the reference defendant (White, female, 31,
# misdemeanor, arrest case no charge, 0 priors, 0 days in custody).
# The grey "base rate" bar is the log-odds of that reference defendant.
# Base rate + all feature bars = the model's log-odds for this defendant.
#
# Input:  data/model.rds, data/trials.rds
# Output: stimuli/T01.png ... T32.png, data/contributions.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

model  <- readRDS("data/model.rds")
trials <- readRDS("data/trials.rds")
b      <- coef(model)
dir.create("stimuli", showWarnings = FALSE)

ref_age <- 31                                     # reference defendant's age
coef_or_zero <- function(name) if (name %in% names(b)) unname(b[name]) else 0

# --- Contributions (log-odds) -----------------------------------------------------
contributions <- function(row) {
  tibble(
    feature = c(
      paste("Race -", row$race),
      paste("Gender -", row$sex),
      paste("Age -", row$age),
      paste("Prior Count -", row$prior_count),
      paste("Charge Name -", tools::toTitleCase(as.character(row$charge_label))),
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
  ) %>%
    arrange(desc(value)) %>%
    bind_rows(tibble(feature = "base rate",
                     value = unname(b["(Intercept)"] + b["age"] * ref_age)))
}

# --- Chart in the style of Wang & Yin (Fig. 1c) -------------------------------------
plot_explanation <- function(contr) {
  lim <- max(2, ceiling(max(abs(contr$value))))
  contr <- contr %>%
    mutate(feature = factor(feature, levels = rev(feature)),
           fill = case_when(feature == "base rate" ~ "base",
                            value >= 0             ~ "up",
                            TRUE                   ~ "down"))
  ggplot(contr, aes(x = value, y = feature, fill = fill)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.4) +
    scale_fill_manual(values = c(up = "#D9665F", down = "#7EA6EA", base = "#A6A6A6"),
                      guide = "none") +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 1),
                       position = "top") +
    annotate("text", x = lim, y = nlevels(contr$feature) + 0.6, label = "Increase chances",
             hjust = 1, colour = "#C8102E", fontface = "bold", size = 3.6) +
    annotate("text", x = -0.05, y = 0.2, label = "Decrease chances",
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

# --- Generate all stimuli -------------------------------------------------------------
all_contr <- list()
for (i in seq_len(nrow(trials))) {
  row   <- trials[i, ]
  contr <- contributions(row)

  # Sanity check: bars must add up to the model's log-odds for this defendant
  stopifnot(abs(sum(contr$value) - qlogis(row$p_reoffend)) < 1e-6)

  ggsave(file.path("stimuli", paste0(row$trial_id, ".png")), plot_explanation(contr),
         width = 7, height = 3.8, dpi = 200, bg = "#FAFAD2")
  all_contr[[i]] <- mutate(contr, trial_id = row$trial_id, .before = 1)
}

write.csv(bind_rows(all_contr), "data/contributions.csv", row.names = FALSE)
cat("Saved", nrow(trials), "explanation charts to stimuli/\n")
