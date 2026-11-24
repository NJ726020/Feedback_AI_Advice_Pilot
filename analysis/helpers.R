# =============================================================================
# analysis/helpers.R  —  Shared plotting theme and small helpers for the reports
# -----------------------------------------------------------------------------
# Sourced by 02_pilot_analysis.Rmd and 03_exploratory_analysis.Rmd.
# Colours: Solarized (light) by Ethan Schoonover, https://ethanschoonover.com/solarized/
# The two condition colours (blue, orange) were checked for colour-vision
# deficiency separation and contrast against the base3 background.
# =============================================================================

sol <- c(
  base03 = "#002b36", base02 = "#073642", base01 = "#586e75", base00 = "#657b83",
  base0  = "#839496", base1  = "#93a1a1", base2  = "#eee8d5", base3  = "#fdf6e3",
  yellow = "#b58900", orange = "#cb4b16", red    = "#dc322f", magenta = "#d33682",
  violet = "#6c71c4", blue   = "#268bd2", cyan   = "#2aa198", green  = "#859900"
)

# Condition colours and labels (fixed order: no feedback first)
cond_levels <- c("no_feedback", "feedback")
cond_labels <- c(no_feedback = "No feedback", feedback = "Feedback")
cond_cols   <- c("No feedback" = unname(sol["orange"]), "Feedback" = unname(sol["blue"]))

# Minimal ggplot2 theme on a Solarized light background
theme_pilot <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = sol[["base3"]], colour = NA),
      panel.background   = ggplot2::element_rect(fill = sol[["base3"]], colour = NA),
      panel.grid.major   = ggplot2::element_line(colour = sol[["base2"]], linewidth = 0.4),
      panel.grid.minor   = ggplot2::element_blank(),
      text               = ggplot2::element_text(colour = sol[["base01"]]),
      axis.text          = ggplot2::element_text(colour = sol[["base00"]]),
      axis.title         = ggplot2::element_text(colour = sol[["base01"]]),
      plot.title         = ggplot2::element_text(colour = sol[["base02"]], face = "bold",
                                                 size = base_size + 2),
      plot.subtitle      = ggplot2::element_text(colour = sol[["base00"]]),
      plot.caption       = ggplot2::element_text(colour = sol[["base1"]], hjust = 0),
      strip.text         = ggplot2::element_text(colour = sol[["base02"]], face = "bold"),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin        = ggplot2::margin(10, 14, 8, 8)
    )
}
scale_colour_cond <- function(...) ggplot2::scale_colour_manual(values = cond_cols, ...)
scale_fill_cond   <- function(...) ggplot2::scale_fill_manual(values = cond_cols, ...)

# Mean with a 95% t-interval (for participant-level summaries)
mean_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- if (n > 0) mean(x) else NA_real_
  se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
  q  <- if (n > 1) stats::qt(0.975, n - 1) else NA_real_
  data.frame(n = n, mean = m, lo = m - q * se, hi = m + q * se)
}

# Formatting
fmt_p   <- function(p) ifelse(is.na(p), "\u2013", ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p))))
fmt_num <- function(x, d = 2) ifelse(is.na(x), "\u2013", formatC(x, format = "f", digits = d))
fmt_pct <- function(x, d = 1) ifelse(is.na(x), "\u2013", paste0(formatC(100 * x, format = "f", digits = d), "%"))

# Fit a glmer and return NULL (with a printed note) instead of stopping the report
# when the model cannot be estimated, e.g. with very small samples.
safe_glmer <- function(formula, data) {
  tryCatch(
    lme4::glmer(formula, data = data, family = stats::binomial,
                control = lme4::glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5))),
    error = function(e) { message("Model could not be fitted: ", conditionMessage(e)); NULL }
  )
}

# Fixed effects of a binomial glmer as odds ratios with Wald 95% CIs
or_table <- function(fit) {
  if (is.null(fit)) return(NULL)
  s <- summary(fit)$coefficients
  data.frame(
    term = rownames(s),
    OR   = exp(s[, "Estimate"]),
    lo   = exp(s[, "Estimate"] - 1.96 * s[, "Std. Error"]),
    hi   = exp(s[, "Estimate"] + 1.96 * s[, "Std. Error"]),
    z    = s[, "z value"],
    p    = s[, "Pr(>|z|)"],
    row.names = NULL
  )
}

# Pretty version of or_table for knitr::kable
or_kable <- function(tab, labels = NULL) {
  if (is.null(tab)) return(data.frame(Note = "Model could not be fitted with the current data."))
  if (!is.null(labels)) tab$term <- ifelse(tab$term %in% names(labels), labels[tab$term], tab$term)
  data.frame(Term = tab$term,
             `Odds ratio` = fmt_num(tab$OR),
             `95% CI` = paste0("[", fmt_num(tab$lo), ", ", fmt_num(tab$hi), "]"),
             z = fmt_num(tab$z),
             p = fmt_p(tab$p),
             check.names = FALSE)
}

# Model status line: N, singular fit, convergence warnings
model_note <- function(fit) {
  if (is.null(fit)) return("Model not fitted.")
  conv <- fit@optinfo$conv$lme4$messages
  paste0(nobs(fit), " decisions from ", lme4::ngrps(fit)[["participant"]], " participants",
         if (lme4::isSingular(fit)) "; singular fit (a random-effect variance is estimated as ~0)" else "",
         if (length(conv)) paste0("; convergence note: ", paste(conv, collapse = " ")) else "", ".")
}
