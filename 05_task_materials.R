# =============================================================================
# 05_task_materials.R  —  Task screens for each trial as PNG images
# -----------------------------------------------------------------------------
# Builds, for each of the 32 trials, the materials in the style of
# Wang & Yin (2021, Fig. 1a):
#   profile.png           "Defendant Profile:" table with the seven features
#   model_prediction.png  Yellow box with the model's prediction
#   feedback_initial-<c>_final-<c>.png
#                         Feedback screen: actual outcome, plus whether the
#                         participant's initial prediction, the model's
#                         prediction and the participant's final prediction
#                         were correct. The model part is fixed per trial; the
#                         participant parts depend on their answers, so all
#                         4 combinations are rendered (<c> = correct/incorrect).
# The explanation chart (explanation.png) comes from 04_explanations.R.
#
# data/feedback_lookup.csv maps every combination of answers to its image,
# so the survey can show the right feedback file.
#
# Input:  data/trials.rds
# Output: stimuli/T01/ ... stimuli/T32/, data/feedback_lookup.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(grid)
})

trials <- readRDS("data/trials.rds")

# --- Style ----------------------------------------------------------------------
font      <- "sans"
fs        <- 13                 # body font size (pt)
dpi       <- 200
col_head  <- "#595959"          # dark grey header cells
col_line  <- "#333333"
col_yellow <- "#FAFAD2"
col_red   <- "#C8102E"
col_ok    <- "#1A7F37"          # check mark
col_wrong <- "#A84C00"          # cross
pad       <- 0.18               # inner padding (inches)

save_png <- function(file, width, height, draw) {
  png(file, width = width, height = height, units = "in", res = dpi,
      type = "cairo", bg = "white")
  grid.newpage()
  draw()
  invisible(dev.off())
}

# Draw a line of text made of segments with their own style, left-aligned.
# segs: list of list(text = ..., face = "plain"/"bold", col = ...)
draw_rich <- function(segs, x, y, size = fs) {
  x <- unit(x, "in")
  for (s in segs) {
    gp <- gpar(fontfamily = font, fontsize = size,
               fontface = if (is.null(s$face)) "plain" else s$face,
               col = if (is.null(s$col)) "black" else s$col)
    g  <- textGrob(s$text, x = x, y = unit(y, "in"), just = c("left", "centre"), gp = gp)
    grid.draw(g)
    x <- x + grobWidth(g)
  }
}

draw_check <- function(x, y, s = 0.15) {
  grid.lines(unit(x + s * c(0, 0.35, 1), "in"), unit(y + s * c(0, -0.35, 0.55), "in"),
             gp = gpar(col = col_ok, lwd = 3, lineend = "round", linejoin = "round"))
}
draw_cross <- function(x, y, s = 0.13) {
  g <- gpar(col = col_wrong, lwd = 3, lineend = "round")
  grid.lines(unit(x + c(0, s), "in"), unit(y + c(-s, s) / 2, "in"), gp = g)
  grid.lines(unit(x + c(0, s), "in"), unit(y + c(s, -s) / 2, "in"), gp = g)
}

# --- 1. Defendant profile --------------------------------------------------------------
# 8 columns: header/value pairs. Row 2: charge name spans three columns.
col_w <- c(0.95, 0.95, 1.0, 0.75, 0.95, 1.05, 1.1, 0.55)
row_h <- c(0.6, 0.75)

draw_profile <- function(r) {
  W <- sum(col_w) + 2 * pad
  H <- 0.55 + sum(row_h) + pad
  x0 <- c(0, cumsum(col_w)) + pad                          # column left edges
  y_top <- H - 0.55
  grid.text("Defendant Profile:", x = unit(pad, "in"), y = unit(H - 0.28, "in"),
            just = "left", gp = gpar(fontfamily = font, fontsize = fs + 1, fontface = "bold"))

  cell <- function(c1, c2, row, label, header) {
    y1 <- y_top - sum(row_h[seq_len(row)])
    w  <- x0[c2 + 1] - x0[c1]
    grid.rect(x = unit(x0[c1], "in"), y = unit(y1, "in"), width = unit(w, "in"),
              height = unit(row_h[row], "in"), just = c("left", "bottom"),
              gp = gpar(fill = if (header) col_head else "white", col = col_line, lwd = 1))
    grid.text(label, x = unit(x0[c1] + 0.08, "in"), y = unit(y1 + row_h[row] / 2, "in"),
              just = "left",
              gp = gpar(fontfamily = font, fontsize = fs - 1, lineheight = 0.95,
                        fontface = if (header) "bold" else "plain",
                        col = if (header) "white" else "black"))
  }
  # Row 1
  cell(1, 1, 1, "1. Race:", TRUE);                 cell(2, 2, 1, as.character(r$race), FALSE)
  cell(3, 3, 1, "2.\nGender:", TRUE);              cell(4, 4, 1, as.character(r$sex), FALSE)
  cell(5, 5, 1, "3. Age:", TRUE);                  cell(6, 6, 1, r$age, FALSE)
  cell(7, 7, 1, "4. Prior\nCount:", TRUE);         cell(8, 8, 1, r$prior_count, FALSE)
  # Row 2
  cell(1, 1, 2, "5.\nCharge\nName:", TRUE);        cell(2, 4, 2, r$charge_label, FALSE)
  cell(5, 5, 2, "6. Charge\nDegree:", TRUE);       cell(6, 6, 2, as.character(r$charge_degree), FALSE)
  cell(7, 7, 2, "7. Days in\nCustody:", TRUE);     cell(8, 8, 2, r$days_in_custody, FALSE)
  c(W, H)
}

# --- 2. Model prediction -----------------------------------------------------------------
draw_model <- function(r, W, H) {
  grid.rect(gp = gpar(fill = col_yellow, col = NA))
  grid.text("Machine Learning Prediction:", x = unit(pad, "in"), y = unit(H - 0.3, "in"),
            just = "left", gp = gpar(fontfamily = font, fontsize = fs + 1, fontface = "bold"))
  draw_rich(list(
    list(text = "Our machine learning model predicts that this person "),
    list(text = if (r$prediction == 1) "will" else "will not", face = "bold", col = col_red),
    list(text = " reoffend in 2 years.")
  ), x = pad, y = H - 0.72)
}

# --- 3. Feedback -------------------------------------------------------------------------
draw_feedback <- function(r, initial_ok, final_ok, W, H) {
  grid.rect(x = unit(0.06, "in"), y = unit(0.06, "in"), width = unit(W - 0.12, "in"),
            height = unit(H - 0.12, "in"), just = c("left", "bottom"),
            gp = gpar(fill = "white", col = "black", lwd = 2.5))
  grid.text("Actual Outcome:", x = unit(pad + 0.1, "in"), y = unit(H - 0.38, "in"),
            just = "left", gp = gpar(fontfamily = font, fontsize = fs + 1, fontface = "bold"))
  draw_rich(list(
    list(text = "This defendant "),
    list(text = if (r$reoffend == 1) "did" else "did not", face = "bold"),
    list(text = " reoffend within 2 years.")
  ), x = pad + 0.1, y = H - 0.82, size = fs + 1)

  line <- function(y, who, ok) {
    if (ok) draw_check(pad + 0.12, y) else draw_cross(pad + 0.12, y)
    draw_rich(list(list(text = paste0(who, " was ")),
                   list(text = if (ok) "correct" else "incorrect", face = "bold"),
                   list(text = ".")),
              x = pad + 0.45, y = y)
  }
  line(H - 1.27, "Your initial prediction", initial_ok)
  line(H - 1.65, "The machine learning model\u2019s prediction", r$correct)
  line(H - 2.03, "Your final prediction", final_ok)
}

# --- Render all trials ------------------------------------------------------------------
lookup <- list()
for (i in seq_len(nrow(trials))) {
  r   <- trials[i, ]
  dir <- file.path("stimuli", r$trial_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  W <- sum(col_w) + 2 * pad
  save_png(file.path(dir, "profile.png"), W, 0.55 + sum(row_h) + pad,
           function() draw_profile(r))
  save_png(file.path(dir, "model_prediction.png"), W, 1.0,
           function() draw_model(r, W, 1.0))

  for (initial_ok in c(TRUE, FALSE)) for (final_ok in c(TRUE, FALSE)) {
    tag  <- function(ok) if (ok) "correct" else "incorrect"
    file <- sprintf("feedback_initial-%s_final-%s.png", tag(initial_ok), tag(final_ok))
    save_png(file.path(dir, file), W, 2.35,
             function() draw_feedback(r, initial_ok, final_ok, W, 2.35))

    # A participant's answer is correct when it matches the actual outcome
    answer <- function(ok) if (ok == (r$reoffend == 1)) "will reoffend" else "will not reoffend"
    lookup[[length(lookup) + 1]] <- tibble(
      trial_id         = r$trial_id,
      actual_outcome   = if (r$reoffend == 1) "reoffended" else "did not reoffend",
      model_prediction = if (r$prediction == 1) "will reoffend" else "will not reoffend",
      model_correct    = r$correct,
      initial_answer   = answer(initial_ok),
      final_answer     = answer(final_ok),
      feedback_file    = file.path(dir, file)
    )
  }
}

write.csv(bind_rows(lookup), "data/feedback_lookup.csv", row.names = FALSE)
cat("Saved profile, model prediction and 4 feedback images for", nrow(trials),
    "trials in stimuli/<trial>/\n")
cat("Lookup table for the survey: data/feedback_lookup.csv\n")
