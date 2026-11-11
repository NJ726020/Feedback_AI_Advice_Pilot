# =============================================================================
# 06_survey_flow.R  —  Flow diagram of the pilot survey (Qualtrics)
# -----------------------------------------------------------------------------
# Draws the survey flow with base grid graphics.
# Output: figures/survey_flow.png
# =============================================================================

library(grid)
dir.create("figures", showWarnings = FALSE)

box <- function(x, y, w, h, label, fill = "grey92", fontface = "plain", lty = 1) {
  grid.rect(x, y, w, h, default.units = "native",
            gp = gpar(fill = fill, col = "black", lwd = 1, lty = lty))
  if (!is.null(label))
    grid.text(label, x, y, default.units = "native",
              gp = gpar(fontsize = 10, fontface = fontface, lineheight = 1.1))
}
arrow_path <- function(x, y, head = TRUE) {
  grid.lines(x, y, default.units = "native",
             arrow = if (head) arrow(length = unit(0.08, "inches"), type = "closed") else NULL,
             gp = gpar(col = "black", fill = "black", lwd = 1))
}

draw_flow <- function() {
  pushViewport(viewport(xscale = c(0, 7.5), yscale = c(0.65, 7.05)))

  box(3.75, 6.70, 3.0, 0.45, "Random assignment (50/50)")
  box(2.20, 5.90, 2.0, 0.45, "Feedback condition")
  box(5.30, 5.90, 2.0, 0.45, "No-feedback condition")
  box(3.75, 5.05, 3.9, 0.60,
      "Instructions\n(feedback group: extra paragraph on feedback)")

  # Repeated case block
  box(3.75, 3.825, 7.0, 1.25, NULL, fill = "white", lty = 2)
  grid.text("Repeated for 3 cases (T19, T04, T08), random order",
            0.40, 4.27, just = "left", default.units = "native",
            gp = gpar(fontsize = 10, fontface = "italic"))
  box(1.40, 3.65, 2.05, 0.70, "Page 1\nDefendant profile\nInitial prediction")
  box(3.75, 3.65, 2.05, 0.70, "Page 2\nAI prediction + explanation\nFinal prediction")
  box(6.10, 3.65, 2.05, 0.70, "Page 3 (feedback only)\nFeedback matching\nboth answers",
      fill = "grey75")

  box(3.75, 2.25, 4.6, 1.15, paste(
    "Pilot evaluation",
    "Everyone: difficulty, explanations, trust",
    "Feedback group: comprehension, 6 items",
    "No-feedback group: example screen, 3 items",
    "Everyone: preferences, interest, comments", sep = "\n"))
  box(3.75, 1.00, 2.0, 0.45, "Debrief")

  arrow_path(c(3.75, 3.75, 2.20, 2.20), c(6.475, 6.30, 6.30, 6.125))
  arrow_path(c(3.75, 5.30, 5.30), c(6.30, 6.30, 6.125))
  arrow_path(c(2.20, 2.20, 3.75, 3.75), c(5.675, 5.50, 5.50, 5.35))
  arrow_path(c(5.30, 5.30, 3.75), c(5.675, 5.50, 5.50), head = FALSE)
  arrow_path(c(3.75, 3.75), c(4.75, 4.45))
  arrow_path(c(2.425, 2.725), c(3.65, 3.65))
  arrow_path(c(4.775, 5.075), c(3.65, 3.65))
  arrow_path(c(3.75, 3.75), c(3.20, 2.825))
  arrow_path(c(3.75, 3.75), c(1.675, 1.225))
  popViewport()
}

png("figures/survey_flow.png", width = 7.5, height = 6.4, units = "in", res = 200,
    type = "cairo", bg = "white")
grid.newpage(); draw_flow()
invisible(dev.off())
