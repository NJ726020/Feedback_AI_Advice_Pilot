# Survey analysis: AI Feedback Pilot UM

This folder holds the analysis of the survey data for the pilot study on outcome feedback in AI-assisted recidivism prediction
(preregistration: [AsPredicted #324733](https://aspredicted.org/j9rg5y.pdf)).

**Design.** Everyone sees the AI's prediction and its feature-contribution explanation.
Participants are randomly assigned to *outcome feedback after each case* or *no feedback*.
Each participant makes an initial and a final prediction for 8 of 40 cases (random selection and order).

## Files

| File | What it does |
|---|---|
| `01_import_clean.R` | Reads the Qualtrics export(s), removes previews and tests, applies the preregistered exclusions, computes all measures, writes the analysis files |
| `02_pilot_analysis.Rmd` | Preregistered analysis: sample, descriptives, the confirmatory mixed model, robustness checks |
| `03_exploratory_analysis.Rmd` | Exploratory analysis: reliance, switching, learning over cases, questionnaire, time, courses, open comments |
| `run_all.R` | Runs all three steps; reports go to `analysis/reports/` |
| `helpers.R` | Plot theme (minimal, Solarized light), colours, small helper functions |
| `solarized.css` | Styling of the HTML reports |

The case information (true outcome, AI prediction, AI correctness) comes from the project's `data/trials.csv`, written by `03_select_trials.R`. The import script checks via MD5 that it is still the version of commit `acd86672`, the one the survey's images come from. If you rerun the stimulus pipeline and the file changes, the import stops instead of silently using the wrong cases.

## How to run

1. Open the project (`Feedback_AI_Advice.Rproj`). All paths are resolved from the project root with the `here` package, so knitting a report or sourcing a script works from any working directory.
2. In Qualtrics, go to **Data & Analysis → Export & Import → Export data → CSV**, and tick:
   - **Use choice text**
   - **Export viewing order data for randomized surveys** (needed for case position)

   Put the CSV file(s) into `analysis/data/raw/`. Several files, e.g. one per course copy of the survey, are combined automatically.
3. Run `source("analysis/run_all.R")`.

Required packages: `dplyr`, `tidyr`, `readr`, `stringr`, `purrr`, `ggplot2`, `lme4`, `scales`, `knitr`, `rmarkdown`, `here`.

For exact reproducibility, run `renv::init()` once in the project after the first successful run and commit the resulting `renv.lock`. Anyone can then restore the same package versions with `renv::restore()`. Each report also ends with its `sessionInfo()`.

## Settings

All settings are at the top of `01_import_clean.R` and were fixed before data collection:

- `collection_start` (16 Nov 2026, 00:00 Amsterdam time). Responses that started earlier are test runs and are removed.
- `test_pid_pattern` (`"test"`). PIDs containing it are removed.
- `min_median_case_seconds` (6). The preregistered speed criterion.

## Sample flow

1. **Cleaning (non-participants):** survey previews, responses before `collection_start`, test PIDs.
2. **Preregistered exclusions,** in the preregistered order:
   1. no consent;
   2. did not finish all 8 cases;
   3. duplicate PID (the earliest remaining response is kept);
   4. median time per case below 6 s (first-prediction page + final-prediction page);
   5. failed the comprehension check twice.

`analysis/data/processed/exclusion_log.csv` records the counts. `analysis/data/processed/screening.csv` gives the reason for every response.

## Measures

- **Final / initial accuracy:** share of correct predictions (the true outcome is a re-arrest within two years).
- **Reliance types** (Schoeffer et al., 2023):
  - correct adherence: following a correct AI;
  - detrimental adherence: following a wrong AI;
  - corrective override: overriding a wrong AI;
  - detrimental override: overriding a correct AI.

  Final accuracy = correct adherence + corrective override.
- **Reliance quality Q** (Schoeffer et al., 2023, Eq. 2): where final accuracy lies within the range attainable at the participant's adherence level, given the AI's accuracy on that participant's 8 cases (0 = worst, 1 = best). Undefined if the AI was right on fewer than half of the cases or the attainable range is zero.
- **RAIR / RSR** (Schemmer et al., 2023):
  - RAIR (relative AI reliance): share of switches to a correct AI after a wrong initial prediction.
  - RSR (relative self-reliance): share of staying with a correct initial prediction when the AI was wrong.

## Data protection

The consent form says the data are only accessed by the researchers in the project. The `.gitignore` in this folder therefore keeps all participant data out of the repository: `analysis/data/raw/`, `analysis/data/processed/`, `analysis/data/private/`, and the rendered `analysis/reports/`, which contain the open comments.

- `analysis/data/private/id_key.csv` links the anonymous participant codes (P001, …) to the chain PIDs.
- `analysis/data/private/open_comments.csv` holds the free-text answers.

## References

- Schemmer, M., Kühl, N., Benz, C., Bartos, A., & Satzger, G. (2023). Appropriate reliance on AI advice: Conceptualization and the effect of explanations. *Proceedings of IUI 2023*.
- Schoeffer, J., Jakubik, J., Voessing, M., Kuehl, N., & Satzger, G. (2023). On the interdependence of reliance behavior and accuracy in AI-assisted decision-making. *HHAI 2023*. arXiv:2304.08804.
