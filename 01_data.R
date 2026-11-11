# =============================================================================
# 01_data.R  —  Load and prepare the COMPAS data
# -----------------------------------------------------------------------------
# Reconstructs the data set used by Wang & Yin (2021, 2022, Exp. 1):
#   - ProPublica COMPAS "two-years" file (7,214 defendants, Broward County)
#   - Seven profile features: race, sex, age, prior count, charge name,
#     charge degree, days in custody
#   - Outcome: re-offended within two years (two_year_recid)
#   - Random hold-out test set of 1,000 cases; the rest is training data
#
# Output: data/compas_clean.rds  (one data frame with a `set` column)
# Run order: 01_data.R -> 02_model.R -> 03_select_trials.R -> 04_explanations.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

set.seed(72)                       # change to draw a different train/test split
dir.create("data", showWarnings = FALSE)

url <- "https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv"
raw_file <- "data/compas-scores-two-years.csv"
if (!file.exists(raw_file)) download.file(url, raw_file, mode = "wb")

# The file has duplicated column names; base read.csv keeps the first copy as is
raw <- read.csv(raw_file, stringsAsFactors = FALSE)
cat("Raw rows:", nrow(raw), "\n")    # should be 7,214 (same N as Wang & Yin)

# --- Charge names --------------------------------------------------------------
# The raw data contain hundreds of charge descriptions. Rare ones are pooled into
# "other charge" so the model can estimate a stable coefficient for each level.
# "arrest case no charge" is kept as its own level: it is the reference category
# of the reference defendant shown to participants.
min_charge_n <- 40                   # minimum cases for a charge to keep its own level

clean <- raw %>%
  transmute(
    id         = id,
    race       = case_when(race == "African-American" ~ "Black",
                           race == "Caucasian"        ~ "White",
                           race == "Hispanic"         ~ "Hispanic",
                           TRUE                       ~ "Other"),
    sex        = tolower(sex),                                   # "male" / "female"
    age        = age,
    prior_count = priors_count,
    charge_name = tolower(gsub("\\s+", " ", trimws(coalesce(na_if(c_charge_desc, ""), "arrest case no charge")))),
    charge_label = charge_name,   # original description, shown to participants
    charge_degree = if_else(c_charge_degree == "F", "felony", "misdemeanor"),
    days_in_custody = as.numeric(difftime(as.Date(na_if(c_jail_out, "")),
                                          as.Date(na_if(c_jail_in, "")),
                                          units = "days")),
    reoffend   = two_year_recid
  ) %>%
  mutate(
    # No jail record (or a data error) -> 0 days in custody
    days_in_custody = pmax(coalesce(days_in_custody, 0), 0),
    charge_name = if_else(charge_name == "", "arrest case no charge", charge_name)
  )

common <- clean %>% count(charge_name) %>% filter(n >= min_charge_n) %>% pull(charge_name)
clean <- clean %>%
  mutate(charge_name = if_else(charge_name %in% c(common, "arrest case no charge"),
                               charge_name, "other charge"))

# Note: the model uses the pooled `charge_name`; participants see `charge_label`
# (the original charge), so "other charge" never appears on screen.

# --- Factor levels: first level = reference defendant --------------------------
# Reference defendant (Wang & Yin): White, female, 31, misdemeanor,
# arrest case no charge, 0 priors, 0 days in custody.
clean <- clean %>%
  mutate(
    race          = relevel(factor(race), ref = "White"),
    sex           = relevel(factor(sex), ref = "female"),
    charge_name   = relevel(factor(charge_name), ref = "arrest case no charge"),
    charge_degree = relevel(factor(charge_degree), ref = "misdemeanor")
  )

# --- Train / test split ----------------------------------------------------------
test_idx <- sample(nrow(clean), 1000)
clean$set <- "train"
clean$set[test_idx] <- "test"

saveRDS(clean, "data/compas_clean.rds")

cat("Charge-name levels:", nlevels(clean$charge_name), "\n")
cat("Training cases:", sum(clean$set == "train"),
    "| Test cases:", sum(clean$set == "test"), "\n")
cat(sprintf("Share re-offended in test set: %.1f%% (Wang & Yin: 47.7%%)\n",
            100 * mean(clean$reoffend[clean$set == "test"])))
