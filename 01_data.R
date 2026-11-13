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

# Barenstein (2019) correction: ProPublica kept recidivists screened after
# 1 April 2014 but dropped the non-recidivists from that period, which inflates
# the base rate. Keep only people with a full two-year follow-up window.
fix_barenstein <- TRUE
if (fix_barenstein) {
  raw <- raw[as.Date(raw$compas_screening_date) <= as.Date("2014-04-01"), ]
}
cat("Rows after correction:", nrow(raw), "\n")  

min_charge_n <- 40                   # minimum cases for a charge category (see below)

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
    charge_label_raw = charge_name,   # original description
    charge_label = charge_name,       # replaced by the category below
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

# --- Charge categories -----------------------------------------------------------
# The raw data contain over 400 different charge descriptions. They are grouped
# into readable categories with the rules below (applied in order; the first
# matching pattern wins). Categories with fewer than `min_charge_n` cases are
# pooled into "Other Offense".
# The SAME category is used by the model, shown in the profile and shown in the
# explanation chart, so every displayed charge has exactly one effect.
# Review data/charge_labels.csv to see how each original charge was grouped.
no_charge <- "No specific charge recorded"        # reference category (base case)
charge_rules <- c(
  "arrest case no charge"                                   = no_charge,
  "dom |domestic|dom strang|injunc"                         = "Domestic Violence",
  "lewd|lasc|voyeur|sex"                                    = "Sexual Offense",
  "dui|under the influence|under influence"                 = "Driving Under the Influence",
  "dwls|lic |license|licence|drivers lic| dl|susp dl|lictag" = "Driving License Offense",
  "leaving the scene|leave scene|leave acc|lve/scen|leaving acc|reckless driv|regist" = "Other Traffic Offense",
  "deliver|sel/del|w/intent|sell|sale|traffick"             = "Drug Dealing",
  "cocaine"                                                 = "Possession of Cocaine",
  "cannabis|marijuana|tetrahydrocannab"                     = "Possession of Cannabis",
  "heroin|alprazolam|oxycodone|hydrocodone|hydromorphone|morphine|methamph|amphetamine|methylenedioxy|mdma|ecstasy|ethylone|pyrrolidino|contr subst|controlled|drug" = "Possession of Other Drugs",
  "battery on law|law enforc"                               = "Battery on a Police Officer",
  "aggrav.*batt|agg batt|batt.*deadly|great bodily"         = "Aggravated Battery",
  "aggrav.*assault|agg assault|assault.*firearm"            = "Aggravated Assault",
  "resist|obstruct|flee|eluding|obey police"                = "Resisting or Fleeing the Police",
  "battery|batt"                                            = "Battery",
  "assault"                                                 = "Assault",
  "robbery"                                                 = "Robbery",
  "burglary|burgl"                                          = "Burglary",
  "grand theft"                                             = "Grand Theft",
  "petit theft|petty theft"                                 = "Petty Theft",
  "theft|stolen|pawn|false ownership"                       = "Other Theft",
  "forg|uttering|counterfeit|fraud|credit card|personal id|identity" = "Forgery or Fraud",
  "tamper|witness"                                          = "Tampering with Evidence or Witnesses",
  "trespass|tresspass"                                      = "Trespassing",
  "disorderly"                                              = "Disorderly Conduct",
  "mischief"                                                = "Criminal Mischief",
  "child abuse|neglect child|cruelty toward child"          = "Child Abuse or Neglect",
  "firearm|weapon|concealed|f/arm|wep"                      = "Weapons Offense"
)
charge_category <- function(x) {
  out <- rep(NA_character_, length(x))
  for (pat in names(charge_rules)) {
    hit <- is.na(out) & grepl(pat, x)
    out[hit] <- charge_rules[[pat]]
  }
  ifelse(is.na(out), "Other Offense", out)
}
clean <- clean %>% mutate(charge_name = charge_category(charge_label_raw))
small <- clean %>% count(charge_name) %>%
  filter(n < min_charge_n, charge_name != no_charge) %>% pull(charge_name)
clean <- clean %>%
  mutate(charge_name  = if_else(charge_name %in% small, "Other Offense", charge_name),
         charge_label = charge_name)            # what participants see = what the model uses
write.csv(clean %>% count(charge_original = charge_label_raw, category = charge_name),
          "data/charge_labels.csv", row.names = FALSE)

# --- Factor levels: first level = reference defendant --------------------------
# Reference defendant ("base case"): White, female, 31, misdemeanor,
# no specific charge recorded, 0 priors, 0 days in custody.
clean <- clean %>%
  mutate(
    race          = relevel(factor(race), ref = "White"),
    sex           = relevel(factor(sex), ref = "female"),
    charge_name   = relevel(factor(charge_name), ref = no_charge),
    charge_degree = relevel(factor(charge_degree), ref = "misdemeanor")
  )

# --- Train / test split ----------------------------------------------------------
test_idx <- sample(nrow(clean), 1000)
clean$set <- "train"
clean$set[test_idx] <- "test"

saveRDS(clean, "data/compas_clean.rds")

cat("Charge categories:", nlevels(clean$charge_name), "(incl. reference)\n"); print(sort(table(clean$charge_name), decreasing = TRUE))
cat("Training cases:", sum(clean$set == "train"),
    "| Test cases:", sum(clean$set == "test"), "\n")
cat(sprintf("Share re-offended in test set: %.1f%% (Wang & Yin: 47.7%%, Barenstein: 36.2%%)\n",
            100 * mean(clean$reoffend[clean$set == "test"])))
