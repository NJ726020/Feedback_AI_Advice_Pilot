# =============================================================================
# 01_import_clean.R  —  Import the Qualtrics export(s), clean, apply exclusions
# -----------------------------------------------------------------------------
# Study:  AI Feedback Pilot UM (AsPredicted #324733, https://aspredicted.org/j9rg5y.pdf)
# Design: everyone sees the AI prediction + feature-contribution explanation;
#         between subjects: outcome feedback after each case vs. no feedback.
#         Each participant sees 8 of 40 cases in random order.
#
# Input:  analysis/data/raw/*.csv  Qualtrics export(s), "Download data table" as CSV
#                            with "Use choice text" and "Export viewing order
#                            data for randomized surveys" ticked. Several files
#                            (e.g. one per course/survey copy) are combined;
#                            keep only the newest export of each survey.
#         data/trials.csv    Case information (truth, AI prediction) for the
#                            40 cases, written by 03_select_trials.R; must
#                            match commit acd86672 (the commit the survey's
#                            images use) - checked case by case below.
# Output (all under analysis/):
#         data/processed/participants.csv   one row per consenting, complete,
#                                           unique participant (+ exclusion flags)
#         data/processed/trials_long.csv    one row per participant x case
#         data/processed/exclusion_log.csv  sample flow
#         data/processed/screening.csv      one row per response, all flags
#         data/private/id_key.csv           participant <-> PID / ResponseId
#         data/private/open_comments.csv    free-text answers
# Paths are relative to the project root (Feedback_AI_Advice.Rproj) via here.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(here)
})

here::i_am("analysis/01_import_clean.R")
ap <- function(...) here("analysis", ...)       # paths inside analysis/

# --- Settings (fixed before data collection; see README) ----------------------
raw_dir          <- ap("data", "raw")
trials_file      <- here("data", "trials.csv")   # stimulus pipeline output
# True outcome and AI prediction of every case as shown in the survey
# (data/trials.csv at commit acd86672), written as trial:<reoffended><AI says reoffend>.
# Used to check that data/trials.csv still describes the cases the survey showed.
expected_cases <- "T01:00 T02:10 T03:10 T04:10 T05:00 T06:00 T07:00 T08:00 T09:00 T10:10
  T11:00 T12:00 T13:10 T14:00 T15:11 T16:11 T17:11 T18:01 T19:00 T20:00 T21:00 T22:00
  T23:11 T24:00 T25:00 T26:00 T27:10 T28:10 T29:00 T30:00 T31:00 T32:10 T33:01 T34:00
  T35:01 T36:11 T37:00 T38:11 T39:00 T40:10"
local_tz         <- "Europe/Amsterdam"
# Responses that started before this moment are test runs (preregistration:
# no data collected before 15 Nov 2026; sessions start 16 Nov 2026).
collection_start <- as.POSIXct("2026-11-16 00:00:00", tz = local_tz)
test_pid_pattern <- "test"          # PIDs containing this (any case) are tests
# Values that are not real PIDs: empty, or Qualtrics' placeholder text when an
# Embedded Data field was set to a value instead of being read from the link.
# Such responses are kept, but cannot be checked for duplicate PIDs.
pid_placeholders <- c("", "Custom Value")
n_cases          <- 8               # cases per participant
min_median_case_seconds <- 6        # preregistered speed criterion

# Correct answers of the comprehension check (choice text)
comp_correct <- c(comp_baserate = "About 36 out of 100",
                  comp_accuracy = "About 7 out of 10",
                  comp_feature  = "It makes reoffending less likely")
likert7 <- c("Strongly disagree", "Disagree", "Somewhat disagree",
             "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")
items <- c("trust", "decision_help", "learn_AI", "learn_human",
           "understand_AI", "implement", "bias")

dir.create(ap("data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(ap("data", "private"),   showWarnings = FALSE, recursive = TRUE)

# --- 1. Read Qualtrics export(s) ------------------------------------------------
# Qualtrics CSVs have three header rows: variable names, question texts and
# import IDs (which also hold the time zone of the date columns).
read_qualtrics <- function(path) {
  all <- read_csv(path, col_types = cols(.default = col_character()),
                  na = character(), name_repair = "minimal", progress = FALSE)
  if (anyDuplicated(names(all))) stop(path, ": duplicated column names.")
  if (nrow(all) < 2 || !str_detect(all$ResponseId[2], "ImportId"))
    stop(path, ": not a Qualtrics CSV export with three header rows.")
  tz <- str_match(all$StartDate[2], '"timeZone":"([^"]+)"')[, 2]
  if (is.na(tz)) stop(path, ": time zone of the date columns not found.")
  # Display order of the cases: the block randomizer's "_DO" column. Its name
  # (e.g. FL_39_DO) can differ between survey copies, so it is renamed here.
  dat <- all[-(1:2), ]
  do_col <- names(dat)[str_detect(names(dat), "_DO$")] %>%
    keep(~ any(str_detect(dat[[.x]], "Trial")))
  if (length(do_col) != 1) stop(path, ": could not identify the case display-order column. ",
                                "Export with 'viewing order data for randomized surveys'.")
  dat %>%
    rename(case_order = all_of(do_col)) %>%
    select(-matches("^FL_\\d+_DO$")) %>%
    mutate(source_file = basename(path),
           start_time  = with_tz(as.POSIXct(StartDate, tz = tz), local_tz),
           end_time    = with_tz(as.POSIXct(EndDate,   tz = tz), local_tz))
}
with_tz <- function(x, tz) { attr(x, "tzone") <- tz; x }

raw_files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) stop("No CSV files in ", raw_dir, ".")
resp <- map(raw_files, read_qualtrics) %>% bind_rows()
cat("Read", nrow(resp), "responses from", length(raw_files), "file(s):",
    paste(basename(raw_files), collapse = ", "), "\n")

# The same response in two files means two exports of the same survey
# (e.g. an older and a newer download): keep only the newest export in data/raw.
dup_resp <- resp %>% distinct(ResponseId, source_file) %>% count(ResponseId) %>% filter(n > 1)
if (nrow(dup_resp) > 0) {
  files <- resp %>% filter(ResponseId %in% dup_resp$ResponseId) %>% distinct(source_file) %>% pull()
  stop(nrow(dup_resp), " response(s) appear in more than one file (",
       paste(files, collapse = ", "), "). These look like two exports of the same survey. ",
       "Keep only the newest export of each survey in ", raw_dir, ".")
}
stopifnot(!anyDuplicated(resp$ResponseId))

# Course label: "chain" in some survey copies, "course" in others
for (v in c("chain", "course", "study_order")) if (!v %in% names(resp)) resp[[v]] <- NA_character_
resp <- resp %>%
  mutate(pid_valid   = !(PID %in% pid_placeholders) & !str_detect(PID, fixed("${")),
         PID         = if_else(pid_valid, PID, ""),
         study_order = if_else(study_order %in% pid_placeholders, NA_character_, study_order),
         course_lab  = coalesce(na_if(chain, ""), na_if(course, "")))

# Check that the export uses choice text (not numeric codes)
stopifnot(all(resp$consent %in% c("", "Yes, I agree to take part", "No, I do not want to take part")))
stopifnot(all(resp$Condition %in% c("", "feedback", "no_feedback")))

# Remove directly identifying columns right away
resp <- resp %>%
  select(-any_of(c("IPAddress", "LocationLatitude", "LocationLongitude",
                   "RecipientLastName", "RecipientFirstName", "RecipientEmail",
                   "ExternalReference")))

# --- 2. Case information ------------------------------------------------------
cases <- read_csv(trials_file, show_col_types = FALSE) %>%
  transmute(trial_id, truth = as.integer(reoffend), ai_pred = as.integer(prediction),
            ai_correct = as.logical(correct), ai_confidence = confidence,
            conf_group, race, sex, age, prior_count, charge_label, charge_degree,
            days_in_custody)
expected <- str_split(str_squish(expected_cases), " ")[[1]]
observed <- sprintf("%s:%d%d", cases$trial_id, cases$truth, cases$ai_pred)
if (!setequal(observed, expected)) {
  bad <- union(setdiff(observed, expected), setdiff(expected, observed))
  stop("data/trials.csv does not describe the cases shown in the survey (commit acd86672). ",
       "Differences: ", paste(sort(unique(substr(bad, 1, 3))), collapse = ", "), ". ",
       "Restore the survey version with:  git checkout acd8667 -- data/trials.csv")
}
stopifnot(nrow(cases) == 40, all(cases$ai_correct == (cases$truth == cases$ai_pred)))

# --- 3. Trial-level data ----------------------------------------------------------
answer <- function(x) case_when(str_starts(x, "Yes") ~ 1L, str_starts(x, "No") ~ 0L,
                                x == "" ~ NA_integer_)
num <- function(x) suppressWarnings(as.numeric(na_if(x, "")))

resp <- resp %>% mutate(response_no = row_number())
trial_cols <- grep("^T\\d{2}_(initial|final|t_initial_Page Submit|t_final_Page Submit|t_feedback_Page Submit)$",
                   names(resp), value = TRUE)
long <- resp %>%
  select(response_no, all_of(trial_cols)) %>%
  pivot_longer(-response_no, names_to = c("trial_id", "var"),
               names_pattern = "^(T\\d{2})_(.*)$", values_to = "value") %>%
  mutate(var = recode(var, `t_initial_Page Submit` = "time_initial",
                      `t_final_Page Submit` = "time_final",
                      `t_feedback_Page Submit` = "time_feedback")) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  filter(initial != "" | final != "")
if (!all(str_detect(c(long$initial, long$final), "^(Yes|No)|^$")))
  stop("Unexpected prediction answer text (export with 'Use choice text').")
long <- long %>%
  mutate(initial = answer(initial), final = answer(final),
         across(starts_with("time_"), num))

# Display order of the cases (renamed to case_order when reading each file)
order <- resp %>%
  select(response_no, do = case_order) %>%
  filter(do != "") %>%
  mutate(trial_id = str_split(do, "\\|")) %>%
  select(-do) %>% unnest(trial_id) %>%
  group_by(response_no) %>%
  mutate(position = row_number(), trial_id = str_extract(trial_id, "T\\d{2}")) %>%
  ungroup()
long <- long %>% left_join(order, by = c("response_no", "trial_id"))
if (anyNA(long$position)) stop("Answered case without a display position.")

# --- 4. Response-level flags ---------------------------------------------------------
n_complete <- long %>% group_by(response_no) %>%
  summarise(n_complete = sum(!is.na(initial) & !is.na(final)), .groups = "drop")

resp <- resp %>%
  left_join(n_complete, by = "response_no") %>%
  mutate(
    n_complete     = coalesce(n_complete, 0L),
    is_preview     = !(Status %in% c("IP Address", "0")) | DistributionChannel == "preview",
    before_start   = start_time < collection_start,
    test_pid       = str_detect(PID, regex(test_pid_pattern, ignore_case = TRUE)),
    consented      = consent == "Yes, I agree to take part",
    complete       = n_complete == n_cases,
    comp_pass_first = comp_baserate == comp_correct[["comp_baserate"]] &
                      comp_accuracy == comp_correct[["comp_accuracy"]] &
                      comp_feature  == comp_correct[["comp_feature"]],
    comp_pass_second = comp_baserate_2 == comp_correct[["comp_baserate"]] &
                       comp_accuracy_2 == comp_correct[["comp_accuracy"]] &
                       comp_feature_2  == comp_correct[["comp_feature"]],
    comp_failed_twice_calc = consented & !comp_pass_first & !comp_pass_second
  )

# The survey stores comp_failed_twice itself; check that it agrees with the answers
chk <- resp %>% filter(consented, Condition != "")
#stopifnot("comp_failed_twice in the data disagrees with the answers" =
#            all((chk$comp_failed_twice == "1") == chk$comp_failed_twice_calc)) (doesnt match because two finished twice, but did not finished. They are getting sortet out anyways)

# Median time per case = first-prediction page + final-prediction page
speed <- long %>%
  mutate(case_time = time_initial + time_final) %>%
  group_by(response_no) %>%
  summarise(median_case_time = median(case_time, na.rm = TRUE), .groups = "drop")
resp <- resp %>% left_join(speed, by = "response_no")

# --- 5. Sample flow ----------------------------------------------------------------
# Data cleaning (not participants): previews, test runs before the start of data
# collection, test PIDs. Then the preregistered exclusions, in the order listed
# in the preregistration.
log_rows <- list()
step <- function(data, keep, stage, rule) {
  out <- data[keep %in% TRUE, , drop = FALSE]
  log_rows[[length(log_rows) + 1]] <<- tibble(
    stage = stage, rule = rule, removed = nrow(data) - nrow(out), remaining = nrow(out))
  out
}
resp <- resp %>% mutate(exclusion = NA_character_)
mark <- function(ids, reason) {
  resp$exclusion[resp$response_no %in% ids & is.na(resp$exclusion)] <<- reason
}

s <- resp
log_rows[[1]] <- tibble(stage = "Start", rule = "All responses in the export(s)",
                        removed = 0L, remaining = nrow(s))
k <- !s$is_preview;   mark(s$response_no[!k], "cleaning: survey preview")
s <- step(s, k, "Cleaning", "Survey previews")
k <- !s$before_start; mark(s$response_no[!k], "cleaning: before data collection")
s <- step(s, k, "Cleaning", paste("Started before", format(collection_start, "%d %b %Y %H:%M %Z")))
k <- !s$test_pid;     mark(s$response_no[!k], "cleaning: test PID")
s <- step(s, k, "Cleaning", "Test PIDs")

k <- s$consented;     mark(s$response_no[!k], "excluded: no consent")
s <- step(s, k, "Preregistered", "Did not consent")
k <- s$complete;      mark(s$response_no[!k], "excluded: did not finish all 8 cases")
s <- step(s, k, "Preregistered", "Did not finish all 8 cases")
# Duplicate PIDs: keep the first (earliest) remaining response per PID.
# Responses without a PID cannot be checked and are kept.
s <- s %>% arrange(start_time)
k <- s$PID == "" | !duplicated(s$PID); mark(s$response_no[!k], "excluded: duplicate PID")
s <- step(s, k, "Preregistered", "Duplicate PID (later response)")

eligible <- s                                  # consented, complete, unique
if (nrow(eligible) == 0)
  stop("No eligible participants left after cleaning. If the export only contains test runs ",
       "from before ", format(collection_start, "%d %b %Y"), ", this is expected.")
k <- !(eligible$median_case_time < min_median_case_seconds)
mark(eligible$response_no[!k], "excluded: median time per case < 6 s")
s <- step(eligible, k, "Preregistered", "Median time per case < 6 s")
k <- !s$comp_failed_twice_calc; mark(s$response_no[!k], "excluded: failed comprehension check twice")
s <- step(s, k, "Preregistered", "Failed the comprehension check twice")
exclusion_log <- bind_rows(log_rows)
print(exclusion_log, n = Inf)

# --- 6. Participant ids (anonymous, in order of starting) -------------------------
eligible <- eligible %>%
  arrange(start_time) %>%
  mutate(participant = sprintf("P%03d", row_number()),
         excl_speed = median_case_time < min_median_case_seconds,
         excl_comprehension = comp_failed_twice_calc,
         included = !excl_speed & !excl_comprehension)

# --- 7. Trial-level analysis file -----------------------------------------------
trials_long <- long %>%
  inner_join(eligible %>% select(response_no, participant, included, excl_speed,
                                 excl_comprehension, condition = Condition),
             by = "response_no") %>%
  left_join(cases, by = "trial_id") %>%
  arrange(participant, position) %>%
  mutate(
    initial_correct = as.integer(initial == truth),
    final_correct   = as.integer(final == truth),
    agree_initial   = as.integer(initial == ai_pred),
    agree_final     = as.integer(final == ai_pred),
    switched        = as.integer(initial != final),
    switch_to_ai    = as.integer(agree_initial == 0 & agree_final == 1),
    switch_from_ai  = as.integer(agree_initial == 1 & agree_final == 0),
    # Reliance behaviour (Schoeffer et al., 2023)
    reliance_type = case_when(
      agree_final == 1 &  ai_correct ~ "correct adherence",
      agree_final == 1 & !ai_correct ~ "detrimental adherence",
      agree_final == 0 & !ai_correct ~ "corrective override",
      agree_final == 0 &  ai_correct ~ "detrimental override"),
    case_time = time_initial + time_final
  ) %>%
  group_by(participant) %>%
  mutate(ai_errors_before = lag(cumsum(!ai_correct), default = 0L)) %>%
  ungroup() %>%
  select(participant, condition, included, excl_speed, excl_comprehension,
         position, trial_id, truth, ai_pred, ai_correct, ai_confidence, conf_group,
         initial, final, initial_correct, final_correct, agree_initial, agree_final,
         switched, switch_to_ai, switch_from_ai, reliance_type, ai_errors_before,
         time_initial, time_final, time_feedback, case_time,
         race, sex, age, prior_count, charge_label, charge_degree, days_in_custody)

stopifnot(all(count(trials_long, participant)$n == n_cases),
          !anyNA(trials_long$final_correct),
          all(trials_long %>% group_by(participant) %>% summarise(ok = setequal(position, 1:n_cases)) %>% pull(ok)))

# --- 8. Participant-level analysis file -------------------------------------------
# Reliance quality Q (Schoeffer et al., 2023, Eq. 2): where final accuracy lies
# within the range attainable at the participant's adherence level A, given the
# accuracy of the AI on the cases this participant saw. 0 = worst, 1 = best.
# Defined only if the AI was right on at least half of the cases (the paper's
# setting) and the attainable range has width > 0.
reliance_quality <- function(A, acc_final, acc_ai) {
  W   <- ifelse(A <= 1 - acc_ai, 2 * A, ifelse(A <= acc_ai, 2 * (1 - acc_ai), 2 * (1 - A)))
  num <- ifelse(A <= 1 - acc_ai, acc_final - (1 - acc_ai - A), acc_final + (1 - acc_ai - A))
  ifelse(acc_ai >= 0.5 & W > 0, num / W, NA_real_)
}
ratio <- function(num, den) ifelse(den > 0, num / den, NA_real_)

part_trials <- trials_long %>%
  group_by(participant) %>%
  summarise(
    final_acc     = mean(final_correct),
    initial_acc   = mean(initial_correct),
    ai_acc_seen   = mean(ai_correct),
    n_ai_errors   = sum(!ai_correct),
    n_reoffend_seen = sum(truth),
    adherence     = mean(agree_final),
    share_correct_adherence     = mean(reliance_type == "correct adherence"),
    share_detrimental_adherence = mean(reliance_type == "detrimental adherence"),
    share_corrective_override   = mean(reliance_type == "corrective override"),
    share_detrimental_override  = mean(reliance_type == "detrimental override"),
    switch_rate   = mean(switched),
    # Schemmer et al. (2023): relative AI reliance / relative self-reliance
    rair = ratio(sum(initial_correct == 0 & ai_correct & final_correct == 1),
                 sum(initial_correct == 0 & ai_correct)),
    rsr  = ratio(sum(initial_correct == 1 & !ai_correct & final_correct == 1),
                 sum(initial_correct == 1 & !ai_correct)),
    median_case_time = median(case_time),
    .groups = "drop") %>%
  mutate(reliance_quality = reliance_quality(adherence, final_acc, ai_acc_seen))
stopifnot(all(is.na(part_trials$reliance_quality) |
                (part_trials$reliance_quality >= -1e-9 & part_trials$reliance_quality <= 1 + 1e-9)),
          isTRUE(all.equal(part_trials$final_acc,
                           part_trials$share_correct_adherence + part_trials$share_corrective_override)))

participants <- eligible %>%
  transmute(participant, condition = Condition, included, excl_speed, excl_comprehension,
            course = course_lab, study_order,
            source_file, start_date = as.Date(start_time, tz = local_tz),
            duration_min = num(`Duration (in seconds)`) / 60,
            comp_pass_first, comp_reread = comp_reread == "1",
            comp_failed_twice = comp_failed_twice_calc,
            across(all_of(items), ~ match(.x, likert7))) %>%
  left_join(part_trials, by = "participant")
stopifnot(!anyNA(participants$condition), all(participants$condition %in% c("feedback", "no_feedback")))

# --- 9. Write files -------------------------------------------------------------------
write_csv(participants, ap("data", "processed", "participants.csv"), na = "")
write_csv(trials_long,  ap("data", "processed", "trials_long.csv"),  na = "")
write_csv(exclusion_log, ap("data", "processed", "exclusion_log.csv"))
write_csv(resp %>%
            left_join(eligible %>% select(response_no, participant), by = "response_no") %>%
            transmute(response_no, participant, source_file, start_date = as.Date(start_time, tz = local_tz),
                      condition = na_if(Condition, ""), is_preview, before_start, test_pid,
                      consented, n_complete, median_case_time,
                      comp_failed_twice = comp_failed_twice_calc,
                      course = course_lab, pid_missing = PID == "",
                      exclusion = coalesce(exclusion, "included")),
          ap("data", "processed", "screening.csv"), na = "")
write_csv(eligible %>% select(participant, PID, ResponseId, source_file, start_time),
          ap("data", "private", "id_key.csv"))
write_csv(eligible %>% filter(open_comments != "") %>%
            select(participant, condition = Condition, open_comments),
          ap("data", "private", "open_comments.csv"))

cat("\nFinal sample:", sum(participants$included), "participants (",
    paste(names(table(participants$condition[participants$included])),
          table(participants$condition[participants$included]), sep = " = ", collapse = ", "),
    ")\n")
if (any(eligible$PID == "")) {
  cat("Note:", sum(eligible$PID == ""),
      "eligible response(s) without a valid PID (empty or placeholder; duplicates could not be checked):\n")
  print(eligible %>% filter(PID == "") %>% count(source_file, course = course_lab))
}
dups_any <- resp %>% filter(!is_preview, !before_start, !test_pid, PID != "") %>%
  count(PID) %>% filter(n > 1)
if (nrow(dups_any)) cat("Note:", nrow(dups_any), "PID(s) occur more than once among all responses",
                        "(see screening.csv).\n")
