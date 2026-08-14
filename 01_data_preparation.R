# ============================================================================
# Validation of the Moral Foundations Questionnaire-2 (MFQ-2) in Germany
# Data preparation: raw exports -> analytic datasets
# ----------------------------------------------------------------------------
# Reads the raw Unipark and Prolific exports, applies the pre-registered
# case-wise exclusions, constructs scales and grouping variables, and writes:
#   outputs/processed_data.csv  — internal full set (contains IDs)
#   outputs/final_dataset.csv   — public, de-identified set
#   outputs/exclusion_log.csv   — exclusion counts per step
# Requires the restricted raw data; NOT runnable from the public dataset.
# All analyses run on the de-identified set via data_analysis.R.
# Deviations from the pre-registered protocol are reported in the manuscript.
#
# Content:
#
# Phase 1: Prepare working environment
#   1.1 Packages
#   1.2 Global options
# Phase 2: Read and recode dataset
#   2.1 Import raw data
#   2.2 Rename variables
#   2.3 Merge demographic variables
#   2.4 Define variable vectors
#   2.5 Case-wise exclusions
#   2.6 Variable recoding and scale construction
#   2.7 Demographic factor labels
#   2.8 Analysis grouping variables
#   2.9 Export datasets (internal + de-identified) and exclusion log
# ============================================================================


#### --- Phase 1: Prepare working environment ----------------------------- ####

# --- 1.1 Packages --------------------------------------------------------- #
suppressPackageStartupMessages({
  library(tidyverse)   # v2.0.0
})

# --- 1.2 Global options --------------------------------------------------- #
options(scipen = 999)


#### --- Phase 2: Read and recode dataset --------------------------------- ####

# --- 2.1 Import raw data -------------------------------------------------- #
data_dir <- ifelse(dir.exists("data_and_analyses"), "data_and_analyses", ".")

# Dynamically find the latest data_project_*.csv file
raw_data_files <- list.files(path = data_dir, pattern = "^data_project_.*\\.csv$", full.names = TRUE)
if (length(raw_data_files) == 0) stop("No Unipark data file found.")
raw_data_path <- sort(raw_data_files, decreasing = TRUE)[1]
raw_data <- read.csv2(raw_data_path, header = TRUE, stringsAsFactors = FALSE)

# Rename p_0001 (Prolific ID)
raw_data <- raw_data %>% rename(prolific_pid = p_0001)

prolific_files <- list.files(path = data_dir, pattern = "^prolific_demographic_export_.*\\.csv$", full.names = TRUE)
if (length(prolific_files) == 0) stop("No Prolific data file found.")
prolific_path <- sort(prolific_files, decreasing = TRUE)[1]
prolific_data <- read.csv(prolific_path, header = TRUE, stringsAsFactors = FALSE)

# Rename Participant.id to match raw_data
prolific_data <- prolific_data %>% rename(prolific_pid = Participant.id)

message(sprintf("Unipark raw data imported. N = %d", nrow(raw_data)))
message(sprintf("Prolific demographic data imported. N = %d", nrow(prolific_data)))


# --- 2.2 Rename variables without meaningful aliases ---------------------- #
# Unipark exports use the alias names for variables that have them.
# The following variables have no alias in the codebook and must be renamed.
raw_data <- raw_data %>%
  rename(
    # AI detection task (LUMIR word unscrambling)
    ai_check_L = v_87,
    ai_check_U = v_88,
    ai_check_M = v_89,
    ai_check_I = v_90,
    ai_check_R = v_91,
    # Political ideology (social & economic sliders)
    pol_2_social = v_43,
    pol_3_economic = v_44,
    # Religiosity / spirituality slider
    religiosity = v_97
  )

# --- 2.3 Merge demographic variables -------------------------------------- #
# Inner join on Prolific ID. Cases without a Prolific demographic record are
# dropped pre-screening (cannot enter the nomological network).
n_unipark  <- nrow(raw_data)
n_prolific <- nrow(prolific_data)
raw_data   <- raw_data %>% inner_join(prolific_data, by = "prolific_pid")
n_merged   <- nrow(raw_data)
message(sprintf(
  "Merge: Unipark N = %d, Prolific N = %d, matched N = %d (dropped = %d).",
  n_unipark, n_prolific, n_merged, n_unipark - n_merged
))

raw_data <- raw_data %>%
  rename(age = Age, gender = Sex) %>%
  mutate(age = suppressWarnings(as.numeric(age)))

n_raw <- nrow(raw_data)
message(sprintf("Pre-screening N = %d.", n_raw))

# --- 2.4 Define variable vectors for convenience -------------------------- #

# MFQ-2 items: 36 items, 6 subscales × 6 items each (5-point Likert, 1–5)
mfq_care_items <- c(
  "mfq_1_care_1", "mfq_7_care_2", "mfq_13_care_3",
  "mfq_19_care_4", "mfq_25_care_5", "mfq_31_care_6"
)
mfq_equa_items <- c(
  "mfq_2_equa_1", "mfq_8_equa_2", "mfq_14_equa_3",
  "mfq_20_equa_4", "mfq_26_equa_5", "mfq_32_equa_6"
)
mfq_pro_items <- c(
  "mfq_3_pro_1", "mfq_9_pro_2", "mfq_15_pro_3",
  "mfq_21_pro_4", "mfq_27_pro_5", "mfq_33_pro_6"
)
mfq_loya_items <- c(
  "mfq_4_loya_1", "mfq_10_loya_2", "mfq_16_loya_3",
  "mfq_22_loya_4", "mfq_28_loya_5", "mfq_34_loya_6"
)
mfq_auth_items <- c(
  "mfq_5_auth_1", "mfq_11_auth_2", "mfq_17_auth_3",
  "mfq_23_auth_4", "mfq_29_auth_5", "mfq_35_auth_6"
)
mfq_pur_items <- c(
  "mfq_6_pur_1", "mfq_12_pur_2", "mfq_18_pur_3",
  "mfq_24_pur_4", "mfq_30_pur_5", "mfq_36_pur_6"
)

mfq_all_items <- c(
  mfq_care_items, mfq_equa_items, mfq_pro_items,
  mfq_loya_items, mfq_auth_items, mfq_pur_items
)

# 7-factor Loyalty split (Zakharin & Bates, 2023)
mfq_country_loyalty_items <- c("mfq_4_loya_1", "mfq_10_loya_2", "mfq_22_loya_4")
mfq_group_loyalty_items <- c("mfq_16_loya_3", "mfq_28_loya_5", "mfq_34_loya_6")

# BFI-2-S items: Extraversion (6 items) & Agreeableness (6 items), 5-point Likert
bfi_extra_items <- c(
  "bfi_1_extra_1r", "bfi2_6_extra_2", "bfi_11_extra_3",
  "bfi_16_extra_4", "bfi_21_extra_5r", "bfi_26_extra_6r"
)
bfi_agree_items <- c(
  "bfi_2_agree_1", "bfi_7_agree_2r", "bfi_12_agree_3",
  "bfi_17_agree_4r", "bfi_22_agree_5", "bfi_27_agree_6r"
)

# Items to reverse-code (indicated by 'r' suffix)
bfi_reverse_items <- c(
  "bfi_1_extra_1r", "bfi_21_extra_5r", "bfi_26_extra_6r",
  "bfi_7_agree_2r", "bfi_17_agree_4r", "bfi_27_agree_6r"
)

# EPQ-5 items: Idealism (5 items) & Relativism (5 items), 5-point Likert
epq_idea_items <- c(
  "epq_1_idea_1", "epq_2_idea_2", "epq_3_idea_3",
  "epq_4_idea_4", "epq_5_idea_5"
)
epq_rela_items <- c(
  "epq_6_rela_1", "epq_7_rela_2", "epq_8_rela_3",
  "epq_9_rela_4", "epq_10_rela_5"
)

attention_items <- c("attention_check_1", "attention_check_2", "attention_check_3")

pol_items <- c("pol_1_lrscale", "pol_2_social", "pol_3_economic")


# --- 2.5 Case-wise exclusions --------------------------------------------- #
exclusion_log <- data.frame(
  step = character(), n_excluded = integer(), n_remaining = integer(),
  stringsAsFactors = FALSE
)

log_exclusion <- function(step_name, data_before, data_after) {
  n_excl <- nrow(data_before) - nrow(data_after)
  message(sprintf("  %s: excluded %d, remaining %d.", step_name, n_excl, nrow(data_after)))
  exclusion_log <<- rbind(exclusion_log, data.frame(
    step = step_name, n_excluded = n_excl, n_remaining = nrow(data_after)
  ))
}

# Log Prolific merge (2.3) as additional exclusion step (not pre-registered)
exclusion_log <- rbind(exclusion_log, data.frame(
  step = "No matching Prolific demographic record (merge)",
  n_excluded = n_unipark - n_merged, n_remaining = n_merged
))

message(sprintf("Starting N = %d.", n_raw))

# Step 0: dispcode == 31 (completed responses only)
d <- raw_data %>% filter(dispcode == 31)
log_exclusion("Completed surveys (dispcode == 31)", raw_data, d)

# Step 1: AI agent detection task (LUMIR; expected: L=1, U=2, M=3, I=4, R=5)
d_before <- d
d <- d %>% filter(
  ai_check_L == 1 & ai_check_U == 2 & ai_check_M == 3 &
    ai_check_I == 4 & ai_check_R == 5
)
log_exclusion("AI detection task (LUMIR)", d_before, d)

# Step 2: Instructed-response attention checks
# attention_check_1 = 2, attention_check_2 = 3, attention_check_3 = 5
d_before <- d
d <- d %>% filter(
  attention_check_1 == 2 & attention_check_2 == 3 & attention_check_3 == 5
)
log_exclusion("Attention checks (>= 1 failed)", d_before, d)

# Step 3: Invariant responding (SD < .50 across 36 MFQ-2 items; Leiner, 2019)
d_before <- d
d <- d %>%
  mutate(mfq_sd = apply(select(., all_of(mfq_all_items)), 1, sd, na.rm = TRUE)) %>%
  filter(mfq_sd >= 0.50) %>%
  select(-mfq_sd)
log_exclusion("Invariant responding (SD < .50)", d_before, d)

# Step 4: Self-reported AI agent use (AI_agent_use == 1)
d_before <- d
d <- d %>% filter(AI_agent_use != 1)
log_exclusion("Self-reported AI agent use", d_before, d)

# --- Exclusion summary -------------------------------------------------------
n_final <- nrow(d)
n_excluded_total <- n_raw - n_final
message(sprintf(
  "Exclusion summary: %d of %d excluded (%.1f%%). Final N = %d",
  n_excluded_total, n_raw, 100 * n_excluded_total / n_raw, n_final
))
print(exclusion_log)


# --- 2.6 Variable recoding and scale construction ------------------------- #

# Reverse-code BFI-2-S items (5-point scale: recoded = 6 - original)
d <- d %>%
  mutate(across(all_of(bfi_reverse_items), ~ 6 - .x, .names = "{.col}_rec"))
for (item in bfi_reverse_items) {
  d[[item]] <- d[[paste0(item, "_rec")]]
}
d <- d %>% select(-ends_with("_rec"))

# MFQ-2 subscale means (Care, Equality, Proportionality, Loyalty, Authority, Purity)
d <- d %>%
  mutate(
    care_mean            = rowMeans(select(., all_of(mfq_care_items)), na.rm = TRUE),
    equality_mean        = rowMeans(select(., all_of(mfq_equa_items)), na.rm = TRUE),
    proportionality_mean = rowMeans(select(., all_of(mfq_pro_items)),  na.rm = TRUE),
    loyalty_mean         = rowMeans(select(., all_of(mfq_loya_items)), na.rm = TRUE),
    authority_mean       = rowMeans(select(., all_of(mfq_auth_items)), na.rm = TRUE),
    purity_mean          = rowMeans(select(., all_of(mfq_pur_items)),  na.rm = TRUE)
  )

mfq_subscale_vars <- c(
  "care_mean", "equality_mean", "proportionality_mean",
  "loyalty_mean", "authority_mean", "purity_mean"
)

# Composite scores for H3 (Individualizing = Care, Equality; Binding = rest)
d <- d %>%
  mutate(
    individualizing_mean = (care_mean + equality_mean) / 2,
    binding_mean         = (proportionality_mean + loyalty_mean +
                            authority_mean + purity_mean) / 4,
    ind_bind_diff        = individualizing_mean - binding_mean
  )

# BFI-2-S subscale means (Extraversion, Agreeableness)
d <- d %>%
  mutate(
    extraversion_mean  = rowMeans(select(., all_of(bfi_extra_items)), na.rm = TRUE),
    agreeableness_mean = rowMeans(select(., all_of(bfi_agree_items)), na.rm = TRUE)
  )

# EPQ-5 subscale means (Idealism, Relativism)
d <- d %>%
  mutate(
    idealism_mean   = rowMeans(select(., all_of(epq_idea_items)), na.rm = TRUE),
    relativism_mean = rowMeans(select(., all_of(epq_rela_items)), na.rm = TRUE)
  )

# East/West Germany classification (former GDR states; Berlin separate;
# post-protocol specification)
d <- d %>%
  mutate(
    east_west = case_when(
      de_bundesland %in% c(4, 8, 13, 14, 16) ~ "East",
      de_bundesland == 3                     ~ "Berlin",
      TRUE                                   ~ "West"
    )
  )

# Political orientation groups (Liberal <= 4, Conservative >= 7, Middle 5-6;
# post-protocol specification)
d <- d %>%
  mutate(
    pol_group = case_when(
      pol_1_lrscale <= 4 ~ "Liberal",
      pol_1_lrscale >= 7 ~ "Conservative",
      TRUE               ~ "Middle"
    )
  )

# Religiosity groups (0-100 slider; post-protocol specification).
# religiosity_group     : median split (primary, data-driven)
# religiosity_group_mid : scale midpoint = 50 (sensitivity)
religiosity_median   <- median(d$religiosity, na.rm = TRUE)
religiosity_midpoint <- 50
d <- d %>%
  mutate(
    religiosity_group     = ifelse(religiosity <= religiosity_median,
                                   "Non-religious", "Religious"),
    religiosity_group_mid = ifelse(religiosity <  religiosity_midpoint,
                                   "Non-religious", "Religious")
  )
message(sprintf(
  "Religiosity (median = %.1f): Non-religious n = %d, Religious n = %d.",
  religiosity_median,
  sum(d$religiosity_group == "Non-religious"),
  sum(d$religiosity_group == "Religious")
))
message(sprintf(
  "Religiosity (midpoint = 50, sensitivity): Non-religious n = %d, Religious n = %d.",
  sum(d$religiosity_group_mid == "Non-religious"),
  sum(d$religiosity_group_mid == "Religious")
))


# --- 2.7 Demographic factor labels ---------------------------------------- #
d <- d %>%
  mutate(
    pol_4_party = factor(pol_4_party,
      levels = 1:13,
      labels = c(
        "CDU", "CSU", "SPD", "Bündnis 90 / Die Grünen",
        "FDP", "AfD", "Die Linke", "BSW",
        "Freie Wähler", "Volt", "Die Partei", "ÖDP",
        "andere / nicht genannt"
      )
    ),
    de_bundesland = factor(de_bundesland,
      levels = 1:16,
      labels = c(
        "Baden Württemberg", "Bayern", "Berlin", "Brandenburg",
        "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
        "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
        "Saarland", "Sachsen", "Sachsen-Anhalt",
        "Schleswig-Holstein", "Thüringen"
      )
    )
  )


# --- 2.8 Analysis grouping variables ------------------------------------- #
# Age cohorts (split at 35; post-protocol specification; missing age -> NA).
d <- d %>%
  mutate(age_group = case_when(
    is.na(age) ~ NA_character_,
    age <= 35  ~ "Young",
    TRUE       ~ "Older"
  ))
# Gender dummy for the E3 regression (Male = 0, Female = 1).
d <- d %>% mutate(gender_dummy = ifelse(gender == "Male", 0, 1))


# --- 2.9 Export analytic datasets (internal + de-identified public) ------- #
# Internal and public files hold identical cases and analytic values; the
# public file is built from an explicit keep-list (analytic columns only).
out_dir <- file.path(data_dir, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Internal full dataset (contains Prolific ID, IP, geolocation, timestamps;
# git-ignored, NOT for public release).
write.csv(d, file.path(out_dir, "processed_data.csv"), row.names = FALSE)

# De-identified public dataset (repository release).
public_keep <- c(
  "age", "gender", "de_bundesland",
  mfq_all_items, attention_items,
  "ai_check_L", "ai_check_U", "ai_check_M", "ai_check_I", "ai_check_R",
  "AI_agent_use",
  "pol_1_lrscale", "pol_2_social", "pol_3_economic", "pol_4_party", "religiosity",
  bfi_extra_items, bfi_agree_items, epq_idea_items, epq_rela_items,
  mfq_subscale_vars, "individualizing_mean", "binding_mean", "ind_bind_diff",
  "extraversion_mean", "agreeableness_mean", "idealism_mean", "relativism_mean",
  "east_west", "pol_group", "religiosity_group", "religiosity_group_mid",
  "age_group", "gender_dummy"
)
d_public <- d %>%
  mutate(participant_id = row_number()) %>%
  select(participant_id, any_of(public_keep))
write.csv(d_public, file.path(out_dir, "final_dataset.csv"),
          row.names = FALSE)
message(sprintf(
  "Datasets exported: processed_data.csv (internal, %d cols), final_dataset.csv (public, %d cols).",
  ncol(d), ncol(d_public)
))

# Exclusion log (aggregate counts; shipped with the public dataset for fig_1).
write.csv(exclusion_log, file.path(out_dir, "exclusion_log.csv"), row.names = FALSE)
message(sprintf("Exclusion log written (%d steps).", nrow(exclusion_log)))