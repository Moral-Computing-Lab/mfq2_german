# ============================================================================
# Validation of the Moral Foundations Questionnaire-2 (MFQ-2) in Germany
# Analyses: structural validity, reliability, invariance, nomological network
# ----------------------------------------------------------------------------
# Reproduces all analyses, result tables, and figures from the de-identified
# final dataset:
#   final_dataset.csv  -- analytic data (produced by 01_data_preparation.R)
#   exclusion_log.csv  -- exclusion counts (for fig_1: CONSORT)
# No raw or identifiable data required.
# Deviations from the pre-registered protocol are reported in the manuscript.
#
# Content:
#
# Phase 1: Prepare working environment
#   1.1 Packages
#   1.2 Global options
# Phase 2: Load analytic dataset
#   2.1 Load de-identified dataset and exclusion log
#   2.2 Variable vectors
# Phase 3: Inspect dataset
#   3.1 Sample demographics
#   3.2 MFQ-2 item descriptives
#   3.3 MFQ-2 subscale descriptives
#   3.4 Validation variable descriptives
#   3.5 MFQ-2 subscale intercorrelations
# Phase 4: Check statistical assumptions
#   4.1 Multivariate normality (MFQ-2 subscales)
#   4.2 Regression diagnostics (H3)
#   4.3 Multiple regression diagnostics (E3)
# Phase 5: Main analyses
#   5.1 Sample splitting
#   5.2 Structural validation (H1a)
#   5.3 Replication in validation subsample
#   5.4 Internal consistency reliability (H1b)
#   5.5 Measurement invariance
#   5.6 Nomological network (H2-H6)
#   5.7 Exploratory analyses (E1-E3)
#   5.9 Additional analyses (not pre-registered)
# Phase 6: Figures
#   6.1 Theme and output directory
#   6.2 Participant flow diagram (CONSORT)
#   6.3 MFQ-2 subscale distributions
#   6.4 Q-Q plots
#   6.5 Intercorrelation heatmap
#   6.6 ESEM standardized factor loadings
#   6.7 SEM path diagram (primary CFA 7-factor measurement model)
#   6.8 Group comparison profile
#   6.9 H3 scatter
#   6.10 Reliability
# ============================================================================


#### --- Phase 1: Prepare working environment ----------------------------- ####

# --- 1.1 Packages --------------------------------------------------------- #
# Versions of the confirmatory run; full record in outputs/session_info.txt.
suppressPackageStartupMessages({
  library(tidyverse)   # v2.0.0
  library(lavaan)      # v0.6-21
  library(semTools)    # v0.5-8
  library(psych)       # v2.6.3
  library(moments)     # v0.14.1
  library(GPArotation) # v2025.3-1
  library(effectsize)  # v1.0.2
  library(car)         # v3.1-5
  library(ggplot2)     # v4.0.3
  library(knitr)       # v1.51
  library(pwr)         # v1.3-0
  library(semPlot)     # v1.1.8
})

# --- 1.2 Global options --------------------------------------------------- #
options(scipen = 999)
# RNG pinned so the split and the geomin starts stay identical across R versions.
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
set.seed(2026)


#### --- Phase 2: Load analytic dataset ----------------------------------- ####

# --- 2.1 Load de-identified dataset and exclusion log --------------------- #
data_dir <- "."
out_dir  <- file.path(data_dir, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Locate inputs in the working directory or an outputs/ subfolder.
find_file <- function(name) {
  cands <- c(name, file.path("outputs", name), file.path(out_dir, name))
  hit   <- cands[file.exists(cands)]
  if (length(hit) == 0) stop(sprintf("Required input not found: %s", name))
  hit[1]
}
d             <- read.csv(find_file("final_dataset.csv"), stringsAsFactors = FALSE)
exclusion_log <- read.csv(find_file("exclusion_log.csv"),             stringsAsFactors = FALSE)
message(sprintf("Analytic dataset loaded: N = %d.", nrow(d)))


# --- 2.2 Variable vectors ------------------------------------------------- #

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
# Subscale score columns (constructed in data_preparation.R).
mfq_subscale_vars <- c(
  "care_mean", "equality_mean", "proportionality_mean",
  "loyalty_mean", "authority_mean", "purity_mean"
)


#### --- Phase 3: Inspect dataset ----------------------------------------- ####

# --- 3.1 Sample demographics ---------------------------------------------- #
message(sprintf("Final N = %d", nrow(d)))

message(sprintf(
  "Age: M = %.2f, SD = %.2f, Range = %.0f-%.0f",
  mean(d$age, na.rm = TRUE), sd(d$age, na.rm = TRUE),
  min(d$age, na.rm = TRUE), max(d$age, na.rm = TRUE)
))

message("Gender distribution:")
print(table(d$gender, useNA = "ifany"))

message("Federal state distribution:")
print(table(d$de_bundesland, useNA = "ifany"))

message("East/West distribution:")
print(table(d$east_west, useNA = "ifany"))

message("Party affiliation:")
print(table(d$pol_4_party, useNA = "ifany"))

message("Political orientation groups (Liberal/Middle/Conservative):")
print(table(d$pol_group, useNA = "ifany"))

# --- 3.2 MFQ-2 item descriptives ------------------------------------------ #
mfq_item_desc <- d %>%
  select(all_of(mfq_all_items)) %>%
  summarise(across(everything(), list(
    M    = ~ mean(.x, na.rm = TRUE),
    SD   = ~ sd(.x, na.rm = TRUE),
    Skew = ~ moments::skewness(.x, na.rm = TRUE),
    Kurt = ~ moments::kurtosis(.x, na.rm = TRUE) - 3
  ))) %>%
  pivot_longer(everything(),
    names_to = c("item", ".value"),
    names_pattern = "(.+)_(M|SD|Skew|Kurt)$"
  ) %>%
  mutate(
    flag_skew = ifelse(abs(Skew) > 2, "***", ""),
    flag_kurt = ifelse(abs(Kurt) > 7, "***", "")
  )
message("MFQ-2 item descriptives (flag: |Skew| > 2 or |Kurt| > 7).")
print(as.data.frame(mfq_item_desc), row.names = FALSE)

# --- 3.3 MFQ-2 subscale descriptives -------------------------------------- #
mfq_sub_desc <- d %>%
  select(all_of(mfq_subscale_vars)) %>%
  summarise(across(everything(), list(
    M    = ~ mean(.x, na.rm = TRUE),
    SD   = ~ sd(.x, na.rm = TRUE),
    Min  = ~ min(.x, na.rm = TRUE),
    Max  = ~ max(.x, na.rm = TRUE),
    Skew = ~ moments::skewness(.x, na.rm = TRUE),
    Kurt = ~ moments::kurtosis(.x, na.rm = TRUE) - 3
  ))) %>%
  pivot_longer(everything(),
    names_to = c("subscale", ".value"),
    names_pattern = "(.+)_(M|SD|Min|Max|Skew|Kurt)$"
  )
message("MFQ-2 subscale descriptives.")
print(as.data.frame(mfq_sub_desc), row.names = FALSE)

# --- 3.4 Validation variable descriptives --------------------------------- #
validation_vars <- c(
  "pol_1_lrscale", "pol_2_social", "pol_3_economic",
  "religiosity", "extraversion_mean", "agreeableness_mean",
  "idealism_mean", "relativism_mean"
)
val_desc <- d %>%
  select(all_of(validation_vars)) %>%
  summarise(across(everything(), list(
    M    = ~ mean(.x, na.rm = TRUE),
    SD   = ~ sd(.x, na.rm = TRUE),
    Skew = ~ moments::skewness(.x, na.rm = TRUE),
    Kurt = ~ moments::kurtosis(.x, na.rm = TRUE) - 3
  ))) %>%
  pivot_longer(everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(M|SD|Skew|Kurt)$"
  )
message("Validation variable descriptives.")
print(as.data.frame(val_desc), row.names = FALSE)

# --- 3.5 MFQ-2 subscale intercorrelations --------------------------------- #
mfq_cor <- cor(d[, mfq_subscale_vars], use = "pairwise.complete.obs")
message("MFQ-2 subscale intercorrelation matrix.")
print(round(mfq_cor, 3))


#### --- Phase 4: Check statistical assumptions --------------------------- ####

# --- 4.1 Multivariate normality (MFQ-2 subscales) ------------------------- #
mardia_test <- psych::mardia(d[, mfq_subscale_vars], plot = FALSE)
message(sprintf(
  "Mardia: skew = %.2f (p = %.3f), kurt = %.2f (p = %.3f).",
  mardia_test$b1p, mardia_test$p.skew, mardia_test$b2p, mardia_test$p.kurt
))

# --- 4.2 Regression diagnostics (H3) -------------------------------------- #
fit_h3_diag <- lm(ind_bind_diff ~ pol_1_lrscale, data = d)
sw_h3 <- shapiro.test(residuals(fit_h3_diag))$p.value
bp_h3 <- car::ncvTest(fit_h3_diag)$p
message(sprintf(
  "H3 diagnostics: Shapiro-Wilk p = %.3f, ncvTest p = %.3f.", sw_h3, bp_h3
))

# --- 4.3 Multiple regression diagnostics (E3) ----------------------------- #
# Per DV: max VIF, ncvTest, Shapiro-Wilk. Predictor set as in 5.7.3.
e3_predictor_vars    <- c("pol_1_lrscale", "religiosity", "agreeableness_mean",
                          "extraversion_mean", "age")
d_diag               <- d %>% mutate(gender_dummy = ifelse(gender == "Male", 0, 1))
e3_predictor_formula <- paste(c(e3_predictor_vars, "gender_dummy"), collapse = " + ")
e3_diagnostics <- lapply(mfq_subscale_vars, function(sub) {
  fit <- lm(as.formula(paste(sub, "~", e3_predictor_formula)), data = d_diag)
  data.frame(
    DV        = sub,
    max_VIF   = max(car::vif(fit)),
    ncv_p     = car::ncvTest(fit)$p,
    shapiro_p = shapiro.test(residuals(fit))$p.value,
    stringsAsFactors = FALSE
  )
})
e3_diagnostics <- do.call(rbind, e3_diagnostics)
message("E3 regression diagnostics:")
for (i in seq_len(nrow(e3_diagnostics))) {
  r <- e3_diagnostics[i, ]
  message(sprintf(
    "  %-22s max VIF = %.2f, ncvTest p = %s, Shapiro-Wilk p = %s.",
    r$DV, r$max_VIF,
    ifelse(r$ncv_p     < .001, "< .001", sprintf("%.3f", r$ncv_p)),
    ifelse(r$shapiro_p < .001, "< .001", sprintf("%.3f", r$shapiro_p))
  ))
}


#### --- Phase 5: Main analyses ------------------------------------------- ####

# --- 5.1 Sample splitting ------------------------------------------------- #
# 50/50 split; seed re-set directly before it for partition reproducibility.
set.seed(2026)
split_index   <- sample(seq_len(nrow(d)), size = floor(nrow(d) / 2))
d_calibration <- d[split_index, ]
d_validation     <- d[-split_index, ]
message(sprintf("Calibration N = %d, Validation N = %d.",
                nrow(d_calibration), nrow(d_validation)))

message("Calibration vs. validation subscale means:")
for (sub in mfq_subscale_vars) {
  m_cal <- mean(d_calibration[[sub]], na.rm = TRUE)
  m_hol <- mean(d_validation[[sub]],     na.rm = TRUE)
  message(sprintf("  %s: M_cal = %.3f, M_hol = %.3f.", sub, m_cal, m_hol))
}


# --- 5.2 Structural validation (H1a) -------------------------------------- #
message("Structural validation (H1a).")

# --- Helper: report fit indices ----------------------------------------------
# Robust (scaled) indices where available; naive indices as fallback.
report_fit <- function(fit, model_name) {
  fm <- fitMeasures(fit)
  pick <- function(robust, naive) {
    if (robust %in% names(fm) && is.finite(fm[robust])) unname(fm[robust])
    else if (naive %in% names(fm)) unname(fm[naive])
    else NA_real_
  }
  fi <- c(
    chisq          = pick("chisq.scaled",  "chisq"),
    df             = pick("df.scaled",     "df"),
    pvalue         = pick("pvalue.scaled", "pvalue"),
    cfi            = pick("cfi.robust",    "cfi"),
    tli            = pick("tli.robust",    "tli"),
    rmsea          = pick("rmsea.robust",  "rmsea"),
    rmsea.ci.lower = pick("rmsea.ci.lower.robust", "rmsea.ci.lower"),
    rmsea.ci.upper = pick("rmsea.ci.upper.robust", "rmsea.ci.upper"),
    srmr           = pick("srmr", "srmr"),
    aic            = pick("aic",  "aic"),
    bic            = pick("bic",  "bic"),
    npar           = pick("npar", "npar")
  )
  message(sprintf("[%s] fit indices (robust where available):", model_name))
  message(sprintf("  chi2(%.0f) = %.2f, p = %.3f.",
                  fi["df"], fi["chisq"], fi["pvalue"]))
  message(sprintf("  CFI = %.3f, TLI = %.3f.", fi["cfi"], fi["tli"]))
  message(sprintf("  RMSEA = %.3f [90%% CI: %.3f, %.3f].",
                  fi["rmsea"], fi["rmsea.ci.lower"], fi["rmsea.ci.upper"]))
  message(sprintf("  SRMR = %.3f.", fi["srmr"]))
  message(sprintf("  AIC = %.2f, BIC = %.2f (npar = %.0f).",
                  fi["aic"], fi["bic"], fi["npar"]))

  if (fi["cfi"] > .95 & fi["tli"] > .95 & fi["rmsea"] < .06 & fi["srmr"] < .08) {
    message("  Evaluation: good.")
  } else if (fi["cfi"] > .90 & fi["tli"] > .90 & fi["rmsea"] < .08 & fi["srmr"] < .08) {
    message("  Evaluation: acceptable.")
  } else {
    message("  Evaluation: below acceptable thresholds.")
  }
  return(fi)
}


# --- 5.2.1 ESEM 6-factor (primary) — calibration -------------------------- #
message("ESEM 6-factor (calibration).")

esem_6f_syntax <- '
  efa("efa1")*Care +
  efa("efa1")*Equality +
  efa("efa1")*Proportionality +
  efa("efa1")*Loyalty +
  efa("efa1")*Authority +
  efa("efa1")*Purity =~
    mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
    mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6 +
    mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
    mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6 +
    mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
    mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6 +
    mfq_4_loya_1 + mfq_10_loya_2 + mfq_16_loya_3 +
    mfq_22_loya_4 + mfq_28_loya_5 + mfq_34_loya_6 +
    mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
    mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6 +
    mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
    mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
'

set.seed(2026)  # reproducible geomin random starts
fit_esem_6f_cal <- sem(
  esem_6f_syntax,
  data      = d_calibration,
  estimator = "MLR",
  missing   = "fiml",
  rotation  = "geomin",
  rotation.args = list(orthogonal = FALSE, geomin.epsilon = 0.01, rstarts = 100)
)
fi_esem_6f_cal <- report_fit(fit_esem_6f_cal, "ESEM 6-factor (calibration)")

esem_6f_loadings_lv <- standardizedSolution(fit_esem_6f_cal) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, pvalue) %>%
  rename(Factor = lhs, Item = rhs, Lambda = est.std, p = pvalue)
message("Standardized factor loadings (ESEM 6-factor).")
print(as.data.frame(esem_6f_loadings_lv), row.names = FALSE)

mfq_target_lookup <- c(care = "Care", equa = "Equality", pro = "Proportionality",
                       loya = "Loyalty", auth = "Authority", pur = "Purity")
cross_load_check <- esem_6f_loadings_lv %>%
  mutate(
    target_factor = unname(
      mfq_target_lookup[sub("^mfq_\\d+_([a-z]+)_.*$", "\\1", Item)]
    )
  ) %>%
  group_by(Item) %>%
  mutate(
    target_loading = abs(Lambda[Factor == target_factor][1]),
    is_target      = Factor == target_factor,
    exceeds_030    = !is_target & abs(Lambda) > 0.30,
    diff_lt_020    = !is_target & (target_loading - abs(Lambda)) < 0.20
  ) %>%
  ungroup() %>%
  filter(exceeds_030 | diff_lt_020)
if (nrow(cross_load_check) > 0) {
  message("Cross-loading flags (non-target |lambda| > .30 or target - non-target diff < .20):")
  print(as.data.frame(cross_load_check), row.names = FALSE)
} else {
  message("No problematic cross-loadings.")
}

esem_6f_factor_cors <- standardizedSolution(fit_esem_6f_cal) %>%
  filter(op == "~~",
         lhs %in% unname(mfq_target_lookup),
         rhs %in% unname(mfq_target_lookup),
         lhs != rhs) %>%
  select(lhs, rhs, est.std, pvalue)
message("Latent factor correlations (ESEM 6-factor, calibration):")
print(as.data.frame(esem_6f_factor_cors), row.names = FALSE)


# --- 5.2.2 CFA 6-factor — calibration ------------------------------------- #
message("CFA 6-factor (calibration).")

cfa_6f_syntax <- "
  Care            =~ mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
                     mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6
  Equality        =~ mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
                     mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6
  Proportionality =~ mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
                     mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6
  Loyalty         =~ mfq_4_loya_1 + mfq_10_loya_2 + mfq_16_loya_3 +
                     mfq_22_loya_4 + mfq_28_loya_5 + mfq_34_loya_6
  Authority       =~ mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
                     mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6
  Purity          =~ mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
                     mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
"

fit_cfa_6f_cal <- cfa(
  cfa_6f_syntax,
  data      = d_calibration,
  estimator = "MLR",
  missing   = "fiml"
)
fi_cfa_6f_cal <- report_fit(fit_cfa_6f_cal, "CFA 6-factor (calibration)")

cfa_6f_sl <- standardizedSolution(fit_cfa_6f_cal) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, pvalue)
message("Standardized factor loadings (CFA 6-factor).")
print(as.data.frame(cfa_6f_sl), row.names = FALSE)

weak_items_cfa6 <- cfa_6f_sl %>% filter(abs(est.std) < 0.50)
if (nrow(weak_items_cfa6) > 0) {
  message("Loadings < .50:")
  print(as.data.frame(weak_items_cfa6), row.names = FALSE)
}

mi_cfa_6f_top <- modindices(fit_cfa_6f_cal, sort. = TRUE) %>%
  filter(mi > 20.0) %>%
  head(10)
message("Modification indices (top 10, MI > 20).")
print(as.data.frame(mi_cfa_6f_top), row.names = FALSE)


# --- 5.2.3 CFA 7-factor (Loyalty split) — calibration --------------------- #
message("CFA 7-factor (calibration).")

cfa_7f_syntax <- "
  Care            =~ mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
                     mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6
  Equality        =~ mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
                     mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6
  Proportionality =~ mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
                     mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6
  CountryLoyalty  =~ mfq_4_loya_1 + mfq_10_loya_2 + mfq_22_loya_4
  GroupLoyalty     =~ mfq_16_loya_3 + mfq_28_loya_5 + mfq_34_loya_6
  Authority       =~ mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
                     mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6
  Purity          =~ mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
                     mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
"

fit_cfa_7f_cal <- cfa(
  cfa_7f_syntax,
  data      = d_calibration,
  estimator = "MLR",
  missing   = "fiml"
)

fi_cfa_7f_cal <- report_fit(fit_cfa_7f_cal, "CFA 7-Factor (Calibration)")


# --- 5.2.4 ESEM 7-Factor Model – Calibration Sample ----------------------- #
message("ESEM 7-Factor Model (Calibration Sample)")

esem_7f_syntax <- '
  efa("efa1")*Care +
  efa("efa1")*Equality +
  efa("efa1")*Proportionality +
  efa("efa1")*CountryLoyalty +
  efa("efa1")*GroupLoyalty +
  efa("efa1")*Authority +
  efa("efa1")*Purity =~
    mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
    mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6 +
    mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
    mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6 +
    mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
    mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6 +
    mfq_4_loya_1 + mfq_10_loya_2 + mfq_16_loya_3 +
    mfq_22_loya_4 + mfq_28_loya_5 + mfq_34_loya_6 +
    mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
    mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6 +
    mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
    mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
'

set.seed(2026)  # reproducible geomin random starts
fit_esem_7f_cal <- sem(
  esem_7f_syntax,
  data      = d_calibration,
  estimator = "MLR",
  missing   = "fiml",
  rotation  = "geomin",
  rotation.args = list(orthogonal = FALSE, geomin.epsilon = 0.01, rstarts = 100)
)
fi_esem_7f_cal <- report_fit(fit_esem_7f_cal, "ESEM 7-factor (calibration)")


# --- 5.2.5 Second-order CFA — calibration --------------------------------- #
# Higher-order: Individualizing (Care, Equality); Binding (remaining factors).
message("Second-order CFA (calibration).")

bic_6f_best <- min(fi_esem_6f_cal["bic"], fi_cfa_6f_cal["bic"], na.rm = TRUE)
bic_7f_best <- min(fi_esem_7f_cal["bic"], fi_cfa_7f_cal["bic"], na.rm = TRUE)
delta_bic_loyalty <- bic_6f_best - bic_7f_best
loyalty_split <- is.finite(delta_bic_loyalty) && delta_bic_loyalty >= 2
message(sprintf(
  "Loyalty structure decision: BIC_6f = %.2f, BIC_7f = %.2f, delta = %.2f -> %s (evidence: %s).",
  bic_6f_best, bic_7f_best, delta_bic_loyalty,
  ifelse(loyalty_split, "7-factor split (Country/Group Loyalty)",
         "6-factor structure (parsimony)"),
  ifelse(abs(delta_bic_loyalty) > 10, "strong",
         ifelse(abs(delta_bic_loyalty) >= 2, "moderate", "indistinguishable"))
))

second_order_first_order_6f <- "
  Care            =~ mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
                     mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6
  Equality        =~ mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
                     mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6
  Proportionality =~ mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
                     mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6
  Loyalty         =~ mfq_4_loya_1 + mfq_10_loya_2 + mfq_16_loya_3 +
                     mfq_22_loya_4 + mfq_28_loya_5 + mfq_34_loya_6
  Authority       =~ mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
                     mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6
  Purity          =~ mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
                     mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
  Individualizing =~ Care + Equality
  Binding         =~ Proportionality + Loyalty + Authority + Purity
"
second_order_first_order_7f <- "
  Care            =~ mfq_1_care_1 + mfq_7_care_2 + mfq_13_care_3 +
                     mfq_19_care_4 + mfq_25_care_5 + mfq_31_care_6
  Equality        =~ mfq_2_equa_1 + mfq_8_equa_2 + mfq_14_equa_3 +
                     mfq_20_equa_4 + mfq_26_equa_5 + mfq_32_equa_6
  Proportionality =~ mfq_3_pro_1 + mfq_9_pro_2 + mfq_15_pro_3 +
                     mfq_21_pro_4 + mfq_27_pro_5 + mfq_33_pro_6
  CountryLoyalty  =~ mfq_4_loya_1 + mfq_10_loya_2 + mfq_22_loya_4
  GroupLoyalty    =~ mfq_16_loya_3 + mfq_28_loya_5 + mfq_34_loya_6
  Authority       =~ mfq_5_auth_1 + mfq_11_auth_2 + mfq_17_auth_3 +
                     mfq_23_auth_4 + mfq_29_auth_5 + mfq_35_auth_6
  Purity          =~ mfq_6_pur_1 + mfq_12_pur_2 + mfq_18_pur_3 +
                     mfq_24_pur_4 + mfq_30_pur_5 + mfq_36_pur_6
  Individualizing =~ Care + Equality
  Binding         =~ Proportionality + CountryLoyalty + GroupLoyalty +
                     Authority + Purity
"
second_order_syntax <- if (loyalty_split) {
  second_order_first_order_7f
} else {
  second_order_first_order_6f
}

fit_so_cal <- cfa(
  second_order_syntax,
  data      = d_calibration,
  estimator = "MLR",
  missing   = "fiml"
)
fi_so_cal <- report_fit(fit_so_cal, sprintf(
  "Second-order on %s first-order factors (calibration)",
  ifelse(loyalty_split, "7", "6")
))


# --- 5.2.6 Model comparison (AIC / BIC) ----------------------------------- #
model_comparison <- data.frame(
  Model = c("ESEM 6-factor", "CFA 6-factor", "ESEM 7-factor",
            "CFA 7-factor",  "Second-order"),
  npar = c(fi_esem_6f_cal["npar"], fi_cfa_6f_cal["npar"], fi_esem_7f_cal["npar"],
           fi_cfa_7f_cal["npar"],  fi_so_cal["npar"]),
  AIC = c(fi_esem_6f_cal["aic"], fi_cfa_6f_cal["aic"], fi_esem_7f_cal["aic"],
          fi_cfa_7f_cal["aic"],  fi_so_cal["aic"]),
  BIC = c(fi_esem_6f_cal["bic"], fi_cfa_6f_cal["bic"], fi_esem_7f_cal["bic"],
          fi_cfa_7f_cal["bic"],  fi_so_cal["bic"]),
  CFI = c(fi_esem_6f_cal["cfi"], fi_cfa_6f_cal["cfi"], fi_esem_7f_cal["cfi"],
          fi_cfa_7f_cal["cfi"],  fi_so_cal["cfi"]),
  RMSEA = c(fi_esem_6f_cal["rmsea"], fi_cfa_6f_cal["rmsea"],
            fi_esem_7f_cal["rmsea"], fi_cfa_7f_cal["rmsea"], fi_so_cal["rmsea"])
) %>%
  mutate(delta_BIC = BIC - min(BIC), delta_AIC = AIC - min(AIC))
message("Model comparison (calibration):")
print(model_comparison, row.names = FALSE)

aic_best_model <- model_comparison$Model[which.min(model_comparison$AIC)]
bic_best_model <- model_comparison$Model[which.min(model_comparison$BIC)]
if (aic_best_model != bic_best_model) {
  message(sprintf(
    "AIC (%s) and BIC (%s) diverge -> BIC prioritized per protocol.",
    aic_best_model, bic_best_model
  ))
}
indistinguishable <- model_comparison %>% filter(delta_BIC < 2)
selected_model <- indistinguishable$Model[which.min(indistinguishable$npar)]
runner_up_delta <- sort(model_comparison$delta_BIC)[2]
message(sprintf(
  "Selected model: %s (delta-BIC of runner-up = %.2f -> evidence: %s).",
  selected_model, runner_up_delta,
  ifelse(runner_up_delta > 10, "strong",
         ifelse(runner_up_delta >= 2, "moderate", "indistinguishable; parsimony applied"))
))


# --- 5.2.7 WLSMV sensitivity analysis ------------------------------------- #
# Sensitivity: CFA 6-factor with ordered-categorical items (WLSMV); not pre-registered.
fit_cfa_6f_wlsmv <- cfa(
  cfa_6f_syntax,
  data      = d_calibration,
  estimator = "WLSMV",
  ordered   = mfq_all_items
)
fi_cfa_6f_wlsmv <- report_fit(fit_cfa_6f_wlsmv, "CFA 6-factor WLSMV (calibration)")


# --- 5.2.8 Model re-specification check ----------------------------------- #
primary_fit <- fi_esem_6f_cal
if (primary_fit["cfi"] < 0.90 | primary_fit["tli"] < 0.90 |
    primary_fit["rmsea"] > 0.08) {
  message("Primary ESEM fit below acceptable thresholds.")
  if (primary_fit["cfi"] < 0.85 | primary_fit["tli"] < 0.85 |
      primary_fit["rmsea"] > 0.10) {
    message("Primary ESEM fit below critical thresholds; item removal applicable.")
  }
} else {
  message(sprintf(
    "Primary ESEM fit acceptable: CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f.",
    primary_fit["cfi"], primary_fit["tli"],
    primary_fit["rmsea"], primary_fit["srmr"]
  ))
}


# --- 5.3 Replication in validation subsample ------------------------------------ #
# Confirmatory CFA replication of the selected model.
validation_syntax <- switch(selected_model,
  "ESEM 6-factor" = ,
  "CFA 6-factor"  = cfa_6f_syntax,
  "ESEM 7-factor" = ,
  "CFA 7-factor"  = cfa_7f_syntax,
  "Second-order"  = second_order_syntax
)
validation_cal_fi <- switch(selected_model,
  "ESEM 6-factor" = fi_cfa_6f_cal,
  "CFA 6-factor"  = fi_cfa_6f_cal,
  "ESEM 7-factor" = fi_cfa_7f_cal,
  "CFA 7-factor"  = fi_cfa_7f_cal,
  "Second-order"  = fi_so_cal
)
fit_best_validation <- cfa(
  validation_syntax,
  data      = d_validation,
  estimator = "MLR",
  missing   = "fiml"
)
fi_best_validation <- report_fit(
  fit_best_validation, sprintf("Selected model [%s], CFA replication (validation)",
                            selected_model)
)

message("Latent factor correlations (validation CFA replication):")
print(round(lavInspect(fit_best_validation, "cor.lv"), 3))

resid_mat <- lavResiduals(fit_best_validation)$cov.z
resid_df  <- as.data.frame(as.table(resid_mat))
colnames(resid_df) <- c("Item1", "Item2", "StdResid")
resid_df <- resid_df %>%
  filter(Item1 != Item2, abs(StdResid) > 0.10) %>%
  arrange(desc(abs(StdResid)))
if (nrow(resid_df) > 0) {
  message("Standardized residual correlations > |.10| (validation CFA):")
  print(resid_df, row.names = FALSE)
} else {
  message("No standardized residuals exceed |.10|.")
}

message(sprintf(
  "%s (CFA): CFI_cal = %.3f, CFI_hol = %.3f; RMSEA_cal = %.3f, RMSEA_hol = %.3f; SRMR_cal = %.3f, SRMR_hol = %.3f.",
  selected_model,
  validation_cal_fi["cfi"],   fi_best_validation["cfi"],
  validation_cal_fi["rmsea"], fi_best_validation["rmsea"],
  validation_cal_fi["srmr"],  fi_best_validation["srmr"]
))

# ESEM 6-factor — supplementary validation-subsample replication of the primary model
set.seed(2026)  # reproducible geomin random starts
fit_esem_6f_validation <- sem(
  esem_6f_syntax,
  data      = d_validation,
  estimator = "MLR",
  missing   = "fiml",
  rotation  = "geomin",
  rotation.args = list(orthogonal = FALSE, geomin.epsilon = 0.01, rstarts = 100)
)
fi_esem_6f_validation <- report_fit(fit_esem_6f_validation, "ESEM 6-factor (validation)")


# --- 5.4 Internal consistency reliability (H1b) --------------------------- #
message("Internal consistency reliability (H1b).")

set.seed(2026)  # reproducible geomin random starts
fit_esem_6f_full <- sem(
  esem_6f_syntax,
  data      = d,
  estimator = "MLR",
  missing   = "fiml",
  rotation  = "geomin",
  rotation.args = list(orthogonal = FALSE, geomin.epsilon = 0.01, rstarts = 100)
)
report_fit(fit_esem_6f_full, "ESEM 6-factor (full)")

fit_cfa_6f_full <- cfa(
  cfa_6f_syntax,
  data      = d,
  estimator = "MLR",
  missing   = "fiml"
)
report_fit(fit_cfa_6f_full, "CFA 6-factor (full)")

fit_cfa_7f_full <- cfa(
  cfa_7f_syntax,
  data      = d,
  estimator = "MLR",
  missing   = "fiml"
)
report_fit(fit_cfa_7f_full, "CFA 7-factor (full)")

# McDonald's omega from standardized target loadings and model-implied
# standardized residual variances (theta):
#   omega = (sum lambda)^2 / [(sum lambda)^2 + sum theta].
# Theta-based computation is required for ESEM; for single-loading CFA it
# coincides with 1 - lambda^2.
omega_from_loadings <- function(fit, factor_to_items) {
  std      <- standardizedSolution(fit)
  loadings <- std %>% filter(op == "=~")
  thetas   <- std %>% filter(op == "~~", lhs == rhs)
  vapply(names(factor_to_items), function(fac) {
    items   <- factor_to_items[[fac]]
    lambdas <- loadings$est.std[loadings$lhs == fac & loadings$rhs %in% items]
    theta   <- thetas$est.std[thetas$lhs %in% items]
    if (length(lambdas) == 0 || length(theta) != length(items)) return(NA_real_)
    s <- sum(lambdas)
    s^2 / (s^2 + sum(theta))
  }, numeric(1))
}

factor_items_6f <- list(
  Care            = mfq_care_items,
  Equality        = mfq_equa_items,
  Proportionality = mfq_pro_items,
  Loyalty         = mfq_loya_items,
  Authority       = mfq_auth_items,
  Purity          = mfq_pur_items
)
factor_items_7f <- c(
  factor_items_6f[c("Care", "Equality", "Proportionality")],
  list(CountryLoyalty = mfq_country_loyalty_items,
       GroupLoyalty   = mfq_group_loyalty_items),
  factor_items_6f[c("Authority", "Purity")]
)

omega_esem_6f <- omega_from_loadings(fit_esem_6f_full, factor_items_6f)
message("Omega (ESEM 6-factor, primary):")
print(round(omega_esem_6f, 3))

omega_cfa_6f <- omega_from_loadings(fit_cfa_6f_full, factor_items_6f)
message("Omega (CFA 6-factor, comparison):")
print(round(omega_cfa_6f, 3))

omega_cfa_7f <- omega_from_loadings(fit_cfa_7f_full, factor_items_7f)
message("Omega (CFA 7-factor, incl. Loyalty split):")
print(round(omega_cfa_7f, 3))

message("H1b decision (omega_ESEM >= .70):")
for (i in seq_along(omega_esem_6f)) {
  sub_name  <- names(omega_esem_6f)[i]
  omega_val <- omega_esem_6f[i]
  decision  <- ifelse(omega_val >= 0.70, "SUPPORTED", "NOT SUPPORTED")
  message(sprintf("  %s: omega = %.3f -> %s.", sub_name, omega_val, decision))
}

mfq_subscale_list <- list(
  Care            = mfq_care_items,
  Equality        = mfq_equa_items,
  Proportionality = mfq_pro_items,
  Loyalty         = mfq_loya_items,
  Authority       = mfq_auth_items,
  Purity          = mfq_pur_items,
  CountryLoyalty  = mfq_country_loyalty_items,
  GroupLoyalty    = mfq_group_loyalty_items
)
message("Cronbach's alpha (reference):")
for (sub_name in names(mfq_subscale_list)) {
  alpha_val <- psych::alpha(d[, mfq_subscale_list[[sub_name]]], check.keys = FALSE)
  message(sprintf("  %s: alpha = %.3f.", sub_name, alpha_val$total$raw_alpha))
}

subscale_order <- c("Care", "Equality", "Proportionality",
                    "Loyalty", "Authority", "Purity",
                    "CountryLoyalty", "GroupLoyalty")
reliability_summary <- tibble::tibble(
  subscale      = subscale_order,
  omega_ESEM_6f = c(unname(omega_esem_6f)[match(subscale_order[1:6],
                                                names(omega_esem_6f))],
                    NA_real_, NA_real_),
  omega_CFA_6f  = c(unname(omega_cfa_6f)[match(subscale_order[1:6],
                                               names(omega_cfa_6f))],
                    NA_real_, NA_real_),
  omega_CFA_7f  = unname(omega_cfa_7f)[match(subscale_order, names(omega_cfa_7f))],
  alpha         = vapply(subscale_order,
                         function(s) psych::alpha(d[, mfq_subscale_list[[s]]],
                                                  check.keys = FALSE)$total$raw_alpha,
                         numeric(1))
)
reliability_summary$omega_CFA_7f[reliability_summary$subscale == "Loyalty"] <- NA_real_
message("Reliability summary table:")
print(as.data.frame(reliability_summary), row.names = FALSE, digits = 3)


# --- 5.5 Measurement Invariance ------------------------------------------- #
# Decision rule: supported if ΔCFI ≥ −.010 and ΔRMSEA ≤ +.015.

message("Measurement Invariance")

# --- 5.5a Gender Invariance --------------------------------------------------

message("Gender Invariance")

# Restricted to Male/Female (post-protocol specification)
d_gender_inv <- d %>% filter(gender %in% c("Male", "Female"))
message(sprintf(
  "Gender invariance subsample: n = %d (excluded: %d cases with gender other than Male/Female).",
  nrow(d_gender_inv), nrow(d) - nrow(d_gender_inv)
))
gender_n <- table(d_gender_inv$gender)
message("Gender group sizes:")
print(gender_n)

if (length(gender_n) == 2 && all(gender_n >= 200)) {
  fit_config_gender <- cfa(cfa_6f_syntax,
    data = d_gender_inv, group = "gender",
    estimator = "MLR", missing = "fiml"
  )
  fit_metric_gender <- cfa(cfa_6f_syntax,
    data = d_gender_inv, group = "gender",
    estimator = "MLR", missing = "fiml",
    group.equal = "loadings"
  )
  fit_scalar_gender <- cfa(cfa_6f_syntax,
    data = d_gender_inv, group = "gender",
    estimator = "MLR", missing = "fiml",
    group.equal = c("loadings", "intercepts")
  )

  fi_config <- fitMeasures(fit_config_gender, c("cfi.robust", "rmsea.robust"))
  fi_metric <- fitMeasures(fit_metric_gender, c("cfi.robust", "rmsea.robust"))
  fi_scalar <- fitMeasures(fit_scalar_gender, c("cfi.robust", "rmsea.robust"))
  names(fi_config) <- names(fi_metric) <- names(fi_scalar) <- c("cfi", "rmsea")

  message("Invariance Model Comparison (Gender):")
  message(sprintf("  Configural: CFI = %.3f, RMSEA = %.3f", fi_config["cfi"], fi_config["rmsea"]))
  message(sprintf("  Metric:     CFI = %.3f, RMSEA = %.3f", fi_metric["cfi"], fi_metric["rmsea"]))
  message(sprintf("  Scalar:     CFI = %.3f, RMSEA = %.3f", fi_scalar["cfi"], fi_scalar["rmsea"]))

  delta_cfi_metric <- fi_metric["cfi"] - fi_config["cfi"]
  delta_rmsea_metric <- fi_metric["rmsea"] - fi_config["rmsea"]
  delta_cfi_scalar <- fi_scalar["cfi"] - fi_metric["cfi"]
  delta_rmsea_scalar <- fi_scalar["rmsea"] - fi_metric["rmsea"]

  message(sprintf(
    "  Configural -> Metric: DCFI = %.3f, DRMSEA = %.3f -> %s",
    delta_cfi_metric, delta_rmsea_metric,
    ifelse(delta_cfi_metric >= -0.010 && delta_rmsea_metric <= 0.015,
      "INVARIANCE SUPPORTED", "INVARIANCE NOT SUPPORTED"
    )
  ))
  message(sprintf(
    "  Metric -> Scalar:     DCFI = %.3f, DRMSEA = %.3f -> %s",
    delta_cfi_scalar, delta_rmsea_scalar,
    ifelse(delta_cfi_scalar >= -0.010 && delta_rmsea_scalar <= 0.015,
      "INVARIANCE SUPPORTED", "INVARIANCE NOT SUPPORTED"
    )
  ))

  message("Supplementary chi-square difference tests:")
  print(lavTestLRT(fit_config_gender, fit_metric_gender, fit_scalar_gender))
} else {
  message("Gender invariance skipped: at least one group has n < 200.")
}

# --- 5.5b Age cohort invariance ------------------------------------------- #
# Cohorts split at age 35 (sensitivity analysis; the primary age grouping is
# the median split in 5.9.2); age_group is defined in 01_data_preparation.R (2.8).
d_age_inv <- d %>% filter(!is.na(age_group))
age_n <- table(d_age_inv$age_group)
message("Age group sizes:")
print(age_n)

if (length(age_n) == 2 && all(age_n >= 200)) {
  fit_config_age <- cfa(cfa_6f_syntax, data = d_age_inv, group = "age_group",
                        estimator = "MLR", missing = "fiml")
  fit_metric_age <- cfa(cfa_6f_syntax, data = d_age_inv, group = "age_group",
                        estimator = "MLR", missing = "fiml",
                        group.equal = "loadings")
  fit_scalar_age <- cfa(cfa_6f_syntax, data = d_age_inv, group = "age_group",
                        estimator = "MLR", missing = "fiml",
                        group.equal = c("loadings", "intercepts"))

  fi_config_a <- fitMeasures(fit_config_age, c("cfi.robust", "rmsea.robust"))
  fi_metric_a <- fitMeasures(fit_metric_age, c("cfi.robust", "rmsea.robust"))
  fi_scalar_a <- fitMeasures(fit_scalar_age, c("cfi.robust", "rmsea.robust"))
  names(fi_config_a) <- names(fi_metric_a) <- names(fi_scalar_a) <- c("cfi", "rmsea")

  message("Invariance (age):")
  message(sprintf("  Configural: CFI = %.3f, RMSEA = %.3f.", fi_config_a["cfi"], fi_config_a["rmsea"]))
  message(sprintf("  Metric:     CFI = %.3f, RMSEA = %.3f.", fi_metric_a["cfi"], fi_metric_a["rmsea"]))
  message(sprintf("  Scalar:     CFI = %.3f, RMSEA = %.3f.", fi_scalar_a["cfi"], fi_scalar_a["rmsea"]))

  dcfi_metric_a   <- fi_metric_a["cfi"] - fi_config_a["cfi"]
  drmsea_metric_a <- fi_metric_a["rmsea"] - fi_config_a["rmsea"]
  dcfi_scalar_a   <- fi_scalar_a["cfi"] - fi_metric_a["cfi"]
  drmsea_scalar_a <- fi_scalar_a["rmsea"] - fi_metric_a["rmsea"]

  message(sprintf(
    "  Configural -> Metric: dCFI = %.3f, dRMSEA = %.3f -> %s.",
    dcfi_metric_a, drmsea_metric_a,
    ifelse(dcfi_metric_a >= -0.010 && drmsea_metric_a <= 0.015,
           "SUPPORTED", "NOT SUPPORTED")
  ))
  message(sprintf(
    "  Metric -> Scalar:     dCFI = %.3f, dRMSEA = %.3f -> %s.",
    dcfi_scalar_a, drmsea_scalar_a,
    ifelse(dcfi_scalar_a >= -0.010 && drmsea_scalar_a <= 0.015,
           "SUPPORTED", "NOT SUPPORTED")
  ))

  message("Chi-square difference tests (age):")
  print(lavTestLRT(fit_config_age, fit_metric_age, fit_scalar_age))
} else {
  message("Age cohort invariance skipped: at least one group has n < 200.")
}


# --- 5.6 Nomological network (H2-H6) -------------------------------------- #
# SUPPORTED requires expected sign, p < .05, and |r| >= .15 (pre-registered).
message("Nomological network (H2-H6).")

report_cor <- function(x, y, x_name, y_name, hypothesis, expected_sign) {
  ct <- cor.test(x, y, method = "pearson", use = "pairwise.complete.obs")
  r  <- unname(ct$estimate)
  p  <- ct$p.value
  n  <- sum(complete.cases(x, y))
  sign_match <- (expected_sign == "positive" & r > 0) |
                (expected_sign == "negative" & r < 0)
  magnitude  <- abs(r) >= 0.15
  result <- if (sign_match && p < 0.05 && magnitude) {
    "SUPPORTED"
  } else if (sign_match && p < 0.05) {
    "NOT SUPPORTED (significant in expected direction, but |r| < .15)"
  } else {
    "NOT SUPPORTED"
  }
  message(sprintf(
    "  %s: r(%d) = %.3f, p = %.3f, expected %s -> %s.",
    hypothesis, n - 2, r, p, expected_sign, result
  ))
  return(ct)
}

message("H2a-d: political ideology x binding foundations.")
h2a <- report_cor(d$pol_1_lrscale, d$proportionality_mean,
                  "pol_1_lrscale", "proportionality_mean",
                  "H2a (Proportionality)", "positive")
h2b <- report_cor(d$pol_1_lrscale, d$loyalty_mean,
                  "pol_1_lrscale", "loyalty_mean",
                  "H2b (Loyalty)", "positive")
h2c <- report_cor(d$pol_1_lrscale, d$authority_mean,
                  "pol_1_lrscale", "authority_mean",
                  "H2c (Authority)", "positive")
h2d <- report_cor(d$pol_1_lrscale, d$purity_mean,
                  "pol_1_lrscale", "purity_mean",
                  "H2d (Purity)", "positive")

message("H3: regression ind_bind_diff ~ pol_1_lrscale.")
h3_model   <- lm(ind_bind_diff ~ pol_1_lrscale, data = d)
h3_summary <- summary(h3_model)
message(sprintf(
  "  b = %.3f, SE = %.3f, t = %.3f, p = %.3f.",
  coef(h3_summary)[2, 1], coef(h3_summary)[2, 2],
  coef(h3_summary)[2, 3], coef(h3_summary)[2, 4]
))
message(sprintf(
  "  R2 = %.3f, adj. R2 = %.3f.",
  h3_summary$r.squared, h3_summary$adj.r.squared
))
h3_coef <- coef(h3_model)["pol_1_lrscale"]
h3_p    <- coef(h3_summary)["pol_1_lrscale", "Pr(>|t|)"]
# SESOI applies to H3; in the simple regression, |r| = sqrt(R^2)
h3_r <- sign(h3_coef) * sqrt(h3_summary$r.squared)
h3_result <- if (h3_coef < 0 && h3_p < 0.05 && abs(h3_r) >= 0.15) {
  "SUPPORTED"
} else if (h3_coef < 0 && h3_p < 0.05) {
  "NOT SUPPORTED (significant in expected direction, but |r| < .15)"
} else {
  "NOT SUPPORTED"
}
message(sprintf("  H3: r = %.3f -> %s.", h3_r, h3_result))

message("H4: political ideology x Equality.")
h4 <- report_cor(d$pol_1_lrscale, d$equality_mean,
                 "pol_1_lrscale", "equality_mean",
                 "H4 (Equality)", "negative")

message("H5a-b: Agreeableness x individualizing foundations.")
h5a <- report_cor(d$agreeableness_mean, d$care_mean,
                  "agreeableness_mean", "care_mean",
                  "H5a (Care)", "positive")
h5b <- report_cor(d$agreeableness_mean, d$equality_mean,
                  "agreeableness_mean", "equality_mean",
                  "H5b (Equality)", "positive")

message("H6: Extraversion x Care.")
h6 <- report_cor(d$extraversion_mean, d$care_mean,
                 "extraversion_mean", "care_mean",
                 "H6 (Care)", "positive")


# --- 5.6.1 Minimum detectable effect (MDE) for null directional tests ----- #
message("MDE: minimum detectable |r| (alpha = .05, power = .80).")
n_full    <- sum(complete.cases(d$pol_1_lrscale, d$care_mean))
mde_full  <- pwr::pwr.r.test(n = n_full,    sig.level = 0.05, power = 0.80)$r
mde_h5b_n <- sum(complete.cases(d$agreeableness_mean, d$equality_mean))
mde_h5b   <- pwr::pwr.r.test(n = mde_h5b_n, sig.level = 0.05, power = 0.80)$r
mde_h6_n  <- sum(complete.cases(d$extraversion_mean,  d$care_mean))
mde_h6    <- pwr::pwr.r.test(n = mde_h6_n,  sig.level = 0.05, power = 0.80)$r
message(sprintf("  Full sample (n = %d): MDE |r| = %.3f.", n_full,    mde_full))
message(sprintf("  H5b (n = %d): MDE |r| = %.3f; observed |r| = %.3f.",
                mde_h5b_n, mde_h5b, abs(unname(h5b$estimate))))
message(sprintf("  H6  (n = %d): MDE |r| = %.3f; observed |r| = %.3f.",
                mde_h6_n,  mde_h6,  abs(unname(h6$estimate))))


# --- 5.7 Exploratory analyses (E1-E3) ------------------------------------- #
# BH-FDR within each pool (E1: 48 correlations, E2: 24 t-tests, E3: 36 betas);
# not pre-registered, raw and adjusted p-values are reported.

# --- 5.7.1 Bivariate correlations (E1) ------------------------------------ #
nom_vars <- c(
  mfq_subscale_vars, "pol_1_lrscale", "pol_2_social", "pol_3_economic",
  "religiosity", "extraversion_mean", "agreeableness_mean",
  "idealism_mean", "relativism_mean"
)
nom_cor <- cor(d[, nom_vars], use = "pairwise.complete.obs")
message("Nomological correlation matrix (MFQ-2 subscales x validation):")
print(round(nom_cor, 3))

ext_vars <- c(
  "pol_1_lrscale", "pol_2_social", "pol_3_economic", "religiosity",
  "extraversion_mean", "agreeableness_mean", "idealism_mean", "relativism_mean"
)
e1_results <- expand.grid(
  mfq_subscale   = mfq_subscale_vars,
  validation_var = ext_vars,
  stringsAsFactors = FALSE
)
e1_results$r     <- NA_real_
e1_results$p_raw <- NA_real_
e1_results$n     <- NA_integer_
for (i in seq_len(nrow(e1_results))) {
  x  <- d[[e1_results$mfq_subscale[i]]]
  y  <- d[[e1_results$validation_var[i]]]
  ct <- cor.test(x, y, method = "pearson", use = "pairwise.complete.obs")
  e1_results$r[i]     <- unname(ct$estimate)
  e1_results$p_raw[i] <- ct$p.value
  e1_results$n[i]     <- sum(complete.cases(x, y))
}
e1_results$p_BH <- p.adjust(e1_results$p_raw, method = "BH")

fmt_sig <- function(p) {
  ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
}
e1_print <- e1_results %>%
  arrange(mfq_subscale, validation_var) %>%
  mutate(raw_sig = fmt_sig(p_raw), BH_sig = fmt_sig(p_BH))
for (mfq_sub in mfq_subscale_vars) {
  message(sprintf("  %s:", mfq_sub))
  sub_df <- e1_print %>% filter(mfq_subscale == mfq_sub)
  for (i in seq_len(nrow(sub_df))) {
    message(sprintf(
      "    x %-22s r = %.3f (n = %d)  p_raw = %.3f%-3s  p_BH = %.3f%s.",
      sub_df$validation_var[i], sub_df$r[i], sub_df$n[i],
      sub_df$p_raw[i], sub_df$raw_sig[i],
      sub_df$p_BH[i],  sub_df$BH_sig[i]
    ))
  }
}
message(sprintf(
  "E1: %d tests; raw p < .05: %d; BH p < .05: %d.",
  nrow(e1_results),
  sum(e1_results$p_raw < .05),
  sum(e1_results$p_BH  < .05)
))


# --- 5.7.2 Group comparisons (E2) ----------------------------------------- #
collect_group_comparison <- function(data, group_var, group_labels, comparison_name) {
  d_sub <- data %>% filter(.data[[group_var]] %in% group_labels)
  n1 <- sum(d_sub[[group_var]] == group_labels[1])
  n2 <- sum(d_sub[[group_var]] == group_labels[2])
  res <- lapply(mfq_subscale_vars, function(sub) {
    formula <- as.formula(paste(sub, "~", group_var))
    tt <- t.test(formula, data = d_sub, var.equal = FALSE)
    cd <- effectsize::cohens_d(formula, data = d_sub)
    data.frame(
      comparison = comparison_name,
      group_var  = group_var,
      subscale   = sub,
      n1 = n1, n2 = n2,
      t  = unname(tt$statistic),
      df = unname(tt$parameter),
      p_raw    = tt$p.value,
      cohens_d = cd$Cohens_d,
      d_low    = cd$CI_low,
      d_high   = cd$CI_high,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res)
}

# Primary E2 pool (24 tests): 4 contrasts x 6 subscales
e2_primary <- rbind(
  collect_group_comparison(d, "religiosity_group",
                           c("Non-religious", "Religious"),
                           "Religiosity (median split)"),
  collect_group_comparison(d, "gender",
                           c("Male", "Female"), "Gender"),
  collect_group_comparison(d, "east_west",
                           c("East", "West"), "East vs. West"),
  collect_group_comparison(d, "pol_group",
                           c("Liberal", "Conservative"),
                           "Liberal vs. Conservative")
)
e2_primary$p_BH <- p.adjust(e2_primary$p_raw, method = "BH")

for (cmp in unique(e2_primary$comparison)) {
  block <- e2_primary %>% filter(comparison == cmp)
  message(sprintf("%s (n1 = %d, n2 = %d):", cmp, unique(block$n1), unique(block$n2)))
  for (i in seq_len(nrow(block))) {
    message(sprintf(
      "   %-22s t(%.1f) = %6.3f  p_raw = %.3f%-3s  p_BH = %.3f%-3s  d = %.3f [%.3f, %.3f].",
      block$subscale[i], block$df[i], block$t[i],
      block$p_raw[i], fmt_sig(block$p_raw[i]),
      block$p_BH[i],  fmt_sig(block$p_BH[i]),
      block$cohens_d[i], block$d_low[i], block$d_high[i]
    ))
  }
}
message(sprintf(
  "E2 primary: %d tests; raw p < .05: %d; BH p < .05: %d.",
  nrow(e2_primary),
  sum(e2_primary$p_raw < .05),
  sum(e2_primary$p_BH  < .05)
))

# Sensitivity: religiosity split at scale midpoint = 50 (separate pool)
e2_sensitivity_relig <- collect_group_comparison(
  d, "religiosity_group_mid", c("Non-religious", "Religious"),
  "Religiosity (sensitivity, midpoint = 50)"
)
e2_sensitivity_relig$p_BH_local <- p.adjust(e2_sensitivity_relig$p_raw, method = "BH")
message("Religiosity sensitivity (midpoint = 50):")
for (i in seq_len(nrow(e2_sensitivity_relig))) {
  b <- e2_sensitivity_relig[i, ]
  message(sprintf(
    "   %-22s t(%.1f) = %6.3f  p_raw = %.3f%-3s  p_BH_local = %.3f%-3s  d = %.3f [%.3f, %.3f].",
    b$subscale, b$df, b$t,
    b$p_raw, fmt_sig(b$p_raw),
    b$p_BH_local, fmt_sig(b$p_BH_local),
    b$cohens_d, b$d_low, b$d_high
  ))
}


# --- 5.7.3 Multiple regression (E3) --------------------------------------- #
# IVs per protocol; gender_dummy (Female = 1) is defined in 01_data_preparation.R (2.8).
predictor_vars <- c(
  "pol_1_lrscale", "religiosity", "agreeableness_mean",
  "extraversion_mean", "age", "gender_dummy"
)
predictor_continuous <- setdiff(predictor_vars, "gender_dummy")
predictor_formula <- paste(predictor_vars, collapse = " + ")

e3_results <- list()
e3_omnibus <- list()
for (sub in mfq_subscale_vars) {
  formula     <- as.formula(paste(sub, "~", predictor_formula))
  reg_model   <- lm(formula, data = d)
  reg_summary <- summary(reg_model)
  e3_omnibus[[sub]] <- data.frame(
    DV     = sub,
    R2     = reg_summary$r.squared,
    R2_adj = reg_summary$adj.r.squared,
    F_stat = unname(reg_summary$fstatistic[1]),
    df1    = unname(reg_summary$fstatistic[2]),
    df2    = unname(reg_summary$fstatistic[3]),
    F_p    = pf(reg_summary$fstatistic[1], reg_summary$fstatistic[2],
                reg_summary$fstatistic[3], lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
  # DV and continuous predictors z-scored; binary gender dummy unstandardized.
  d_std <- d[, c(sub, predictor_vars)]
  d_std[, c(sub, predictor_continuous)] <-
    scale(d_std[, c(sub, predictor_continuous)])
  reg_std <- lm(formula, data = d_std)
  coefs   <- coef(summary(reg_std))[-1, , drop = FALSE]
  e3_results[[sub]] <- data.frame(
    DV        = sub,
    predictor = rownames(coefs),
    beta      = coefs[, 1],
    SE        = coefs[, 2],
    t         = coefs[, 3],
    p_raw     = coefs[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
e3_results <- do.call(rbind, e3_results)
e3_omnibus <- do.call(rbind, e3_omnibus)
e3_results$p_BH <- p.adjust(e3_results$p_raw, method = "BH")

for (sub in mfq_subscale_vars) {
  om <- e3_omnibus[e3_omnibus$DV == sub, ]
  message(sprintf("  DV: %s.", sub))
  message(sprintf(
    "    R2 = %.3f, adj. R2 = %.3f, F(%d, %d) = %.3f, p = %.3f.",
    om$R2, om$R2_adj, om$df1, om$df2, om$F_stat, om$F_p
  ))
  block <- e3_results[e3_results$DV == sub, ]
  for (i in seq_len(nrow(block))) {
    message(sprintf(
      "      %-22s b = %6.3f  SE = %.3f  t = %6.3f  p_raw = %.3f%-3s  p_BH = %.3f%s.",
      block$predictor[i], block$beta[i], block$SE[i], block$t[i],
      block$p_raw[i], fmt_sig(block$p_raw[i]),
      block$p_BH[i],  fmt_sig(block$p_BH[i])
    ))
  }
}
message(sprintf(
  "E3: %d coefficients; raw p < .05: %d; BH p < .05: %d.",
  nrow(e3_results),
  sum(e3_results$p_raw < .05),
  sum(e3_results$p_BH  < .05)
))


# --- 5.8 Hypothesis summary ----------------------------------------------- #
message("Hypothesis summary.")

message(sprintf(
  "Model selection: %s (Loyalty structure: %s).",
  selected_model,
  ifelse(loyalty_split, "7-factor split", "6-factor")
))

message("H1a (calibration fit):")
message(sprintf(
  "  ESEM 6-factor: CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f.",
  fi_esem_6f_cal["cfi"], fi_esem_6f_cal["tli"],
  fi_esem_6f_cal["rmsea"], fi_esem_6f_cal["srmr"]
))
message(sprintf(
  "  CFA  6-factor: CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f.",
  fi_cfa_6f_cal["cfi"], fi_cfa_6f_cal["tli"],
  fi_cfa_6f_cal["rmsea"], fi_cfa_6f_cal["srmr"]
))

message("H1b (omega_ESEM):")
for (i in seq_along(omega_esem_6f)) {
  message(sprintf("  %s: omega = %.3f.",
                  names(omega_esem_6f)[i], omega_esem_6f[i]))
}
message("Loyalty subfactors (omega from CFA 7-factor):")
for (sub in c("CountryLoyalty", "GroupLoyalty")) {
  message(sprintf("  %s: omega = %.3f.", sub, omega_cfa_7f[sub]))
}

message("H2a-d (Pearson r, expected positive):")
message(sprintf("  H2a (Proportionality): r = %.3f, p = %.3f.", h2a$estimate, h2a$p.value))
message(sprintf("  H2b (Loyalty):         r = %.3f, p = %.3f.", h2b$estimate, h2b$p.value))
message(sprintf("  H2c (Authority):       r = %.3f, p = %.3f.", h2c$estimate, h2c$p.value))
message(sprintf("  H2d (Purity):          r = %.3f, p = %.3f.", h2d$estimate, h2d$p.value))

message(sprintf("H3 (regression): b = %.3f, p = %.3f -> %s.", h3_coef, h3_p, h3_result))
message(sprintf("H4 (Equality):  r = %.3f, p = %.3f.", h4$estimate,  h4$p.value))
message(sprintf("H5a (Care):     r = %.3f, p = %.3f.", h5a$estimate, h5a$p.value))
message(sprintf("H5b (Equality): r = %.3f, p = %.3f.", h5b$estimate, h5b$p.value))
message(sprintf("H6  (Care):     r = %.3f, p = %.3f.", h6$estimate,  h6$p.value))

message("Exclusion log:")
print(exclusion_log)


# --- 5.9 Additional analyses (not pre-registered) ------------------------ #
# Correlation CIs, age invariance at the median, full loading matrices, and
# invariance per subscale.

# --- 5.9.1 Correlations with 95% CIs -------------------------------------- #
# Fisher-z CIs from cor.test(); subscale intercorrelations without correction
# (descriptive), nomological pool with the BH adjustment from E1.
cor_ci_row <- function(x, y, x_name, y_name, family) {
  ct <- cor.test(x, y, method = "pearson")
  data.frame(
    family = family, var1 = x_name, var2 = y_name,
    r = unname(ct$estimate), ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2],
    n = sum(complete.cases(x, y)), df = unname(ct$parameter),
    p_raw = ct$p.value, stringsAsFactors = FALSE
  )
}

subscale_pairs <- combn(mfq_subscale_vars, 2, simplify = FALSE)
cor_ci_inter <- do.call(rbind, lapply(subscale_pairs, function(pr) {
  cor_ci_row(d[[pr[1]]], d[[pr[2]]], pr[1], pr[2], "intercorrelation")
}))
cor_ci_inter$p_BH <- NA_real_

cor_ci_nom <- do.call(rbind, lapply(seq_len(nrow(e1_results)), function(i) {
  cor_ci_row(d[[e1_results$mfq_subscale[i]]], d[[e1_results$validation_var[i]]],
             e1_results$mfq_subscale[i], e1_results$validation_var[i], "nomological")
}))
cor_ci_nom$p_BH <- e1_results$p_BH
stopifnot(isTRUE(all.equal(cor_ci_nom$r, e1_results$r)))

cor_ci_all <- rbind(cor_ci_inter, cor_ci_nom)
message(sprintf("Correlations with 95%% CIs: %d intercorrelations, %d nomological.",
                nrow(cor_ci_inter), nrow(cor_ci_nom)))
print(cor_ci_all %>% mutate(across(c(r, ci_lo, ci_hi), ~ round(.x, 3))), row.names = FALSE)

h3_ci <- confint(h3_model)["pol_1_lrscale", ]
message(sprintf("  H3: b = %.3f, 95%% CI [%.3f, %.3f].",
                unname(h3_coef), h3_ci[1], h3_ci[2]))

# --- 5.9.2 Age invariance at the median ----------------------------------- #
# Primary age grouping: median split (<= median vs. > median). The cut-off at
# 35 (age_group, 5.5b) is reported as sensitivity analysis.
run_invariance <- function(syntax, data, group, analysis) {
  fits <- list(
    configural = cfa(syntax, data = data, group = group,
                     estimator = "MLR", missing = "fiml"),
    metric     = cfa(syntax, data = data, group = group,
                     estimator = "MLR", missing = "fiml",
                     group.equal = "loadings"),
    scalar     = cfa(syntax, data = data, group = group,
                     estimator = "MLR", missing = "fiml",
                     group.equal = c("loadings", "intercepts"))
  )
  fm <- t(sapply(fits, function(f) fitMeasures(
    f, c("chisq.scaled", "df.scaled", "cfi.robust", "rmsea.robust"))))
  n_tab <- table(data[[group]])
  out <- data.frame(
    analysis = analysis, grouping = group, model = names(fits),
    group_sizes = paste(sprintf("%s = %d", names(n_tab), as.integer(n_tab)),
                        collapse = "; "),
    chisq = fm[, "chisq.scaled"], df = fm[, "df.scaled"],
    cfi = fm[, "cfi.robust"], rmsea = fm[, "rmsea.robust"],
    converged = sapply(fits, lavInspect, "converged"),
    row.names = NULL, stringsAsFactors = FALSE
  )
  out$delta_cfi   <- c(NA, diff(out$cfi))
  out$delta_rmsea <- c(NA, diff(out$rmsea))
  out$supported   <- ifelse(is.na(out$delta_cfi), NA,
                            out$delta_cfi >= -0.010 & out$delta_rmsea <= 0.015)
  out
}

age_median <- median(d$age, na.rm = TRUE)
d_age_med  <- d %>%
  filter(!is.na(age)) %>%
  mutate(age_group_median = ifelse(age <= age_median, "Younger", "Older"))
message(sprintf("Age median = %.1f years.", age_median))
print(table(d_age_med$age_group_median))

inv_age <- rbind(
  run_invariance(cfa_6f_syntax, d_age_med, "age_group_median", "CFA 6-factor, age median split"),
  run_invariance(cfa_6f_syntax, d_age_inv, "age_group", "CFA 6-factor, age cut-off 35 (sensitivity)")
)
message("Age invariance (median split and cut-off 35):")
print(inv_age %>% mutate(across(c(cfi, rmsea, delta_cfi, delta_rmsea), ~ round(.x, 3))),
      row.names = FALSE)

# --- 5.9.3 Full standardized loading matrices ----------------------------- #
# CFA 6-factor in the validation subsample is the only newly estimated model.
fit_cfa_6f_val <- cfa(cfa_6f_syntax, data = d_validation,
                      estimator = "MLR", missing = "fiml")
fit_cfa_7f_val <- if (selected_model == "CFA 7-factor") fit_best_validation else
  cfa(cfa_7f_syntax, data = d_validation, estimator = "MLR", missing = "fiml")

loading_long <- function(fit, model, sample_label) {
  standardizedSolution(fit) %>%
    filter(op == "=~") %>%
    transmute(model = model, sample = sample_label, item = rhs, factor = lhs,
              lambda = est.std, se = se, p = pvalue)
}
loadings_full <- rbind(
  loading_long(fit_esem_6f_cal,        "ESEM 6-factor", "calibration"),
  loading_long(fit_esem_6f_validation, "ESEM 6-factor", "validation"),
  loading_long(fit_cfa_6f_cal,         "CFA 6-factor",  "calibration"),
  loading_long(fit_cfa_6f_val,         "CFA 6-factor",  "validation"),
  loading_long(fit_cfa_7f_cal,         "CFA 7-factor",  "calibration"),
  loading_long(fit_cfa_7f_val,         "CFA 7-factor",  "validation")
)
message(sprintf("Loading matrices: %d loadings across %d model x sample blocks.",
                nrow(loadings_full), nrow(distinct(loadings_full, model, sample))))

# --- 5.9.4 Measurement invariance per subscale ---------------------------- #
# One-factor CFA per subscale; configural model adequate if CFI > .90 and
# RMSEA < .08. Loyalty is additionally modelled with two factors
# (Country/Group) only if its one-factor configural model is not adequate.
subscale_item_sets <- list(
  Care = mfq_care_items, Equality = mfq_equa_items,
  Proportionality = mfq_pro_items, Loyalty = mfq_loya_items,
  Authority = mfq_auth_items, Purity = mfq_pur_items
)
one_factor_syntax <- function(name, items) {
  sprintf("%s =~ %s", name, paste(items, collapse = " + "))
}
loyalty_2f_syntax <- sprintf(
  "CountryLoyalty =~ %s\nGroupLoyalty =~ %s",
  paste(mfq_country_loyalty_items, collapse = " + "),
  paste(mfq_group_loyalty_items, collapse = " + ")
)
inv_groupings <- list(
  list(data = d_gender_inv, group = "gender"),
  list(data = d_age_med,    group = "age_group_median")
)

inv_sub_list <- list()
for (grp in inv_groupings) {
  for (sub_name in names(subscale_item_sets)) {
    inv_1f <- run_invariance(one_factor_syntax(sub_name, subscale_item_sets[[sub_name]]),
                             grp$data, grp$group, paste0(sub_name, " (one-factor)"))
    inv_1f$subscale <- sub_name
    inv_1f$configural_adequate <- inv_1f$cfi[1] > 0.90 & inv_1f$rmsea[1] < 0.08
    inv_sub_list[[length(inv_sub_list) + 1]] <- inv_1f
    if (sub_name == "Loyalty" && !inv_1f$configural_adequate[1]) {
      inv_2f <- run_invariance(loyalty_2f_syntax, grp$data, grp$group,
                               "Loyalty (two-factor: Country/Group)")
      inv_2f$subscale <- "Loyalty"
      inv_2f$configural_adequate <- inv_2f$cfi[1] > 0.90 & inv_2f$rmsea[1] < 0.08
      inv_sub_list[[length(inv_sub_list) + 1]] <- inv_2f
    }
  }
}
inv_subscales <- do.call(rbind, inv_sub_list)
message("Measurement invariance per subscale (gender, age median split):")
print(inv_subscales %>%
        select(grouping, analysis, model, cfi, rmsea, delta_cfi, delta_rmsea,
               supported, configural_adequate) %>%
        mutate(across(c(cfi, rmsea, delta_cfi, delta_rmsea), ~ round(.x, 3))),
      row.names = FALSE)


#### --- Phase 6: Figures ------------------------------------------------- ####

# --- 6.1 Theme and output directory --------------------------------------- #
fig_dir     <- file.path(data_dir, "figures")
fig_prev_dir <- file.path(fig_dir, "preview")
for (p in c(fig_dir, fig_prev_dir)) if (!dir.exists(p)) dir.create(p, recursive = TRUE)

PLOS_MAX_W_CM <- 19.05   # 2250 px at 300 dpi
PLOS_MAX_H_CM <- 22.23   # 2625 px at 300 dpi
PLOS_MIN_W_CM <- 6.68    #  789 px at 300 dpi

theme_apa <- function(base_size = 10, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text         = element_text(family = base_family, size = base_size),
      axis.text    = element_text(size = base_size, colour = "black"),
      axis.title   = element_text(size = base_size),
      axis.title.y = element_text(hjust = 0.5),
      legend.text  = element_text(size = base_size),
      legend.title = element_text(size = base_size),
      legend.position = "bottom",
      plot.title   = element_blank(),
      panel.border = element_blank()
    )
}
apa_palette  <- c("#004b8d", "#00afe4", "#a3d8f7")

# name = PLOS file name without extension (e.g. "Fig1", "S1_Fig")
apa_save <- function(plot, name, width = 16, height = 10) {
  stopifnot(width >= PLOS_MIN_W_CM, width <= PLOS_MAX_W_CM, height <= PLOS_MAX_H_CM)
  ggsave(file.path(fig_dir, paste0(name, ".tif")), plot = plot,
         width = width, height = height, units = "cm", dpi = 300,
         device = "tiff", compression = "lzw", bg = "white")
  ggsave(file.path(fig_prev_dir, paste0(name, ".png")), plot = plot,
         width = width, height = height, units = "cm", dpi = 300, bg = "white")
}

subscale_labels <- c(
  care_mean            = "Care",
  equality_mean        = "Equality",
  proportionality_mean = "Proportionality",
  loyalty_mean         = "Loyalty",
  authority_mean       = "Authority",
  purity_mean          = "Purity"
)

# --- 6.2 Participant flow diagram (CONSORT) ---------------------------- #
# Central column = sample retained per stage; right-hand boxes = exclusions.
flow_entering <- exclusion_log$n_remaining[1] + exclusion_log$n_excluded[1]
flow_excl     <- exclusion_log[exclusion_log$n_excluded > 0, , drop = FALSE]

# Display labels: the log stores the screening rule, the figure names the reason.
flow_labels <- c(
  "Completed surveys (dispcode == 31)"             = "Incomplete responses",
  "AI detection task (LUMIR)"                      = "Failed AI agent detection task (LUMIR)",
  "Attention checks (>= 1 failed)"                 = "Failed one or more attention checks",
  "Invariant responding (SD < .50)"                = "Invariant responding",
  "Self-reported AI agent use"                     = "Self-reported AI agent use",
  "No matching Prolific demographic record (merge)" = "No matching Prolific demographic record"
)
flow_excl$label <- ifelse(flow_excl$step %in% names(flow_labels),
                          flow_labels[flow_excl$step], flow_excl$step)
node_n        <- c(flow_entering, flow_excl$n_remaining)
n_node        <- length(node_n)
# APA-7: italic N/n via plotmath, thousands separators.
fmt_n <- function(n) formatC(n, big.mark = ",", format = "d")
node_df <- data.frame(
  y     = rev(seq_len(n_node)),
  label = c(
    sprintf("atop(\"Records assessed\", \"(Unipark + Prolific merge): \" * italic(N) == \"%s\")",
            fmt_n(flow_entering)),
    vapply(seq_len(nrow(flow_excl)), function(i) {
      if (i == nrow(flow_excl)) {
        sprintf("\"Final analytic sample: \" * italic(N) == \"%s\"", fmt_n(flow_excl$n_remaining[i]))
      } else {
        sprintf("\"Retained: \" * italic(n) == \"%s\"", fmt_n(flow_excl$n_remaining[i]))
      }
    }, character(1))
  ),
  stringsAsFactors = FALSE
)
excl_df <- data.frame(
  y     = rev(seq_len(n_node))[-1] + 0.5,
  label = sprintf("atop(\"Excluded \" * italic(n) == \"%s\" * \":\", \"%s\")",
                  fmt_n(flow_excl$n_excluded), flow_excl$label),
  stringsAsFactors = FALSE
)
fig_consort <- ggplot() +
  geom_segment(data = node_df[-1, , drop = FALSE],
               aes(x = 0, xend = 0, y = y + 1 - 0.2, yend = y + 0.2),
               arrow = grid::arrow(length = grid::unit(0.15, "cm")), colour = "grey45") +
  geom_segment(data = excl_df,
               aes(x = 0, xend = 0.55, y = y, yend = y),
               arrow = grid::arrow(length = grid::unit(0.13, "cm")), colour = "grey45") +
  geom_label(data = node_df, aes(x = 0, y = y, label = label), parse = TRUE,
             fill = apa_palette[3], colour = "black", size = 3,
             label.size = 0.3, lineheight = 0.95) +
  geom_label(data = excl_df, aes(x = 0.6, y = y, label = label), parse = TRUE,
             fill = "grey95", colour = "black", size = 2.6, hjust = 0,
             label.size = 0.3, lineheight = 0.95) +
  scale_x_continuous(limits = c(-1, 2.4)) +
  scale_y_continuous(expand = expansion(add = 0.7)) +
  theme_void()
apa_save(fig_consort, "Fig1", width = 17, height = 16)

# --- 6.3 MFQ-2 subscale distributions ------------------------------------- #
fig_dist_df <- d %>%
  select(all_of(mfq_subscale_vars)) %>%
  pivot_longer(everything(), names_to = "subscale", values_to = "score") %>%
  mutate(subscale = factor(subscale,
                           levels = mfq_subscale_vars,
                           labels = subscale_labels[mfq_subscale_vars]))
fig_distributions <- ggplot(fig_dist_df, aes(x = score)) +
  geom_histogram(bins = 20, fill = apa_palette[1],
                 colour = "white", alpha = 0.85) +
  facet_wrap(~ subscale, ncol = 3, scales = "free_y") +
  labs(x = "Subscale score", y = "Frequency") +
  theme_apa()
apa_save(fig_distributions, "S1_Fig",
         width = 18, height = 12)

# --- 6.4 Q-Q plots -------------------------------------------------------- #
fig_qq_df <- d %>%
  select(all_of(mfq_subscale_vars)) %>%
  pivot_longer(everything(), names_to = "subscale", values_to = "score") %>%
  mutate(subscale = factor(subscale,
                           levels = mfq_subscale_vars,
                           labels = subscale_labels[mfq_subscale_vars]))
fig_qq <- ggplot(fig_qq_df, aes(sample = score)) +
  stat_qq(colour = apa_palette[1], size = 0.6, alpha = 0.6) +
  stat_qq_line(colour = apa_palette[2], linewidth = 0.6) +
  facet_wrap(~ subscale, ncol = 3) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_apa()
apa_save(fig_qq, "S2_Fig", width = 18, height = 12)

# --- 6.5 Intercorrelation heatmap (MFQ-2 x validation) -------------------- #
fig_cor_df <- as.data.frame(as.table(round(nom_cor, 2))) %>%
  rename(Var1 = Var1, Var2 = Var2, r = Freq)
fig_heatmap <- ggplot(fig_cor_df, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = apa_palette[1], mid = "white",
                       high = apa_palette[2], midpoint = 0,
                       limits = c(-1, 1), name = expression(italic(r))) +
  labs(x = NULL, y = NULL) +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank())
apa_save(fig_heatmap, "S3_Fig", width = 19, height = 15.5)

# --- 6.6 ESEM standardized factor loadings -------------------------------- #
fig_loadings_df <- esem_6f_loadings_lv %>%
  mutate(
    Factor   = factor(Factor,
                      levels = c("Care", "Equality", "Proportionality",
                                 "Loyalty", "Authority", "Purity")),
    is_target = vapply(seq_along(Item), function(i) {
      target_factor <- sub("^mfq_\\d+_([a-z]+)_.*$", "\\1", Item[i])
      lookup <- c(care = "Care", equa = "Equality", pro = "Proportionality",
                  loya = "Loyalty", auth = "Authority", pur = "Purity")
      as.character(Factor[i]) == lookup[target_factor]
    }, logical(1))
  )
# Items ordered by target foundation, with rules separating the six blocks.
mfq_target_of <- function(item) {
  key <- sub("^mfq_\\d+_([a-z]+)_.*$", "\\1", item)
  unname(c(care = "Care", equa = "Equality", pro = "Proportionality",
           loya = "Loyalty", auth = "Authority", pur = "Purity")[key])
}
fig_loadings_df <- fig_loadings_df %>%
  mutate(
    Target = factor(mfq_target_of(Item), levels = levels(Factor)),
    ItemNo = as.integer(sub("^mfq_(\\d+)_.*$", "\\1", Item))
  )
# Reversed: ggplot draws the first level at the bottom.
item_order <- fig_loadings_df %>%
  distinct(Item, Target, ItemNo) %>%
  arrange(desc(as.integer(Target)), desc(ItemNo)) %>%
  pull(Item)
fig_loadings_df$Item <- factor(fig_loadings_df$Item, levels = item_order)
# Block boundaries; the last coincides with the panel edge and is dropped.
block_tops <- fig_loadings_df %>%
  distinct(Item, Target) %>%
  mutate(pos = as.integer(Item)) %>%
  group_by(Target) %>%
  summarise(top = max(pos), .groups = "drop") %>%
  pull(top) %>%
  sort()
block_breaks <- head(block_tops, -1) + 0.5

fig_loadings <- ggplot(fig_loadings_df,
                       aes(x = Lambda, y = Item, fill = is_target)) +
  geom_hline(yintercept = block_breaks, colour = "grey75", linewidth = 0.25) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = c(-0.30, 0.30), linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  facet_wrap(~ Factor, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = apa_palette[1], `FALSE` = apa_palette[3]),
                    labels = c(`TRUE` = "Target", `FALSE` = "Cross"),
                    name = NULL) +
  labs(x = expression(Standardized ~ loading ~ italic(lambda)), y = NULL) +
  theme_apa() +
  theme(axis.text.y = element_text(size = 5.5),
        panel.spacing.x = grid::unit(0.6, "lines"))
apa_save(fig_loadings, "Fig3", width = 19, height = 21)

# --- 6.7 SEM path diagram of the primary measurement model (CFA 7-factor) -- #
# semPlot draws on a base-graphics device: TIFF and PNG written separately.
sem_diagram_path <- file.path(fig_dir, "Fig2.tif")
draw_sem_diagram <- function(LAT_CEX = 0.85, MAN_CEX = 0.45) {
  sem_vars    <- semPlot::semPlotModel(fit_cfa_7f_full)@Vars
  node_labels <- sem_vars$name
  node_labels[node_labels == "GroupLoyalty"]   <- "Group Loyalty"
  node_labels[node_labels == "CountryLoyalty"] <- "Country Loyalty"
  node_cex <- ifelse(sem_vars$manifest, MAN_CEX, LAT_CEX)
  qg <- semPlot::semPaths(
    object         = fit_cfa_7f_full,
    what           = "std",      # edge width/shading scaled to standardized estimates
    whatLabels     = "std",      # print standardized loadings
    layout         = "tree2",
    rotation       = 2,          # latent factors on the left, indicators on the right
    style          = "lisrel",
    intercepts     = FALSE,
    residuals      = FALSE,
    thresholds     = FALSE,
    exoCov         = FALSE,      # omit factor covariance paths
    nCharNodes     = 0,          # keep full item names
    nodeLabels     = node_labels,
    fixedStyle     = 1,          # marker indicators solid, like all other paths
    # Sized for the longest labels ("mfq_13_care_3", "Proportionality").
    sizeMan        = 6.5,
    sizeMan2       = 2.2,
    sizeLat        = 13,
    sizeLat2       = 5.4,
    edge.label.cex = 0.55,
    label.scale    = FALSE,      # absolute label sizes instead of fit-to-node
    label.cex      = node_cex,   # one size for all factors, one for all items
    edge.color     = "grey30",
    color          = list(lat = apa_palette[3], man = "grey92"),
    border.color   = "grey40",
    curvePivot     = TRUE,
    mar            = c(3, 6, 3, 6),
    DoNotPlot      = TRUE
  )
  qg$layout[, 2] <- -qg$layout[, 2]
  plot(qg)
}
grDevices::tiff(sem_diagram_path, width = 19, height = 22, units = "cm",
                res = 300, compression = "lzw")
draw_sem_diagram()
grDevices::dev.off()

# PNG preview for the manuscript file.
grDevices::png(file.path(fig_prev_dir, "Fig2.png"), width = 19, height = 22,
               units = "cm", res = 300)
draw_sem_diagram()
grDevices::dev.off()
message(sprintf("SEM path diagram written to %s.", sem_diagram_path))

# --- 6.8 Group comparison profile (Liberal vs. Conservative) -------------- #
fig_group_df <- d %>%
  filter(pol_group %in% c("Liberal", "Conservative")) %>%
  pivot_longer(all_of(mfq_subscale_vars),
               names_to = "subscale", values_to = "score") %>%
  mutate(subscale = factor(subscale,
                           levels = mfq_subscale_vars,
                           labels = subscale_labels[mfq_subscale_vars]),
         pol_group = factor(pol_group, levels = c("Liberal", "Conservative")))
fig_group_summary <- fig_group_df %>%
  group_by(subscale, pol_group) %>%
  summarise(M = mean(score, na.rm = TRUE),
            SE = sd(score, na.rm = TRUE) / sqrt(sum(!is.na(score))),
            .groups = "drop")
fig_group <- ggplot(fig_group_summary,
                    aes(x = subscale, y = M, colour = pol_group,
                        shape = pol_group, group = pol_group)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = 0.15, linewidth = 0.4) +
  scale_colour_manual(values = c(Liberal = apa_palette[1],
                                 Conservative = apa_palette[2]),
                      name = NULL) +
  scale_shape_manual(values = c(Liberal = 16, Conservative = 17), name = NULL) +
  labs(x = NULL, y = "Subscale mean") +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
apa_save(fig_group, "Fig5",
         width = 18, height = 11)

# --- 6.9 H3 scatter: ind_bind_diff ~ pol_1_lrscale ------------------------ #
# Horizontal offset drawn once from a pinned seed and plotted with geom_point(),
# so the figure is reproducible; geom_smooth() uses the unjittered predictor.
set.seed(2026)
fig_h3_df <- d %>%
  mutate(lrscale_jittered = pol_1_lrscale + runif(n(), -0.15, 0.15))
fig_h3 <- ggplot(fig_h3_df, aes(x = pol_1_lrscale, y = ind_bind_diff)) +
  geom_point(aes(x = lrscale_jittered),
             alpha = 0.35, colour = apa_palette[1], size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = apa_palette[2], fill = apa_palette[3]) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(x = "Political ideology (1 = left, 10 = right)",
       y = "Individualizing minus binding difference score") +
  theme_apa()
apa_save(fig_h3, "Fig4", width = 14, height = 10)

# --- 6.10 Reliability (omega per subscale, with .70 threshold) ------------- #
fig_rel_df <- reliability_summary %>%
  pivot_longer(c(omega_ESEM_6f, omega_CFA_6f, omega_CFA_7f, alpha),
               names_to = "estimator", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    estimator = factor(estimator,
                       levels = c("omega_ESEM_6f", "omega_CFA_6f",
                                  "omega_CFA_7f", "alpha"),
                       labels = c("omega (ESEM 6F)", "omega (CFA 6F)",
                                  "omega (CFA 7F)", "alpha")),
    subscale  = factor(subscale,
                       levels = c("Care", "Equality", "Proportionality",
                                  "Loyalty", "Authority", "Purity",
                                  "CountryLoyalty", "GroupLoyalty"))
  )
fig_reliability <- ggplot(fig_rel_df,
                          aes(x = subscale, y = value, fill = estimator)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0.70, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = c(apa_palette, "grey60"), name = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = NULL, y = "Reliability coefficient") +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
apa_save(fig_reliability, "S4_Fig", width = 18, height = 10)

message(sprintf("Figures written to %s.", normalizePath(fig_dir)))

# --- 6.11 PLOS compliance check for the submitted figure files ------------ #
plos_figs <- c("Fig1.tif", "Fig2.tif", "Fig3.tif", "Fig4.tif", "Fig5.tif",
               "S1_Fig.tif", "S2_Fig.tif", "S3_Fig.tif", "S4_Fig.tif")
fig_info <- file.info(file.path(fig_dir, plos_figs))
message("PLOS figure files (TIFF, LZW, 300 dpi):")
for (i in seq_along(plos_figs)) {
  sz <- fig_info$size[i]
  message(sprintf("  %-12s %s", plos_figs[i],
                  if (is.na(sz)) "MISSING" else sprintf("%6.2f MB%s", sz / 1024^2,
                                                        if (sz > 10 * 1024^2) "  <-- exceeds the 10 MB limit" else "")))
}
if (any(is.na(fig_info$size))) warning("At least one PLOS figure file is missing.")


#### --- Save outputs ----------------------------------------------------- ####
out_dir <- file.path(data_dir, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Machine-readable headline results (source for run_check.R) ----------- #
fit_row <- function(fi, model, sample_label) {
  data.frame(
    model = model, sample = sample_label,
    chisq = unname(fi["chisq"]), df = unname(fi["df"]),
    cfi = unname(fi["cfi"]), tli = unname(fi["tli"]),
    rmsea = unname(fi["rmsea"]),
    rmsea_lo = unname(fi["rmsea.ci.lower"]), rmsea_hi = unname(fi["rmsea.ci.upper"]),
    srmr = unname(fi["srmr"]), aic = unname(fi["aic"]), bic = unname(fi["bic"]),
    npar = unname(fi["npar"]), stringsAsFactors = FALSE
  )
}
fit_indices <- rbind(
  fit_row(fi_esem_6f_cal,     "ESEM 6-factor",       "calibration"),
  fit_row(fi_cfa_6f_cal,      "CFA 6-factor",        "calibration"),
  fit_row(fi_esem_7f_cal,     "ESEM 7-factor",       "calibration"),
  fit_row(fi_cfa_7f_cal,      "CFA 7-factor",        "calibration"),
  fit_row(fi_so_cal,          "Second-order",        "calibration"),
  fit_row(fi_cfa_6f_wlsmv,    "CFA 6-factor (WLSMV)","calibration"),
  fit_row(fi_best_validation,    selected_model,        "validation"),
  fit_row(fi_esem_6f_validation, "ESEM 6-factor",       "validation")
)
write.csv(fit_indices, file.path(out_dir, "fit_indices.csv"), row.names = FALSE)

model_comparison$selected <- model_comparison$Model == selected_model
write.csv(model_comparison, file.path(out_dir, "model_comparison.csv"), row.names = FALSE)

inv_rows <- list()
if (exists("fi_config")) {
  inv_rows[[length(inv_rows) + 1]] <- data.frame(
    grouping = "gender",
    model = c("configural", "metric", "scalar"),
    cfi   = c(fi_config["cfi"],   fi_metric["cfi"],   fi_scalar["cfi"]),
    rmsea = c(fi_config["rmsea"], fi_metric["rmsea"], fi_scalar["rmsea"]),
    delta_cfi   = c(NA, delta_cfi_metric,   delta_cfi_scalar),
    delta_rmsea = c(NA, delta_rmsea_metric, delta_rmsea_scalar),
    stringsAsFactors = FALSE)
}
if (exists("fi_config_a")) {
  inv_rows[[length(inv_rows) + 1]] <- data.frame(
    grouping = "age_cohort",
    model = c("configural", "metric", "scalar"),
    cfi   = c(fi_config_a["cfi"],   fi_metric_a["cfi"],   fi_scalar_a["cfi"]),
    rmsea = c(fi_config_a["rmsea"], fi_metric_a["rmsea"], fi_scalar_a["rmsea"]),
    delta_cfi   = c(NA, dcfi_metric_a,   dcfi_scalar_a),
    delta_rmsea = c(NA, drmsea_metric_a, drmsea_scalar_a),
    stringsAsFactors = FALSE)
}
if (length(inv_rows) > 0) {
  invariance_summary <- do.call(rbind, inv_rows)
  invariance_summary$supported <- ifelse(
    is.na(invariance_summary$delta_cfi), NA,
    invariance_summary$delta_cfi >= -0.010 & invariance_summary$delta_rmsea <= 0.015)
  write.csv(invariance_summary, file.path(out_dir, "invariance.csv"), row.names = FALSE)
}

write.csv(as.data.frame(mfq_sub_desc), file.path(out_dir, "descriptives_subscales.csv"),
          row.names = FALSE)
write.csv(data.frame(variable = rownames(mfq_cor), mfq_cor, check.names = FALSE),
          file.path(out_dir, "intercorrelations.csv"), row.names = FALSE)

conf_row <- function(hyp, ct) data.frame(
  hypothesis = hyp, estimate = unname(ct$estimate), p = ct$p.value,
  n = unname(ct$parameter) + 2, stringsAsFactors = FALSE)
hypothesis_tests <- rbind(
  conf_row("H2a_proportionality", h2a), conf_row("H2b_loyalty", h2b),
  conf_row("H2c_authority", h2c),       conf_row("H2d_purity", h2d),
  data.frame(hypothesis = "H3_ind_minus_binding", estimate = unname(h3_coef),
             p = unname(h3_p), n = nobs(h3_model), stringsAsFactors = FALSE),
  conf_row("H4_equality", h4),  conf_row("H5a_care", h5a),
  conf_row("H5b_equality", h5b), conf_row("H6_care", h6)
)
hypothesis_tests$extra <- c(rep(NA_real_, 4), h3_summary$r.squared, rep(NA_real_, 4))
write.csv(hypothesis_tests, file.path(out_dir, "hypothesis_tests.csv"), row.names = FALSE)

sample_characteristics <- data.frame(
  metric = c("N", "age_M", "age_SD", "age_min", "age_max", "n_female", "n_male",
             "n_west", "n_east", "n_berlin", "n_liberal", "n_middle", "n_conservative",
             "n_age_young", "n_age_older", "mde_full_r"),
  value = c(nrow(d), mean(d$age, na.rm = TRUE), sd(d$age, na.rm = TRUE),
            min(d$age, na.rm = TRUE), max(d$age, na.rm = TRUE),
            sum(d$gender == "Female", na.rm = TRUE), sum(d$gender == "Male", na.rm = TRUE),
            sum(d$east_west == "West", na.rm = TRUE), sum(d$east_west == "East", na.rm = TRUE),
            sum(d$east_west == "Berlin", na.rm = TRUE),
            sum(d$pol_group == "Liberal", na.rm = TRUE),
            sum(d$pol_group == "Middle", na.rm = TRUE),
            sum(d$pol_group == "Conservative", na.rm = TRUE),
            sum(d$age_group == "Young", na.rm = TRUE),
            sum(d$age_group == "Older", na.rm = TRUE), mde_full)
)
write.csv(sample_characteristics, file.path(out_dir, "sample_characteristics.csv"),
          row.names = FALSE)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "session_info.txt"))

write.csv(reliability_summary,  file.path(out_dir, "reliability_summary.csv"),  row.names = FALSE)
write.csv(e1_results,           file.path(out_dir, "E1_correlations.csv"),      row.names = FALSE)
write.csv(e2_primary,           file.path(out_dir, "E2_group_comparisons.csv"), row.names = FALSE)
write.csv(e2_sensitivity_relig, file.path(out_dir, "E2_religiosity_sensitivity.csv"),
          row.names = FALSE)
write.csv(e3_results,           file.path(out_dir, "E3_regression_betas.csv"),  row.names = FALSE)
write.csv(e3_omnibus,           file.path(out_dir, "E3_regression_omnibus.csv"),row.names = FALSE)
write.csv(cor_ci_all,     file.path(out_dir, "correlations_ci.csv"),      row.names = FALSE)
write.csv(data.frame(term = "pol_1_lrscale", b = unname(h3_coef),
                     ci_lo = h3_ci[1], ci_hi = h3_ci[2]),
          file.path(out_dir, "h3_ci.csv"), row.names = FALSE)
write.csv(inv_age,        file.path(out_dir, "invariance_age.csv"),       row.names = FALSE)
write.csv(inv_subscales,  file.path(out_dir, "invariance_subscales.csv"), row.names = FALSE)
write.csv(loadings_full,  file.path(out_dir, "loadings_full.csv"),        row.names = FALSE)
save.image(file.path(out_dir, "analysis_workspace.RData"))
message(sprintf("Outputs and workspace written to %s.", normalizePath(out_dir)))