# ============================================================================
# run_check.R — Reproduction check for the MFQ-2 Germany validation study
# ----------------------------------------------------------------------------
# Run AFTER 01_data_preparation.R and 02_data_analysis.R:
#
#   Rscript run_check.R
#
# Reads outputs/*.csv and asserts every headline result reported in the
# Stage 2 manuscript against the reference values of the confirmatory analysis
# (N = 1,171). Prints a PASS/FAIL table and exits non-zero if anything drifts.
#
# All analyses are deterministic: RNGkind is pinned and set.seed(2026) is set
# globally, immediately before the 50/50 split, and before every geomin
# estimation. A FAIL therefore means a real regression (data, package versions,
# or code), not Monte-Carlo noise — investigate before trusting the outputs.
#
# Also verifies that the nine PLOS figure files (TIFF, LZW, 300 dpi) exist and
# stay below the 10 MB limit.
#
# Reference values are stored at the precision reported in the manuscript;
# the tolerances below only absorb that rounding.
# ============================================================================

# --- Locate outputs/ ---------------------------------------------------------
here <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                 error = function(e) getwd())
if (length(here) == 0 || is.na(here) || !dir.exists(file.path(here, "outputs"))) here <- getwd()
OUT <- file.path(here, "outputs")
if (!dir.exists(OUT)) stop("outputs/ not found. Run 01_data_preparation.R and 02_data_analysis.R first.")

# --- Helpers -----------------------------------------------------------------
read1 <- function(f) {
  p <- file.path(OUT, f)
  if (!file.exists(p)) return(NULL)
  tryCatch(read.csv(p, stringsAsFactors = FALSE, check.names = FALSE),
           error = function(e) NULL)
}

# Fetch a numeric cell. Pass one or two key columns to identify the row.
cell <- function(file, out_col, key1 = NULL, val1 = NULL, key2 = NULL, val2 = NULL) {
  d <- read1(file); if (is.null(d) || !out_col %in% names(d)) return(NA_real_)
  keep <- rep(TRUE, nrow(d))
  if (!is.null(key1)) keep <- keep & as.character(d[[key1]]) == val1
  if (!is.null(key2)) keep <- keep & as.character(d[[key2]]) == val2
  row <- which(keep)
  if (length(row) == 0) return(NA_real_)
  suppressWarnings(as.numeric(as.character(d[[out_col]][row[1]])))
}

.results <- list()
check <- function(label, actual, expected, tol = T_EXACT) {
  ok <- length(actual) == 1 && is.finite(actual) && abs(actual - expected) <= tol
  .results[[length(.results) + 1]] <<- data.frame(
    check = label, expected = expected,
    actual = ifelse(length(actual) == 1 && is.finite(actual), round(actual, 4), NA),
    tol = tol, status = ifelse(ok, "PASS", "FAIL"), stringsAsFactors = FALSE
  )
  invisible(ok)
}

# --- Tolerances --------------------------------------------------------------
T_EXACT <- 0.0011   # 3-decimal reference values (fit indices, r, omega, p)
T_2DEC  <- 0.006    # 2-decimal reference values (M, SD, d)
T_BIG   <- 0.05     # chi-square / information criteria
T_INT   <- 0        # counts

# --- Staleness guard ---------------------------------------------------------
script <- file.path(here, "02_data_analysis.R")
# Only the CSVs written by 02_data_analysis.R are checked for staleness;
# final_dataset.csv and exclusion_log.csv come from 01_data_preparation.R and
# are legitimately older.
csvs <- setdiff(list.files(OUT, pattern = "\\.csv$", full.names = TRUE),
                file.path(OUT, c("processed_data.csv", "final_dataset.csv",
                                 "exclusion_log.csv")))
if (file.exists(script) && length(csvs) > 0 &&
    any(file.mtime(csvs) < file.mtime(script))) {
  message("WARNING: some outputs/*.csv are older than 02_data_analysis.R — re-run the analysis.")
}

# --- Sample and screening ----------------------------------------------------
check("N = 1,171",            cell("sample_characteristics.csv", "value", "metric", "N"), 1171, T_INT)
check("Age M = 30.87",        cell("sample_characteristics.csv", "value", "metric", "age_M"), 30.87, T_2DEC)
check("Age SD = 9.08",        cell("sample_characteristics.csv", "value", "metric", "age_SD"), 9.08, T_2DEC)
check("Women n = 597",        cell("sample_characteristics.csv", "value", "metric", "n_female"), 597, T_INT)
check("Men n = 574",          cell("sample_characteristics.csv", "value", "metric", "n_male"), 574, T_INT)
check("West n = 971",         cell("sample_characteristics.csv", "value", "metric", "n_west"), 971, T_INT)
check("East n = 142",         cell("sample_characteristics.csv", "value", "metric", "n_east"), 142, T_INT)
check("Berlin n = 58",        cell("sample_characteristics.csv", "value", "metric", "n_berlin"), 58, T_INT)
check("Liberal n = 755",      cell("sample_characteristics.csv", "value", "metric", "n_liberal"), 755, T_INT)
check("Centrist n = 301",     cell("sample_characteristics.csv", "value", "metric", "n_middle"), 301, T_INT)
check("Conservative n = 115", cell("sample_characteristics.csv", "value", "metric", "n_conservative"), 115, T_INT)
check("Age cohort young = 897", cell("sample_characteristics.csv", "value", "metric", "n_age_young"), 897, T_INT)
check("Age cohort older = 274", cell("sample_characteristics.csv", "value", "metric", "n_age_older"), 274, T_INT)
check("MDE |r| = .082",       cell("sample_characteristics.csv", "value", "metric", "mde_full_r"), 0.082, T_EXACT)

excl <- read1("exclusion_log.csv")
check("Exclusions total = 28", if (is.null(excl)) NA else sum(excl$n_excluded), 28, T_INT)
check("Final n in log = 1,171", if (is.null(excl)) NA else excl$n_remaining[nrow(excl)], 1171, T_INT)

# --- Table 1: descriptives and intercorrelations -----------------------------
check("Care M = 4.10",            cell("descriptives_subscales.csv", "M",  "subscale", "care_mean"), 4.1033, T_2DEC)
check("Care SD = 0.60",           cell("descriptives_subscales.csv", "SD", "subscale", "care_mean"), 0.6049, T_2DEC)
check("Equality M = 3.13",        cell("descriptives_subscales.csv", "M",  "subscale", "equality_mean"), 3.1267, T_2DEC)
check("Equality SD = 0.91",       cell("descriptives_subscales.csv", "SD", "subscale", "equality_mean"), 0.9061, T_2DEC)
check("Proportionality M = 4.00", cell("descriptives_subscales.csv", "M",  "subscale", "proportionality_mean"), 4.0009, T_2DEC)
check("Proportionality SD = 0.53",cell("descriptives_subscales.csv", "SD", "subscale", "proportionality_mean"), 0.5317, T_2DEC)
check("Loyalty M = 2.79",         cell("descriptives_subscales.csv", "M",  "subscale", "loyalty_mean"), 2.7926, T_2DEC)
check("Loyalty SD = 0.68",        cell("descriptives_subscales.csv", "SD", "subscale", "loyalty_mean"), 0.6789, T_2DEC)
check("Authority M = 3.02",       cell("descriptives_subscales.csv", "M",  "subscale", "authority_mean"), 3.0246, T_2DEC)
check("Authority SD = 0.71",      cell("descriptives_subscales.csv", "SD", "subscale", "authority_mean"), 0.7148, T_2DEC)
check("Purity M = 2.31",          cell("descriptives_subscales.csv", "M",  "subscale", "purity_mean"), 2.3081, T_2DEC)
check("Purity SD = 0.68",         cell("descriptives_subscales.csv", "SD", "subscale", "purity_mean"), 0.6774, T_2DEC)

check("r Care-Equality = .422",   cell("intercorrelations.csv", "equality_mean", "variable", "care_mean"), 0.422, T_EXACT)
check("r Care-Purity = .005",     cell("intercorrelations.csv", "purity_mean",   "variable", "care_mean"), 0.005, T_EXACT)
check("r Loyalty-Authority = .699", cell("intercorrelations.csv", "authority_mean", "variable", "loyalty_mean"), 0.699, T_EXACT)
check("r Authority-Purity = .534",  cell("intercorrelations.csv", "purity_mean",    "variable", "authority_mean"), 0.534, T_EXACT)
check("r Equality-Authority = -.447", cell("intercorrelations.csv", "authority_mean", "variable", "equality_mean"), -0.447, T_EXACT)

# --- Table 2: structural models (H1a) ----------------------------------------
check("ESEM 6F CFI = .961",   cell("fit_indices.csv", "cfi",   "model", "ESEM 6-factor", "sample", "calibration"), 0.961, T_EXACT)
check("ESEM 6F TLI = .942",   cell("fit_indices.csv", "tli",   "model", "ESEM 6-factor", "sample", "calibration"), 0.942, T_EXACT)
check("ESEM 6F RMSEA = .039", cell("fit_indices.csv", "rmsea", "model", "ESEM 6-factor", "sample", "calibration"), 0.039, T_EXACT)
check("ESEM 6F SRMR = .027",  cell("fit_indices.csv", "srmr",  "model", "ESEM 6-factor", "sample", "calibration"), 0.027, T_EXACT)
check("ESEM 6F chi2 = 820.50",cell("fit_indices.csv", "chisq", "model", "ESEM 6-factor", "sample", "calibration"), 820.50, T_BIG)
check("CFA 6F CFI = .893",    cell("fit_indices.csv", "cfi",   "model", "CFA 6-factor",  "sample", "calibration"), 0.893, T_EXACT)
check("CFA 6F RMSEA = .050",  cell("fit_indices.csv", "rmsea", "model", "CFA 6-factor",  "sample", "calibration"), 0.050, T_EXACT)
check("ESEM 7F CFI = .975",   cell("fit_indices.csv", "cfi",   "model", "ESEM 7-factor", "sample", "calibration"), 0.975, T_EXACT)
check("CFA 7F CFI = .911",    cell("fit_indices.csv", "cfi",   "model", "CFA 7-factor",  "sample", "calibration"), 0.911, T_EXACT)
check("CFA 7F RMSEA = .046",  cell("fit_indices.csv", "rmsea", "model", "CFA 7-factor",  "sample", "calibration"), 0.046, T_EXACT)
check("CFA 7F BIC = 51120.50",cell("fit_indices.csv", "bic",   "model", "CFA 7-factor",  "sample", "calibration"), 51120.50, T_BIG)
check("Second-order CFI = .902", cell("fit_indices.csv", "cfi", "model", "Second-order", "sample", "calibration"), 0.902, T_EXACT)
check("WLSMV CFI = .861",     cell("fit_indices.csv", "cfi",   "model", "CFA 6-factor (WLSMV)", "sample", "calibration"), 0.861, T_EXACT)
check("WLSMV RMSEA = .068",   cell("fit_indices.csv", "rmsea", "model", "CFA 6-factor (WLSMV)", "sample", "calibration"), 0.068, T_EXACT)
check("Validation CFA 7F CFI = .900",   cell("fit_indices.csv", "cfi",   "model", "CFA 7-factor",  "sample", "validation"), 0.900, T_EXACT)
check("Validation CFA 7F RMSEA = .051", cell("fit_indices.csv", "rmsea", "model", "CFA 7-factor",  "sample", "validation"), 0.051, T_EXACT)
check("Validation ESEM 6F CFI = .949",  cell("fit_indices.csv", "cfi",   "model", "ESEM 6-factor", "sample", "validation"), 0.949, T_EXACT)
check("Validation ESEM 6F RMSEA = .045",cell("fit_indices.csv", "rmsea", "model", "ESEM 6-factor", "sample", "validation"), 0.045, T_EXACT)

check("dBIC ESEM 6F = 400.42", cell("model_comparison.csv", "delta_BIC", "Model", "ESEM 6-factor"), 400.42, T_BIG)
check("dBIC CFA 6F = 111.37",  cell("model_comparison.csv", "delta_BIC", "Model", "CFA 6-factor"),  111.37, T_BIG)
check("dBIC ESEM 7F = 453.80", cell("model_comparison.csv", "delta_BIC", "Model", "ESEM 7-factor"), 453.80, T_BIG)
check("dBIC CFA 7F = 0 (selected)", cell("model_comparison.csv", "delta_BIC", "Model", "CFA 7-factor"), 0, T_BIG)
check("dBIC Second-order = 8.37",   cell("model_comparison.csv", "delta_BIC", "Model", "Second-order"), 8.37, T_BIG)

# --- Table 3: reliability (H1b) ----------------------------------------------
check("omega ESEM Care = .861",            cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Care"), 0.8606, T_EXACT)
check("omega ESEM Equality = .868",        cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Equality"), 0.8675, T_EXACT)
check("omega ESEM Proportionality = .743", cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Proportionality"), 0.7431, T_EXACT)
check("omega ESEM Loyalty = .622",         cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Loyalty"), 0.6220, T_EXACT)
check("omega ESEM Authority = .728",       cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Authority"), 0.7277, T_EXACT)
check("omega ESEM Purity = .551",          cell("reliability_summary.csv", "omega_ESEM_6f", "subscale", "Purity"), 0.5513, T_EXACT)
check("omega CFA Loyalty = .756",          cell("reliability_summary.csv", "omega_CFA_6f",  "subscale", "Loyalty"), 0.7561, T_EXACT)
check("omega CFA Purity = .703",           cell("reliability_summary.csv", "omega_CFA_6f",  "subscale", "Purity"), 0.7026, T_EXACT)
check("omega Country Loyalty = .819",      cell("reliability_summary.csv", "omega_CFA_7f",  "subscale", "CountryLoyalty"), 0.8195, T_EXACT)
check("omega Group Loyalty = .530",        cell("reliability_summary.csv", "omega_CFA_7f",  "subscale", "GroupLoyalty"), 0.5300, T_EXACT)
check("alpha Care = .857",                 cell("reliability_summary.csv", "alpha", "subscale", "Care"), 0.8572, T_EXACT)
check("alpha Purity = .693",               cell("reliability_summary.csv", "alpha", "subscale", "Purity"), 0.6930, T_EXACT)

# --- Table 4: measurement invariance -----------------------------------------
check("Gender configural CFI = .886", cell("invariance.csv", "cfi", "grouping", "gender", "model", "configural"), 0.886, T_EXACT)
check("Gender metric CFI = .883",     cell("invariance.csv", "cfi", "grouping", "gender", "model", "metric"), 0.883, T_EXACT)
check("Gender scalar CFI = .867",     cell("invariance.csv", "cfi", "grouping", "gender", "model", "scalar"), 0.867, T_EXACT)
check("Gender metric dCFI = -.002",   cell("invariance.csv", "delta_cfi",   "grouping", "gender", "model", "metric"), -0.002, T_EXACT)
check("Gender scalar dCFI = -.017",   cell("invariance.csv", "delta_cfi",   "grouping", "gender", "model", "scalar"), -0.017, T_EXACT)
check("Gender scalar dRMSEA = .003",  cell("invariance.csv", "delta_rmsea", "grouping", "gender", "model", "scalar"), 0.003, T_EXACT)
check("Age configural CFI = .893",    cell("invariance.csv", "cfi", "grouping", "age_cohort", "model", "configural"), 0.893, T_EXACT)
check("Age metric dCFI = -.002",      cell("invariance.csv", "delta_cfi",   "grouping", "age_cohort", "model", "metric"), -0.002, T_EXACT)
check("Age metric dRMSEA = .000",     cell("invariance.csv", "delta_rmsea", "grouping", "age_cohort", "model", "metric"), 0.000, T_EXACT)
check("Age scalar dCFI = -.008",      cell("invariance.csv", "delta_cfi",   "grouping", "age_cohort", "model", "scalar"), -0.008, T_EXACT)
check("Age scalar dRMSEA = .001",     cell("invariance.csv", "delta_rmsea", "grouping", "age_cohort", "model", "scalar"), 0.001, T_EXACT)

# --- Confirmatory hypotheses (H2-H6) -----------------------------------------
check("H2a r = .342", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H2a_proportionality"), 0.342, T_EXACT)
check("H2b r = .566", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H2b_loyalty"),        0.566, T_EXACT)
check("H2c r = .605", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H2c_authority"),      0.605, T_EXACT)
check("H2d r = .384", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H2d_purity"),         0.384, T_EXACT)
check("H3 b = -0.434", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H3_ind_minus_binding"), -0.434, T_EXACT)
check("H3 R2 = .565",  cell("hypothesis_tests.csv", "extra",    "hypothesis", "H3_ind_minus_binding"),  0.565, T_EXACT)
check("H4 r = -.634", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H4_equality"), -0.634, T_EXACT)
check("H5a r = .415", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H5a_care"),      0.415, T_EXACT)
check("H5b r = .132", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H5b_equality"),  0.132, T_EXACT)
check("H6 r = -.006", cell("hypothesis_tests.csv", "estimate", "hypothesis", "H6_care"),      -0.006, T_EXACT)

# --- Exploratory analyses (E1-E3) --------------------------------------------
check("E1 Religiosity-Purity r = .495",
      cell("E1_correlations.csv", "r", "validation_var", "religiosity", "mfq_subscale", "purity_mean"), 0.495, T_EXACT)
check("E1 Idealism-Care r = .418",
      cell("E1_correlations.csv", "r", "validation_var", "idealism_mean", "mfq_subscale", "care_mean"), 0.418, T_EXACT)
check("E1 Economic ideology-Equality r = -.521",
      cell("E1_correlations.csv", "r", "validation_var", "pol_3_economic", "mfq_subscale", "equality_mean"), -0.521, T_EXACT)
check("E3 Purity ~ Religiosity beta = .417",
      cell("E3_regression_betas.csv", "beta", "DV", "purity_mean", "predictor", "religiosity"), 0.417, T_EXACT)
check("E3 Equality ~ Gender beta = .269",
      cell("E3_regression_betas.csv", "beta", "DV", "equality_mean", "predictor", "gender_dummy"), 0.269, T_EXACT)
check("E3 Equality R2 = .427",
      cell("E3_regression_omnibus.csv", "R2", "DV", "equality_mean"), 0.427, T_EXACT)

# --- PLOS figure files -------------------------------------------------------
FIGD <- file.path(here, "figures")
plos_figs <- c("Fig1.tif", "Fig2.tif", "Fig3.tif", "Fig4.tif", "Fig5.tif",
               "S1_Fig.tif", "S2_Fig.tif", "S3_Fig.tif", "S4_Fig.tif")
for (f in plos_figs) {
  p <- file.path(FIGD, f)
  check(paste0("figure ", f, " present"), as.numeric(file.exists(p)), 1, T_INT)
  if (file.exists(p)) check(paste0("figure ", f, " < 10 MB"),
                            as.numeric(file.size(p) < 10 * 1024^2), 1, T_INT)
}

# --- Report ------------------------------------------------------------------
tab <- do.call(rbind, .results)
tab$exp_s <- sprintf("%.4f", tab$expected)
tab$act_s <- ifelse(is.na(tab$actual), "  NA  ", sprintf("%.4f", tab$actual))
n_pass <- sum(tab$status == "PASS"); n_fail <- sum(tab$status == "FAIL"); n_tot <- nrow(tab)

cat("\n", strrep("=", 84), "\n", sep = "")
cat("  Reproduction check - MFQ-2 Germany validation (N = 1,171)\n")
cat("  Tolerances: ", format(T_EXACT), " (3-decimal), ", format(T_2DEC),
    " (2-decimal), ", format(T_BIG), " (chi2/IC)\n", sep = "")
cat(strrep("=", 84), "\n", sep = "")
fmt <- "  %-38s exp %-11s got %-11s [%s]\n"
for (i in seq_len(n_tot)) cat(sprintf(fmt, tab$check[i], tab$exp_s[i], tab$act_s[i], tab$status[i]))
cat(strrep("-", 84), "\n", sep = "")
cat(sprintf("  %d/%d passed", n_pass, n_tot))
if (n_fail > 0) cat(sprintf("  |  %d FAILED", n_fail))
cat("\n", strrep("=", 84), "\n\n", sep = "")

if (n_fail > 0) {
  cat("  FAILURES - this run does not reproduce the reported values:\n")
  print(tab[tab$status == "FAIL", c("check", "expected", "actual")], row.names = FALSE)
  cat("\n  If 'got' is NA, the source CSV/column is missing (analysis step did not run,\n")
  cat("  or 02_data_analysis.R predates the export block).\n\n")
  if (interactive()) stop("Reproduction check failed.", call. = FALSE) else quit(status = 1)
} else {
  cat("  All headline results reproduce within tolerance. Outputs are trustworthy.\n\n")
  if (!interactive()) quit(status = 0)
}
