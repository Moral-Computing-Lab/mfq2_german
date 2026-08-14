# ============================================================================
# Screening Script — Identifying Participants Based on Response Patterns
# ----------------------------------------------------------------------------
# Standalone helper used during data collection to identify participants who
# failed the AI-agent detection task (response pattern in v_87 to v_91) or at
# least two of three attention checks; it informed compensation decisions on
# Prolific and is not part of the analysis pipeline. The pre-registered
# exclusions reported in the manuscript are implemented in 01_data_preparation.R.
# Requires the restricted raw export and is therefore not runnable from this
# deposit; it is included for documentation.
#
# Content:
#
# Phase 1: Prepare working environment
#   1.1 File paths and global variables
#
# Phase 2: Read and recode dataset
#   2.1 Data import
#
# Phase 3: Inspect dataset and screening
#   3.1 Apply screening logic
#   3.2 Verify results
# ============================================================================

#### --- Phase 1: Prepare working environment ---------------------------- ####

# --- 1.1 File paths and global variables ---------------------------------- #
# Assumes the repository root as working directory (as in 01/02).
data_dir  <- "."
file_pattern <- "^data_project_.*\\.csv$"

matching_files <- list.files(path = data_dir, pattern = file_pattern, full.names = TRUE)

if (length(matching_files) == 0) {
  stop("No file matching the pattern 'data_project_*.csv' found in the directory.")
}

# Select the latest file (based on alphanumeric sorting, assuming date suffix)
data_path <- sort(matching_files, decreasing = TRUE)[1]


#### --- Phase 2: Read and recode dataset -------------------------------- ####

# --- 2.1 Data import ------------------------------------------------------ #
data <- read.csv2(data_path, header = TRUE, stringsAsFactors = FALSE)

message(sprintf("File found: %s", basename(data_path)))
message(sprintf("Dataset successfully imported (N = %d).", nrow(data)))


#### --- Phase 3: Inspect dataset and screening -------------------------- ####

# --- 3.1 Apply screening logic -------------------------------------------- #

# Criterion 1: response pattern v_87-v_91 deviates from 1, 2, 3, 4, 5.
pattern_failed <- !(data$v_87 == 1 & data$v_88 == 2 & data$v_89 == 3 & data$v_90 == 4 & data$v_91 == 5)

# Criterion 2: at least two of three attention checks failed.
# Thresholds: attention_check_1 = 2, attention_check_2 = 3, attention_check_3 = 5
att_check_failed <- (data$attention_check_1 != 2) +
  (data$attention_check_2 != 3) +
  (data$attention_check_3 != 5) >= 2

data$identified <- pattern_failed | att_check_failed

identified_ids <- data$p_0001[data$identified]


# --- 3.2 Verify results --------------------------------------------------- #

n_total <- nrow(data)
n_identified <- sum(data$identified)
n_not_identified <- sum(!data$identified)

check_sum <- (n_identified + n_not_identified) == n_total

message(sprintf("N_total = %d", n_total))
message(sprintf("N_identified = %d", n_identified))
message(sprintf("N_not_identified = %d", n_not_identified))
message(sprintf("Sum consistency check successful: %s", ifelse(check_sum, "YES", "NO")))

if (n_identified > 0) {
  message("Identified Prolific IDs:")
  cat(identified_ids, sep = "\n")
}
