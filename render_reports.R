# ============================================================================
# render_reports.R -- HTML reports of the analysis scripts
# ----------------------------------------------------------------------------
# Renders 02_data_analysis.R and run_check.R unchanged via knitr::spin()
# (rmarkdown::render) into reports/*.html: code, console output, and messages
# of a complete run, viewable without re-running the analyses.
# 01_data_preparation.R is not rendered: it requires the withheld raw exports,
# and its output would contain recruitment identifiers.
#
# Usage (from the repository root, with final_dataset.csv and
# exclusion_log.csv in outputs/):
#   Rscript render_reports.R            # both scripts
#   Rscript render_reports.R run_check.R  # a single script
# ============================================================================

suppressPackageStartupMessages(library(rmarkdown))  # v2.31
suppressPackageStartupMessages(library(knitr))      # v1.51

report_dir <- "reports"
if (!dir.exists(report_dir)) dir.create(report_dir)

# run_check.R ends with quit(status = ...) in non-interactive sessions, which
# would terminate the rendering process. During rendering, quit() only reports
# the exit status; the check table itself is part of the report.
render_env <- function() {
  e <- new.env()
  e$quit <- function(status = 0, ...) message(sprintf("[render] quit(status = %d) suppressed.", status))
  e
}

scripts <- commandArgs(trailingOnly = TRUE)
if (length(scripts) == 0) scripts <- c("02_data_analysis.R", "run_check.R")

for (script in scripts) {
  rmarkdown::render(
    input         = script,
    output_format = rmarkdown::html_document(toc = TRUE, toc_float = TRUE,
                                             self_contained = TRUE),
    output_dir    = report_dir,
    knit_root_dir = getwd(),
    envir         = render_env(),
    quiet         = TRUE
  )
  # Replace the absolute repository path in the console output by "." so that
  # the reports contain no local user or directory names.
  html_file <- file.path(report_dir, paste0(tools::file_path_sans_ext(script), ".html"))
  html <- readLines(html_file, encoding = "UTF-8", warn = FALSE)
  for (pth in unique(c(normalizePath(getwd(), winslash = "\\"),
                       normalizePath(getwd(), winslash = "/")))) {
    html <- gsub(pth, ".", html, fixed = TRUE)
  }
  # knitr::spin() inserts the system user name as author; remove it.
  html <- html[!grepl('<meta name="author"', html, fixed = TRUE)]
  html <- gsub('<h4 class="author">[^<]*</h4>', "", html)
  writeLines(html, html_file, useBytes = TRUE)
  message(sprintf("Rendered %s -> %s.", script, html_file))
}
