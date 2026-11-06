# Analysis code: German MFQ-2 validation study

Working copy of the analysis code for the Registered Report "Validation of the Moral Foundations Questionnaire-2 (MFQ-2) in Germany: Psychometric properties and associations with political ideology, religiosity, and personality" by Nico S. Musa, Sarah M. Müller and Frederic R. Hopp (Leibniz Institute for Psychology, ZPID, and Trier University).

- **Stage 1 Registered Report Protocol:** *PLoS One*, 2026, 21(3), e0345599. [doi:10.1371/journal.pone.0345599](https://doi.org/10.1371/journal.pone.0345599)
- **Stage 2 article:** DOI will be added upon acceptance.

## Archival version

This repository is a working copy. The citable, archived version of the code and of the data lives on PsychArchives:

| Object             | Content                                                                               | DOI                                                                          |
| ------------------ | ------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| Research Data      | de-identified dataset (*N* = 1,171), exclusion log, codebooks — CC BY 4.0             | [10.23668/psycharchives.22700](https://doi.org/10.23668/psycharchives.22700) |
| Code               | the five scripts below, HTML reports (with PDF/A copies), session record — Apache 2.0 | [10.23668/psycharchives.22701](https://doi.org/10.23668/psycharchives.22701) |
| Stage 1 protocol   | manuscript                                                                            | [10.23668/psycharchives.21651](https://doi.org/10.23668/psycharchives.21651) |
| Measures and items | all instruments as administered                                                       | [10.23668/psycharchives.21652](https://doi.org/10.23668/psycharchives.21652) |

The dataset is not mirrored here. Download `final_dataset.csv` and `exclusion_log.csv` from the Research Data object above; this keeps a single authoritative source for the data.

## Contents

| File                    | Purpose                                                                                                                                                                                                                                  |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `00_screening_script.R` | Standalone helper used during data collection to identify participants who failed the AI-agent detection task or at least two of three attention checks; informed compensation decisions on Prolific. Not part of the analysis pipeline. |
| `01_data_preparation.R` | Raw exports → `final_dataset.csv` and `exclusion_log.csv` (pre-registered case-wise screening, scale construction, grouping variables).                                                                                                  |
| `02_data_analysis.R`    | All analyses, result tables and figures reported in the article (six phases).                                                                                                                                                            |
| `run_check.R`           | Reproduction check: asserts 238 reported values against the script output and exits non-zero on any drift.                                                                                                                               |
| `render_reports.R`      | Renders `02_data_analysis.R` and `run_check.R` unchanged to HTML (`knitr::spin`) in `reports/`, so that the full console output can be inspected without re-running the analyses. |
| `reports/`              | HTML reports of a complete run (`02_data_analysis.html`, `run_check.html`). |
| `session_info.txt`      | Authoritative record of the run environment (R and package versions).                                                                                                                                                                    |

## How to run

```bash
mkdir outputs
# place final_dataset.csv and exclusion_log.csv (from PsychArchives) into outputs/
Rscript 02_data_analysis.R   # reproduces all results, tables, and figures
Rscript run_check.R          # verifies the run against the reported values
Rscript render_reports.R     # optional: HTML reports in reports/
```

`01_data_preparation.R` and `00_screening_script.R` require the raw Unipark and Prolific exports, which are withheld because they contain recruitment identifiers. They are included for documentation and cannot be re-run from the public data. `02_data_analysis.R` and `run_check.R` run from the de-identified dataset alone.

### Reproducibility

`set.seed(2026)` is set globally, immediately before the 50/50 split, and before every geomin estimation; `RNGkind` is pinned (Mersenne-Twister / Inversion / Rejection) so that the split and the random starts survive R-version changes. All analyses are deterministic: repeated runs return identical values, and the exported TIFF figures are bit-identical across runs.

### Environment

R 4.6.1 with `tidyverse` 2.0.0, `lavaan` 0.6-21, `semTools` 0.5-8, `psych` 2.6.3, `moments` 0.14.1, `GPArotation` 2025.3-1, `effectsize` 1.0.2, `car` 3.1-5, `ggplot2` 4.0.3, `knitr` 1.51, `rmarkdown` 2.31, `pwr` 1.3-0, `semPlot` 1.1.8. `session_info.txt` is authoritative.

## Citation

Please cite the article, not this repository. Until the Stage 2 article appears, cite the Stage 1 protocol:

> Musa, N. S., Müller, S. M., & Hopp, F. R. (2026). Validation of the moral foundations questionnaire-2 (MFQ-2) in Germany: Psychometric properties and associations with political ideology, religiosity, and personality. *PLoS One, 21*(3), e0345599. [https://doi.org/10.1371/journal.pone.0345599](https://doi.org/10.1371/journal.pone.0345599)

## License

Apache License 2.0—see `LICENSE`.
