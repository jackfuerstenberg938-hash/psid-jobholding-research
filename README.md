# Multiple Job Holding and Household Expenditures

This project examines the relationship between multiple job holding and household expenditures using data from the Panel Study of Income Dynamics (PSID).

## Data

The analysis uses PSID family-level data spanning multiple survey waves. Because variable identifiers change across waves, the R script harmonizes income, employment, household composition, and expenditure variables into a consistent household-year dataset.

Raw PSID data are not included in this repository in accordance with PSID data-use requirements.

## Analysis

The script:

- parses the PSID fixed-width data layout from a Stata do-file;
- harmonizes variables across survey waves;
- constructs measures of multiple job holding and household expenditures;
- estimates quantile regressions across the spending distribution;
- examines discretionary and essential expenditures; and
- uses propensity score matching as a robustness analysis.

## Tools

The analysis is conducted in R using packages including `dplyr`, `purrr`, `readr`, `stringr`, `quantreg`, `MatchIt`, `ggplot2`, and `modelsummary`.

## Files

`psid_jobholding_analysis.R` contains the data preparation and analysis workflow.