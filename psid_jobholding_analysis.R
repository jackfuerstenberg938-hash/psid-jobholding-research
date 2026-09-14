# Packages ----------------------------------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)
library(MatchIt)
library(quantreg)
library(ggplot2)
library(scales)
library(modelsummary)

# Import and parse PSID data -----------------------------------------------

# Extract variable names and fixed-width positions from the do-file
do_lines <- readLines("J361095.do")
all_text <- paste(do_lines, collapse = " ")
matches <- str_match_all(
  all_text,
  "(?:byte|int|long|double|float)?\\s*(ER[0-9A-Z]+)\\s+([0-9]+)\\s*-\\s*([0-9]+)"
)[[1]]
layout <- tibble(
  varname = matches[, 2],
  start   = as.integer(matches[, 3]),
  end     = as.integer(matches[, 4])
) %>%
  filter(!is.na(varname)) %>%
  distinct()
# Read the raw fixed-width PSID file using positions recovered above
psid <- read_fwf(
  "J361095.txt",
  fwf_positions(
    start = layout$start,
    end = layout$end,
    col_names = layout$varname
  ),
  progress = FALSE
)
dim(psid)

# Identify job variables ---------------------------------------------------

# Parse variable labels and inspect variables related to number of jobs
label_mat <- str_match(
  do_lines,
  'label variable\\s+(ER[0-9A-Z]+)\\s+"([^"]+)"'
)
var_labels <- tibble(
  varname = label_mat[, 2],
  label   = label_mat[, 3]
) %>%
  filter(!is.na(varname))
var_labels %>%
  filter(str_detect(label, regex("NUMBER OF JOBS", ignore_case = TRUE))) %>%
  print(n = 50)

# Harmonize variables across PSID waves ------------------------------------

# PSID variable IDs change across waves; map each wave to common concepts
waves <- tribble(
  ~interview_year, ~jobs_year, ~famid_var, ~fu_var,
  ~head_jobs_var, ~spouse_jobs_var, ~income_var,
  ~food_home_var, ~food_away_var, ~housing_var, ~utility_var,
  ~transport_var, ~health_var, ~trips_var, ~recreation_var, ~total_exp_var,
  
  2003, 2001, "ER21002", "ER21016", "ER23702D4", "ER23702J7", "ER24099",
  "ER24138A2", "ER24138A3", "ER24138A5", "ER24138B1", "ER24138B6",
  "ER24138D2", NA, NA, "ER24138D7",
  
  2005, 2003, "ER25002", "ER25016", "ER27711D4", "ER27711J7", "ER28037",
  "ER28037A2", "ER28037A3", "ER28037A5", "ER28037B1", "ER28037B7",
  "ER28037D3", "ER28037E2", "ER28037E3", "ER28037E4",
  
  2007, 2005, "ER36002", "ER36016", "ER40686D4", "ER40686J7", "ER41027",
  "ER41027A2", "ER41027A3", "ER41027A5", "ER41027B1", "ER41027B7",
  "ER41027D3", "ER41027E2", "ER41027E3", "ER41027E4",
  
  2009, 2007, "ER42002", "ER42016", "ER46670A", "ER46681A", "ER46935",
  "ER46971A2", "ER46971A3", "ER46971A5", "ER46971B1", "ER46971B7",
  "ER46971D3", "ER46971E2", "ER46971E3", "ER46971E4",
  
  2011, 2009, "ER47302", "ER47316", "ER52071A", "ER52082A", "ER52343",
  "ER52395A2", "ER52395A3", "ER52395A5", "ER52395B1", "ER52395B7",
  "ER52395D3", "ER52395E2", "ER52395E3", "ER52395E4",
  
  2013, 2011, "ER53002", "ER53016", "ER57826", "ER57874", "ER58152",
  "ER58212A2", "ER58212A3", "ER58212A5", "ER58212B1", "ER58212B7",
  "ER58212D3", "ER58212E2", "ER58212E3", "ER58212E4",
  
  2015, 2013, "ER60002", "ER60016", "ER65006", "ER65054", "ER65349",
  "ER65411", "ER65412", "ER65414", "ER65419", "ER65425",
  "ER65439", "ER65447", "ER65448", "ER65448B",
  
  2017, 2015, "ER66002", "ER66016", "ER71098", "ER71146", "ER71426",
  "ER71488", "ER71489", "ER71491", "ER71497", "ER71503",
  "ER71517", "ER71526", "ER71527", "ER71527B",
  
  2019, 2017, "ER72002", "ER72016", "ER77120", "ER77168", "ER77448",
  "ER77514", "ER77516", "ER77520", "ER77531", "ER77539",
  "ER77566", "ER77583", "ER77585", "ER77587",
  
  2021, 2019, "ER78002", "ER78016", "ER81456", "ER81504", "ER81775",
  "ER81841", "ER81843", "ER81847", "ER81858", "ER81866",
  "ER81893", "ER81910", "ER81912", "ER81914",
  
  2023, 2021, "ER82002", "ER82017", "ER85313", "ER85361", "ER85629",
  "ER85695", "ER85697", "ER85701", "ER85712", "ER85720",
  "ER85747", "ER85764", "ER85766", "ER85768"
)
# Return the requested variable, or NA when that concept is unavailable in a wave
safe_pull <- function(data, var) {
  if (is.na(var)) rep(NA_real_, nrow(data)) else as.numeric(data[[var]])
}

# Construct harmonized household-year panel --------------------------------

# Iterate across survey waves, pulling the wave-specific PSID variables
# into a consistent set of variable names and stacking them into long format
psid_long <- pmap_dfr(
  waves,
  function(interview_year, jobs_year, famid_var, fu_var,
           head_jobs_var, spouse_jobs_var, income_var,
           food_home_var, food_away_var, housing_var, utility_var,
           transport_var, health_var, trips_var, recreation_var, total_exp_var) {
    
    tibble(
      famid = safe_pull(psid, famid_var),
      interview_year = interview_year,
      jobs_year = jobs_year,
      fu_size = safe_pull(psid, fu_var),
      head_jobs = safe_pull(psid, head_jobs_var),
      spouse_jobs = safe_pull(psid, spouse_jobs_var),
      income_lag = safe_pull(psid, income_var),
      food_home = safe_pull(psid, food_home_var),
      food_away = safe_pull(psid, food_away_var),
      housing = safe_pull(psid, housing_var),
      utilities = safe_pull(psid, utility_var),
      transport = safe_pull(psid, transport_var),
      healthcare = safe_pull(psid, health_var),
      trips = safe_pull(psid, trips_var),
      recreation = safe_pull(psid, recreation_var),
      total_exp = safe_pull(psid, total_exp_var)
    )
  }
)

# Clean data and construct analysis variables -------------------------------

# Recode negative PSID values as missing before constructing outcomes
psid_long <- psid_long %>%
  mutate(
    across(
      c(fu_size, head_jobs, spouse_jobs, income_lag, food_home, food_away,
        housing, utilities, transport, healthcare, trips, recreation, total_exp),
      ~ ifelse(.x < 0, NA, .x)
    ),
    fu_size = ifelse(fu_size <= 0, NA, fu_size),
    # Treat missing head/spouse job counts as zero for household job measures
    head_jobs = ifelse(is.na(head_jobs), 0, head_jobs),
    spouse_jobs = ifelse(is.na(spouse_jobs), 0, spouse_jobs),
    jobs = head_jobs + spouse_jobs,
    # Group expenditure components into essential and discretionary spending
    essential_exp = food_home + housing + utilities + transport + healthcare,
    discretionary_exp = food_away + trips + recreation,
    # Convert expenditure measures to per-capita values
    total_exp_pc = total_exp / fu_size,
    essential_exp_pc = essential_exp / fu_size,
    discretionary_exp_pc = discretionary_exp / fu_size,
    # Multiple job holding is defined as either the head or spouse holding >1 job
    multijob = ifelse(
      (head_jobs > 1) | (spouse_jobs > 1),
      1, 0
    ),
    # Count jobs beyond each person's first job
    extra_jobs = pmax(head_jobs - 1, 0) +
      pmax(spouse_jobs - 1, 0),
    # Log-transform income and expenditure outcomes
    ln_income = log(income_lag + 1),
    ln_total = log(total_exp + 1),
    ln_essential = log(essential_exp + 1),
    ln_discretionary = log(discretionary_exp + 1),
    ln_total_pc = log(total_exp_pc + 1),
    ln_essential_pc = log(essential_exp_pc + 1),
    ln_discretionary_pc = log(discretionary_exp_pc + 1),
    
    income_bucket = case_when(
      is.na(income_lag) ~ NA_character_,
      income_lag < 30000 ~ "<30k",
      income_lag < 60000 ~ "30k-60k",
      income_lag < 100000 ~ "60k-100k",
      income_lag < 200000 ~ "100k-200k",
      TRUE ~ "200k+"
    ),
    income_bucket = factor(
      income_bucket,
      levels = c("<30k", "30k-60k", "60k-100k", "100k-200k", "200k+")
    )
  ) %>%
  group_by(interview_year) %>%
  # Construct within-wave income terciles
  mutate(income_group = ntile(income_lag, 3)) %>%
  ungroup()

# Define analysis sample and inspect key variables --------------------------

# Restrict to households with at least one employed head/spouse and complete
# income, total expenditure, and household-size information
reg_data_main <- psid_long %>%
  filter(
    head_jobs >= 1 | spouse_jobs >= 1,
    !is.na(income_lag),
    !is.na(total_exp),
    !is.na(fu_size),
    interview_year >= 2005
  )

summary(reg_data_main$jobs)
summary(reg_data_main$head_jobs)
summary(reg_data_main$spouse_jobs)
summary(reg_data_main$total_exp)
summary(reg_data_main$discretionary_exp)
summary(reg_data_main$essential_exp)

table(reg_data_main$interview_year)

# Quantile regression analysis ---------------------------------------------

# Use a common complete-case sample for all per-capita spending outcomes
qr_data <- reg_data_main %>%
  filter(
    !is.na(extra_jobs),
    !is.na(ln_income),
    !is.na(ln_total_pc),
    !is.na(ln_discretionary_pc),
    !is.na(ln_essential_pc),
    !is.na(fu_size),
    total_exp_pc > 0,
    discretionary_exp_pc > 0,
    essential_exp_pc > 0
  )
# Estimate associations at several points of the conditional spending distribution
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

qr_total <- lapply(taus, function(tau) {
  rq(
    ln_total_pc ~ extra_jobs + ln_income + fu_size + factor(interview_year),
    tau = tau,
    data = qr_data,
    method = "fn"
  )
})

qr_disc <- lapply(taus, function(tau) {
  rq(
    ln_discretionary_pc ~ extra_jobs + ln_income + fu_size + factor(interview_year),
    tau = tau,
    data = qr_data,
    method = "fn"
  )
})

qr_ess <- lapply(taus, function(tau) {
  rq(
    ln_essential_pc ~ extra_jobs + ln_income + fu_size + factor(interview_year),
    tau = tau,
    data = qr_data,
    method = "fn"
  )
})
# Extract the extra-jobs coefficient from each model for comparison across quantiles
qr_combined <- bind_rows(
  data.frame(
    tau = taus,
    estimate = sapply(qr_total, function(m) coef(m)["extra_jobs"]),
    category = "Total per capita"
  ),
  data.frame(
    tau = taus,
    estimate = sapply(qr_disc, function(m) coef(m)["extra_jobs"]),
    category = "Discretionary per capita"
  ),
  data.frame(
    tau = taus,
    estimate = sapply(qr_ess, function(m) coef(m)["extra_jobs"]),
    category = "Essential per capita"
  )
)
# Visualize how the association with extra jobs varies across spending quantiles
ggplot(qr_combined, aes(x = tau, y = estimate, color = category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Quantile Associations between Extra Jobs and Per-Capita Spending",
    x = "Spending Quantile",
    y = "Coefficient on Extra Jobs",
    color = "Category"
  ) +
  theme_minimal(base_size = 14)
# Report total-spending quantile regression estimates in a compact table
models <- list(
  "Q10" = qr_total[[1]],
  "Q25" = qr_total[[2]],
  "Q50" = qr_total[[3]],
  "Q75" = qr_total[[4]],
  "Q90" = qr_total[[5]]
)
modelsummary(
  models,
  coef_map = c(
    "extra_jobs" = "Extra Jobs",
    "ln_income" = "Log Income",
    "fu_size" = "Household Size"
  ),
  gof_omit = ".*"
)

# Discretionary spending share analysis -----------------------------------

# Construct discretionary spending as a share of total household expenditure
qr_share_data <- reg_data_main %>%
  filter(
    total_exp > 0,
    discretionary_exp >= 0,
    !is.na(extra_jobs),
    !is.na(ln_income),
    !is.na(fu_size)
  ) %>%
  mutate(
    disc_share = discretionary_exp / total_exp
  )
# Reuse the same quantiles to examine heterogeneity in spending shares
qr_share <- lapply(taus, function(tau) {
  rq(
    disc_share ~ extra_jobs + ln_income + fu_size + factor(interview_year),
    tau = tau,
    data = qr_share_data,
    method = "fn"
  )
})
# Organize models for reporting
models_share <- setNames(
  qr_share,
  paste0("Q", taus)
)

modelsummary(
  models_share,
  coef_map = c(
    "extra_jobs" = "Extra Jobs",
    "ln_income" = "Log Income",
    "fu_size" = "Household Size"
  ),
  gof_omit = ".*"
)

# Propensity score matching robustness ------------------------------------

# Match households with and without multiple job holding on observed covariates
psm_data <- reg_data_main %>%
  filter(!is.na(multijob), !is.na(income_lag), !is.na(fu_size))
m.out <- matchit(
  multijob ~ ln_income + fu_size + factor(interview_year),
  data = psm_data,
  method = "nearest"
)
matched_data <- match.data(m.out)
# Estimate post-matching associations for total, discretionary, and essential spending
lm_psm <- lm(
  ln_total ~ multijob + ln_income + fu_size,
  data = matched_data
)

summary(lm_psm)
lm_disc <- lm(
  ln_discretionary ~ multijob + ln_income + fu_size,
  data = matched_data
)

summary(lm_disc)
lm_ess <- lm(
  ln_essential ~ multijob + ln_income + fu_size,
  data = matched_data
)

summary(lm_ess)
