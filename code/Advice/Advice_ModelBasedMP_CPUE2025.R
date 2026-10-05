# ============================================================
# Script: Advice_ModelBasedMP_CPUE2025.R
#
# Purpose:
#   Fit the North Atlantic albacore SPiCT assessment using catch data
#   and standardised CPUE indices updated to 2025, and calculate
#   model-based management advice.
#
# Inputs:
#   - Data/cpue_normalized_2025.xlsx
#   - Stock Synthesis output files
#
# Outputs:
#   - SPiCT fitted model
#   - Diagnostic plots
#   - TAC advice
#
# Author: AZTI
# ============================================================

# ============================================================
# 1. Load packages
# ============================================================

library(readxl)
library(dplyr)
library(here)
library(spict)
library(r4ss)


# ============================================================
# 2. Set project and SharePoint directories
# ============================================================

project_directory <- here::here()
setwd(project_directory)

source("sharepoint_path.R")

if (!exists("shrpoint_path")) {
  stop(
    "Object 'shrpoint_path' was not created by sharepoint_path.R."
  )
}

if (!dir.exists(shrpoint_path)) {
  stop(
    "SharePoint directory not found: ",
    shrpoint_path
  )
}

setwd(shrpoint_path)


# ============================================================
# 3. Read catch data from Stock Synthesis
# ============================================================

ss_directory <- paste0(
  "D:/AZTI/ALB - General/Assessment/Assessment_2023/",
  "ALB_SS3_FinalVersionRecDev2018/",
  "v28_forecast3_relf_v5_Fmsy08_2018_v3"
)

if (!dir.exists(ss_directory)) {
  stop(
    "Stock Synthesis directory not found: ",
    ss_directory
  )
}

ss_output <- r4ss::SS_output(
  dir = ss_directory,
  verbose = FALSE
)

catch_data <- ss_output$catch

total_catch <- aggregate(
  Obs ~ Yr,
  data = catch_data,
  FUN = sum
)

catch_vector <- setNames(
  total_catch$Obs,
  total_catch$Yr
)


# ============================================================
# 4. Read standardised CPUE indices
# ============================================================

cpue_file <- "Data/cpue_normalized_2025.xlsx"

if (!file.exists(cpue_file)) {
  stop(
    "CPUE input file not found: ",
    cpue_file
  )
}

normalised_cpue <- read_excel(
  path = cpue_file,
  na = "NA"
)

if (!"Year" %in% names(normalised_cpue)) {
  stop(
    "The CPUE input file must contain a column named 'Year'."
  )
}

cpue_columns <- setdiff(
  names(normalised_cpue),
  "Year"
)

index_names <- c(
  "BB",
  "JP_LL_N",
  "JP_LL_S",
  "TAI_LL_N",
  "TAI_LL_S",
  "US_LL_N",
  "US_LL_S",
  "VEN_LL"
)

if (length(cpue_columns) != length(index_names)) {
  stop(
    "The number of CPUE columns does not match the number ",
    "of index names. CPUE columns: ",
    length(cpue_columns),
    "; index names: ",
    length(index_names),
    "."
  )
}


# ============================================================
# 5. Extend catch data to 2025
# ============================================================

additional_catches <- c(
  31601,
  28115,
  23800,
  25000
)

additional_catch_years <- 2022:2025

if (length(additional_catches) != length(additional_catch_years)) {
  stop(
    "The number of additional catches must match the number ",
    "of additional catch years."
  )
}


# ============================================================
# 6. Assemble the SPiCT input object
# ============================================================

spict_input <- list()

# Combine historical catches with additional catches for 2022-2025
spict_input$timeC <- c(
  as.numeric(names(catch_vector)[-1]),
  additional_catch_years
)

spict_input$obsC <- c(
  as.numeric(catch_vector)[-1],
  additional_catches
)

# Create one time series for each CPUE index
spict_input$timeI <- vector(
  mode = "list",
  length = length(cpue_columns)
)

spict_input$obsI <- vector(
  mode = "list",
  length = length(cpue_columns)
)

for (index_id in seq_along(cpue_columns)) {
  
  index_data <- normalised_cpue %>%
    select(
      Year,
      all_of(cpue_columns[index_id])
    ) %>%
    rename(
      Value = all_of(cpue_columns[index_id])
    ) %>%
    filter(
      !is.na(Value)
    )
  
  spict_input$timeI[[index_id]] <- index_data$Year
  spict_input$obsI[[index_id]] <- index_data$Value
}

names(spict_input$timeI) <- index_names
names(spict_input$obsI) <- index_names


# ============================================================
# 7. Apply SPiCT model assumptions
# ============================================================

# ------------------------------------------------------------
# 7.1 Priors
# ------------------------------------------------------------

# Remove the default beta and alpha priors
spict_input$priors$logbeta <- c( 0, 0, 0)

spict_input$priors$logalpha <- c(0,  0,  0)

# Initial biomass prior:
# B0 / K approximately equal to 1, with a narrow uncertainty
spict_input$priors$logbkfrac <- c(log(1),0.01^2)

# Intrinsic growth rate prior:
# r approximately equal to 0.4
spict_input$priors$logr <- c(log(0.4),0.5, 1)

# Carrying capacity prior:
# K approximately equal to 1.2 million tonnes
spict_input$priors$logK <- c(log(1.2e6),0.5, 1)


# ------------------------------------------------------------
# 7.2 Initial values
# ------------------------------------------------------------

# Assume catch data have negligible observation error
spict_input$ini$logsdc <- log(0.0001)

# Initialise the production-function shape parameter
spict_input$ini$logn <- log(1.001)


# ------------------------------------------------------------
# 7.3 Estimation phases
# ------------------------------------------------------------

# Fix catch uncertainty
spict_input$phases$logsdc <- -1

# Fix the production-function shape parameter
spict_input$phases$logn <- -1


# ------------------------------------------------------------
# 7.4 CPUE observation uncertainty
# ------------------------------------------------------------

cpue_cv <- 0.2

spict_input$stdevfacI <- vector(
  mode = "list",
  length = length(cpue_columns)
)

for (index_id in seq_along(cpue_columns)) {
  
  spict_input$stdevfacI[[index_id]] <- rep(
    cpue_cv,
    length(spict_input$obsI[[index_id]])
  )
}

names(spict_input$stdevfacI) <- index_names


# ------------------------------------------------------------
# 7.5 Numerical settings
# ------------------------------------------------------------

# Euler integration step
spict_input$dteuler <- 1 / 8

# Skip report covariance calculation to reduce computation time
spict_input$getReportCovariance <- FALSE


# ============================================================
# 8. Verify the assembled SPiCT input
# ============================================================

cat(
  "Catch time range: ",
  min(spict_input$timeC),
  " - ",
  max(spict_input$timeC),
  "\n",
  sep = ""
)

cat(
  "Number of indices: ",
  length(spict_input$timeI),
  "\n\n",
  sep = ""
)

# Display the six most recent catch observations
recent_catches <- tail(
  data.frame(
    Year = spict_input$timeC,
    Catch = spict_input$obsC
  ),
  6
)

print(recent_catches)

# Display a summary of each CPUE index
cat("\nIndex summary:\n")

for (index_id in seq_along(index_names)) {
  
  cat(
    sprintf(
      "  %d. %-15s %d observations | %d-%d\n",
      index_id,
      index_names[index_id],
      length(spict_input$obsI[[index_id]]),
      min(spict_input$timeI[[index_id]]),
      max(spict_input$timeI[[index_id]])
    )
  )
}


# ============================================================
# 9. Plot the input data
# ============================================================

plotspict.data(spict_input)


# ============================================================
# 10. Fit the SPiCT model
# ============================================================

spict_output <- fit.spict(spict_input)

summary(spict_output)
plot(spict_output)


# ============================================================
# 11. Extract terminal-year estimates and reference points
# ============================================================

terminal_time <- 2025.875

fishing_mortality <- get.par(
  "logF",
  spict_output,
  exp = TRUE
)[as.character(terminal_time), "est"]

biomass <- get.par(
  "logB",
  spict_output,
  exp = TRUE
)[as.character(terminal_time), "est"]

f_msy <- get.par(
  "logFmsy",
  spict_output,
  exp = TRUE
)[, "est"]

b_msy <- get.par(
  "logBmsy",
  spict_output,
  exp = TRUE
)[, "est"]

cat(
  "\nTerminal fishing mortality: ",
  fishing_mortality,
  "\n",
  sep = ""
)

cat(
  "Terminal biomass: ",
  biomass,
  "\n",
  sep = ""
)

cat(
  "FMSY: ",
  f_msy,
  "\n",
  sep = ""
)

cat(
  "BMSY: ",
  b_msy,
  "\n\n",
  sep = ""
)


# ============================================================
# 12. Define the harvest control rule
# ============================================================

target_fishing_mortality <- 0.8 * f_msy
minimum_fishing_mortality <- 0.1 * f_msy

trigger_biomass <- 1.0 * b_msy
limit_biomass <- 0.4 * b_msy

maximum_tac_increase <- 0.25
maximum_tac_decrease <- 0.20

maximum_tac <- 50000
previous_tac <- 47251

assessment_biomass <- biomass


# ============================================================
# 13. Calculate unconstrained TAC advice
# ============================================================

biomass_region <- findInterval(
  assessment_biomass,
  c(
    limit_biomass,
    trigger_biomass
  )
)

hcr_intercept <- (
  target_fishing_mortality / f_msy
) - (
  (
    (target_fishing_mortality - minimum_fishing_mortality) /
      f_msy
  ) /
    (
      (trigger_biomass - limit_biomass) /
        b_msy
    )
) * (
  trigger_biomass / b_msy
)

hcr_slope <- (
  (
    target_fishing_mortality - minimum_fishing_mortality
  ) /
    f_msy
) / (
  (
    trigger_biomass - limit_biomass
  ) /
    b_msy
)

unconstrained_tac <- ifelse(
  biomass_region == 0,
  assessment_biomass * minimum_fishing_mortality,
  ifelse(
    biomass_region == 1,
    (
      hcr_intercept +
        hcr_slope * assessment_biomass / b_msy
    ) *
      f_msy *
      assessment_biomass,
    assessment_biomass * target_fishing_mortality
  )
)

cat(
  "Unconstrained TAC: ",
  unconstrained_tac,
  "\n",
  sep = ""
)


# ============================================================
# 14. Apply interannual TAC constraints
# ============================================================

if (
  length(unconstrained_tac) != 1 ||
  is.na(unconstrained_tac) ||
  !is.finite(unconstrained_tac)
) {
  
  warning(
    "The unconstrained TAC is missing or invalid. ",
    "Final TAC advice has been set to zero."
  )
  
  advice_tac <- 0
  
} else if (unconstrained_tac <= 0) {
  
  warning(
    "The unconstrained TAC is zero or negative. ",
    "Final TAC advice has been set to zero."
  )
  
  advice_tac <- 0
  
} else {
  
  relative_tac_change <- (
    unconstrained_tac - previous_tac
  ) / previous_tac
  
  constrained_tac <- unconstrained_tac
  
  if (relative_tac_change > maximum_tac_increase) {
    
    constrained_tac <- previous_tac * (
      1 + maximum_tac_increase
    )
  }
  
  if (relative_tac_change < -maximum_tac_decrease) {
    
    constrained_tac <- previous_tac * (
      1 - maximum_tac_decrease
    )
  }
  
  advice_tac <- min(
    constrained_tac,
    maximum_tac
  )
}


# ============================================================
# 15. Report final TAC advice
# ============================================================

cat(
  "Previous TAC: ",
  previous_tac,
  "\n",
  sep = ""
)

cat(
  "Maximum TAC: ",
  maximum_tac,
  "\n",
  sep = ""
)

cat(
  "Final TAC advice: ",
  advice_tac,
  "\n",
  sep = ""
)
