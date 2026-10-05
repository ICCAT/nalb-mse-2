# ============================================================
# Script: OEM_VulnerableBiomassAnalysis.R
#
# Purpose:
#   Estimate vulnerable biomass for a selected CPUE fleet
#   and compare vulnerable biomass, CPUE and SSB indicators.
#
# Inputs:
#   - SS3 Report.sso
#
# Outputs:
#   - Vulnerable biomass time series
#   - Vulnerable biomass diagnostics
#   - CPUE diagnostic plots
#   - SSB diagnostic plots
#   - Maturity plots
#   - Selectivity plots
#
# Author: AZTI
# ============================================================

# ============================================================
# 1. Load packages
# ============================================================

library(r4ss)
library(here)

project_dir <- here::here()
setwd(project_dir)

source("code/OEM/OEMFunctions.R")


source("sharepoint_path.R")
setwd(shrpoint_path)

# ============================================================
# 2. Define stock, fleet and scenario
# ============================================================

stock_code <- "ALB"

fleet_id <- 8

scenario_name <- "BaseCase"

survey_cpue <- FALSE


# ============================================================
# 3. Define directories
# ============================================================

output_directory <- file.path(
  "OEM","Others"
)

scenario_output_directory <- file.path(
  output_directory,
  scenario_name
)

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

if (!dir.exists(scenario_output_directory)) {
  dir.create(
    scenario_output_directory,
    recursive = TRUE
  )
}
# ============================================================
# 4. Read SS3 output
# ============================================================

ss3_directory <- file.path(
  "OM",
  scenario_name,
  "hess"
)

ss3_output <- SS_output(
  dir = ss3_directory,
  repfile = "Report.sso",
  covar = FALSE
)

cpue_data <- ss3_output$cpue[
  ss3_output$cpue$Fleet == fleet_id,
]

# ============================================================
# 5. Estimate vulnerable biomass
# ============================================================

vulnerable_biomass_df <- NULL

for (year_id in seq_along(cpue_data$Yr)) {
  
  assessment_year <- unique(cpue_data$Yr)[year_id]
  
  selectivity <- ss3_output$ageselex[
    ss3_output$ageselex$Fleet == fleet_id &
      ss3_output$ageselex$Factor == "Asel2" &
      ss3_output$ageselex$Yr == assessment_year,
  ][, -c(1:7)]
  
  weight_at_age <- ss3_output$ageselex[
    ss3_output$ageselex$Fleet == fleet_id &
      ss3_output$ageselex$Factor == "bodywt" &
      ss3_output$ageselex$Yr == assessment_year,
  ][, -c(1:7)]
  
  numbers_at_age <- ss3_output$natage[
    ss3_output$natage$Yr == assessment_year &
      ss3_output$natage$`Beg/Mid` == "M",
  ][, -c(1:12)]
  
  if (survey_cpue) {
    
    assessment_year <- cpue_data$Yr[year_id]
    
    season <- cpue_data$Seas[year_id]
    
    selectivity <- ss3_output$ageselex[
      ss3_output$ageselex$Fleet == fleet_id &
        ss3_output$ageselex$Factor == "Asel2" &
        ss3_output$ageselex$Yr == assessment_year &
        ss3_output$ageselex$Seas == season,
    ][, -c(1:7)]
    
    weight_at_age <- ss3_output$ageselex[
      ss3_output$ageselex$Fleet == fleet_id &
        ss3_output$ageselex$Factor == "bodywt" &
        ss3_output$ageselex$Yr == assessment_year &
        ss3_output$ageselex$Seas == season,
    ][, -c(1:7)]
    
    numbers_at_age <- ss3_output$natage[
      ss3_output$natage$Yr == assessment_year &
        ss3_output$natage$Seas == season &
        ss3_output$natage$`Beg/Mid` == "M",
    ][, -c(1:12)]
    
  }
  
  vulnerable_biomass <- sum(
    numbers_at_age[1, ] *
      weight_at_age[1, ] *
      selectivity[1, ]
  )
  
  vulnerable_biomass_df <- rbind(
    vulnerable_biomass_df,
    c(
      assessment_year,
      vulnerable_biomass
    )
  )
  
}


vulnerable_biomass_df <- as.data.frame(
  vulnerable_biomass_df
)

names(vulnerable_biomass_df) <- c(
  "Year",
  "VulnerableBiomass"
)

vulnerable_biomass_df$VulnerableBiomass <- as.numeric(
  vulnerable_biomass_df$VulnerableBiomass
)


# ============================================================
# 6. Save vulnerable biomass estimates
# ============================================================

write.csv(
  vulnerable_biomass_df,
  file = file.path(
    scenario_output_directory,
    paste0(
      "VulnBio_Fl",
      fleet_id,
      "_",
      scenario_name,
      ".csv"
    )
  ),
  row.names = FALSE
)


# ============================================================
# 7. Plot vulnerable biomass versus expected CPUE
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "VulnBio_LR_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800
)

cex_value <- 1.5

par(
  cex.lab = cex_value,
  cex.axis = cex_value,
  cex.main = cex_value
)

plot(
  log(cpue_data$Exp),
  log(vulnerable_biomass_df$VulnerableBiomass),
  xlab = "Log(Expected CPUE)",
  ylab = "",
  main = paste(
    stock_code,
    "Vulnerable biomass vs expected CPUE",
    ss3_output$FleetNames[fleet_id]
  )
)

lm_cpue_vb <- lm(
  log(vulnerable_biomass_df$VulnerableBiomass) ~
    log(cpue_data$Exp)
)

abline(
  lm_cpue_vb,
  col = 1
)

mtext(
  "Log(Vulnerable Biomass)",
  side = 2,
  line = 2.5,
  cex = 1.7
)

dev.off()

# ============================================================
# 8. Plot maturity
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    "Maturity.png"
  ),
  width = 6,
  height = 6,
  units = "cm",
  res = 300,
  pointsize = 6
)

SSplotBiology(
  ss3_output,
  subplots = 6
)

dev.off()


# ============================================================
# 9. Plot selectivity
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "SelectivityFl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 140
)

SSplotSelex(
  ss3_output,
  fleets = fleet_id,
  subplots = 2
)

dev.off()


png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "SelectivityFl",
      fleet_id,
      "_all.png"
    )
  ),
  width = 800,
  height = 800,
  res = 140
)

SSplotSelex(
  ss3_output,
  fleets = c(1, 5, 6, 7, 8, 9, 10),
  subplots = 2,
  mainTitle = FALSE
)

dev.off()


# ============================================================
# 10. Plot expected CPUE
# ============================================================

expected_cpue <- cpue_data$Exp

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "EXP_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 140
)

plot(
  cpue_data$Yr,
  expected_cpue,
  xlab = "Year",
  ylab = paste0(
    "Expected CPUE (",
    ss3_output$FleetNames[fleet_id],
    ")"
  )
)

dev.off()


# ============================================================
# 11. Plot observed CPUE
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "ObsTS_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 140
)

plot(
  cpue_data$Yr,
  log(cpue_data$Obs),
  xlab = "Year",
  ylab = paste0(
    "Log(Observed CPUE - ",
    ss3_output$FleetNames[fleet_id],
    ")"
  )
)

dev.off()


# ============================================================
# 12. Plot observed versus expected CPUE
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "LR_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 150
)

plot(
  log(cpue_data$Obs),
  log(expected_cpue),
  xlab = paste0(
    "Log(Observed CPUE - ",
    ss3_output$FleetNames[fleet_id],
    ")"
  ),
  ylab = paste0(
    "Log(Expected CPUE - ",
    ss3_output$FleetNames[fleet_id],
    ")"
  )
)

lm_cpue <- lm(
  log(expected_cpue) ~ log(cpue_data$Obs)
)

abline(
  lm_cpue,
  col = 1
)

dev.off()


# ============================================================
# 13. Plot expected CPUE versus SSB
# ============================================================

ssb_values <- ss3_output$derived_quants$Value[
  grep(
    "SSB_",
    ss3_output$derived_quants$Label
  )
][ -c(1, 2) ]

ssb_years <- sapply(
  strsplit(
    ss3_output$derived_quants$Label[
      grep(
        "SSB_",
        ss3_output$derived_quants$Label
      )
    ],
    "_"
  ),
  tail,
  1
)

year_index <- match(
  cpue_data$Yr,
  as.numeric(ssb_years)
)

ssb_years <- as.numeric(
  ssb_years[year_index]
)

ssb_values <- ssb_values[year_index]

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "SSB_LR_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 150
)

cex_value <- 1.5

par(
  cex.lab = cex_value,
  cex.axis = cex_value,
  cex.main = cex_value
)

plot(
  log(expected_cpue),
  log(ssb_values),
  xlab = "Log(Expected CPUE)",
  ylab = "",
  main = paste0(
    stock_code,
    " SSB vs Expected CPUE - ",
    ss3_output$FleetNames[fleet_id]
  )
)

lm_cpue_ssb <- lm(
  log(ssb_values) ~ log(expected_cpue)
)

abline(
  lm_cpue_ssb,
  col = 1
)

mtext(
  "Log(SSB)",
  side = 2,
  line = 2.5,
  cex = 1.7
)

dev.off()

# ============================================================
# 14. Calculate residual diagnostics
# ============================================================

residual_output <- extract_residuals(
  scenario_directory = ss3_directory,
  fleet_id = fleet_id
)

residuals <- residual_output$residuals

residual_df <- residual_output$residual_df


# Check time series continuity
if (survey_cpue) {
  
  years <- residual_df$Year
  
  time_step <- 0.25
  
} else {
  
  years <- residual_df$residual_df$Year
  
  time_step <- 1
  
}

all_years <- seq(
  min(years),
  max(years),
  time_step
)

missing_years <- setdiff(
  all_years,
  years
)

print(missing_years)


# ============================================================
# 15. Plot residual time series
# ============================================================

png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "Residuals_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 150
)

plot(
  residual_df$residual_df$Year,
  residual_df$residual_df$Residual,
  xlab = "Year",
  ylab = "Historical residuals"
)

dev.off()

# ============================================================
# 16. Plot ACF and PACF
# ============================================================

acf_results <- acf(
  residuals,
  plot = FALSE
)

pacf_results <- pacf(
  residuals,
  plot = FALSE
)


png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "ACF_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 150
)

plot(
  acf_results,
  ylim = c(-0.5, 1),
  main = paste0(
    stock_code,
    " ",
    scenario_name,
    " ACF"
  )
)

dev.off()


png(
  filename = file.path(
    scenario_output_directory,
    paste0(
      "PACF_Fl",
      fleet_id,
      ".png"
    )
  ),
  width = 800,
  height = 800,
  res = 150
)

plot(
  pacf_results,
  ylim = c(-0.5, 1),
  main = paste0(
    stock_code,
    " ",
    scenario_name,
    " PACF"
  )
)

dev.off()


# ============================================================
# 17. Calculate AR(1) parameters
# ============================================================

ar_parameters <- calculate_ar_parameters(
  residuals
)

rho <- ar_parameters$rho

sigma <- ar_parameters$sigma

print(rho)
print(sigma)


# ============================================================
# 18. Save residual diagnostics
# ============================================================
save(
  rho,
  sigma,
  lm_cpue_ssb,
  lm_cpue_vb,
  file = file.path(
    output_directory,
    paste0(
      "LR_RandRes_Fl",
      fleet_id,
      "_",
      scenario_name,
      ".RData"
    )
  )
)

ar_summary <- data.frame(
  rho = rho,
  sigma = sigma,
  scenario = scenario_name,
  lag = 1
)

write.csv(
  ar_summary,
  file = file.path(
    output_directory,
    paste0(
      "ARpar_Fl",
      fleet_id,
      "_",
      scenario_name,
      ".csv"
    )
  ),
  row.names = FALSE
)