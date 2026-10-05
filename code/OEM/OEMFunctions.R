# ============================================================
# Script: OEMFunctions.R
#
# Purpose:
#   Functions used for OEM residual analysis.
#
# Inputs:
#   - SS3 output folders
#
# Outputs:
#   - Residual time series
#   - ACF/PACF inputs
#   - AR(1) parameters
#
# Author: AZTI
# ============================================================


# ============================================================
# Extract residuals from one fleet
# ============================================================

extract_residuals <- function(
    scenario_directory,
    fleet_id
) {
  
  ss3_output <- SS_output(
    dir = scenario_directory,
    repfile = "Report.sso",
    covar = FALSE
  )
  
  cpue_data <- ss3_output$cpue[
    ss3_output$cpue$Fleet == fleet_id,
  ]
  
  residuals <- log(cpue_data$Obs) -
    log(cpue_data$Exp)
  
  residual_df <- data.frame(
    Year = cpue_data$Yr,
    Residual = residuals
  )
  
  list(
    residuals = residuals,
    residual_df = residual_df,
    fleet_name = ss3_output$FleetNames[fleet_id]
  )
  
}


# ============================================================
# Calculate AR(1) parameters
# ============================================================

calculate_ar_parameters <- function(residuals) {
  
  n_years <- length(residuals)
  
  residual_t <- residuals[-n_years]
  residual_t_minus1 <- residuals[-1]
  
  rho <- sum(
    residual_t * residual_t_minus1
  ) / sum(
    residuals^2
  )
  
  sigma <- sqrt(
    sum(
      (residuals - mean(residuals))^2
    ) / (n_years - 1)
  )
  
  data.frame(
    rho = rho,
    sigma = sigma
  )
  
}
