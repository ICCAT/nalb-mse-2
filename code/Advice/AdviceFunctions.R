# ============================================================
# Script: AdviceFunctions.R
#
# Purpose:
#   Functions used by advice scripts.
#
# Inputs:
#   - CPUE data
#
# Outputs:
#   - Joint index metrics
#
# Author: AZTI
# ============================================================


# ============================================================
# Calculate weighted mean ignoring NAs
# ============================================================

weighted_mean_na <- function(values, weights) {
  
  valid <- !is.na(values)
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  weighted.mean(values[valid], weights[valid])
  
}


# ============================================================
# Calculate index weights
# ============================================================

calculate_index_weights <- function() {
  
  index_sd <- c(0.38, 0.36, 0.29, 0.33, 0.39, 0.37)
  
  index_ac <- c(0.11, 0.39, 0.16, 0.56, 0.66, 0.59)
  
  weights <- round(
    1 / sqrt(index_sd / (1 - index_ac)),
    2
  )
  
  names(weights) <- c(
    "BB_new", "JP_LL_S_new", "TAI_LL_N_new",
    "TAI_LL_S_new", "US_LL_N_new", "US_LL_S_new"
  )
  
  weights
  
}


# ============================================================
# Calculate weighted joint index
# ============================================================

calculate_joint_index <- function(cpue_data) {
  
  weights <- calculate_index_weights()
  
  cpue_data$JointIndex <- apply(
    cpue_data[, names(weights)],
    1,
    function(x) weighted_mean_na(as.numeric(x), weights)
  )
  
  cpue_data
  
}


# ============================================================
# Calculate advice indicators
# ============================================================

calculate_advice_indicators <- function(
    cpue_data,
    joint_index_reference = 1.08
) {
  
  cpue_data$JointIndex_3yr <- zoo::rollapply(
    cpue_data$JointIndex,
    width = 3,
    FUN = mean,
    align = "right",
    fill = NA
  )
  
  cpue_data$JointIndex_3yr[cpue_data$Year < 2001] <- NA
  
  cpue_data$Jratio_reference <- round(
    cpue_data$JointIndex_3yr / joint_index_reference,
    3
  )
  
  cpue_data <- cpue_data %>%
    dplyr::arrange(Year) %>%
    dplyr::mutate(
      
      Mean_last3 =
        (JointIndex +
           dplyr::lag(JointIndex, 1) +
           dplyr::lag(JointIndex, 2)) / 3,
      
      Mean_previous3 =
        (dplyr::lag(JointIndex, 3) +
           dplyr::lag(JointIndex, 4) +
           dplyr::lag(JointIndex, 5)) / 3,
      
      Jratio_3over3 = ifelse(
        Year >= 2003,
        Mean_last3 / Mean_previous3,
        NA_real_
      )
      
    )
  
  cpue_data
  
}
