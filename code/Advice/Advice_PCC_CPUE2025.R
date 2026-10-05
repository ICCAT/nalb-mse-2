# ============================================================
# Script: Advice_PCC_CPUE2025.R
#
# Purpose:
#   Calculate TAC advice using the pseudo-constant catch
#   management procedure based on the joint CPUE index.
#
# Inputs:
#   - cpue_normalized_2025.xlsx
#
# Outputs:
#   - TAC advice
#
# Author: AZTI
# ============================================================

library(readxl)
library(dplyr)
library(zoo)
library(here)

project_dir <- here::here()
setwd(project_dir)

source("code/Advice/AdviceFunctions.R")

source('sharepoint_path.R')
setwd(shrpoint_path)

# Read CPUE data
cpue_data <- read_excel(
  "Data/cpue_normalized_2025.xlsx"
)

# Calculate joint index and indicators
cpue_data <- calculate_joint_index(cpue_data)

cpue_data <- calculate_advice_indicators(cpue_data)

# Management procedure settings
advice_year <- 2025

previous_tac <- 47251

reference_index <- 1.08

pseudo_constant_catch <- 42000

maximum_increase <- 0.15
maximum_decrease <- 0.15

# Extract indicator
reference_ratio <- cpue_data$Jratio_reference[
  cpue_data$Year == advice_year
]

# Calculate TAC
unconstrained_tac <- ifelse(
  reference_ratio > 1,
  pseudo_constant_catch,
  pseudo_constant_catch * reference_ratio
)

lower_bound <- previous_tac * (1 - maximum_decrease)
upper_bound <- previous_tac * (1 + maximum_increase)

advice_tac <- max(
  lower_bound,
  min(unconstrained_tac, upper_bound)
)

cat(
  "\nPseudo-Constant Catch MP\n",
  "Reference ratio: ", round(reference_ratio, 3), "\n",
  "Advice TAC: ", round(advice_tac), "\n",
  sep = ""
)
