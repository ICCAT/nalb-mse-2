# ============================================================
# Script: Advice_EMP_CPUE2025.R
#
# Purpose:
#   Calculate TAC advice using the empirical management
#   procedure based on the weighted joint CPUE index.
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

source("sharepoint_path.R")
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

maximum_increase <- 0.25
maximum_decrease <- 0.20

maximum_tac <- 50000

# Extract indicator
jratio_3over3 <- cpue_data$Jratio_3over3[
  cpue_data$Year == advice_year
]

# Apply empirical HCR
multiplier <- max(
  1 - maximum_decrease,
  min(
    jratio_3over3,
    1 + maximum_increase
  )
)

multiplier <- round(multiplier, 2)

advice_tac <- min(
  previous_tac * multiplier,
  maximum_tac
)

cat(
  "\nEmpirical MP\n",
  "3-over-3 ratio: ", round(jratio_3over3, 3), "\n",
  "Multiplier: ", multiplier, "\n",
  "Advice TAC: ", round(advice_tac), "\n",
  sep = ""
)
