# ============================================================
# Script: SPiCT_Assessment_CPUE2021.R
#
# Purpose:
#   Fit the Atlantic albacore SPiCT assessment using the
#   standardized CPUE indices available up to 2021.
#
# Inputs:
#   - Data/CPUE_inputs.xlsx (sheet PREV_normalized)
#   - Stock Synthesis catch data
#
# Outputs:
#   - SPiCT fitted model
#   - Diagnostic plots
#   - Assessment summary
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(FLBEIA)
library(spict)
library(readxl)
library(here)


# ============================================================
# 2. Set project and SharePoint directories
# ============================================================

project_dir <- here::here()
setwd(project_dir)

source("sharepoint_path.R")
setwd(shrpoint_path)


# ============================================================
# 3. Read catch data from Stock Synthesis
# ============================================================

ss_directory <- paste0(
  "Assessment/",
  "Assessment_2023/",
  "ALB_SS3_FinalVersionRecDev2018/",
  "v28_forecast3_relf_v5_Fmsy08_2018_v3"
)

ss_output <- SS_output(
  dir = ss_directory,
  verbose = FALSE
)

catch_data <- ss_output$catch

total_catch <- aggregate(
  Obs ~ Yr,
  data = catch_data,
  sum
)

catch_vector <- setNames(
  total_catch$Obs,
  total_catch$Yr
)


# ============================================================
# 4. Read CPUE indices
# ============================================================

cpue_data <- read_excel(
  "Data/CPUE_inputs.xlsx",
  sheet = "PREV_normalized",
  na = c("NA", "")
)

cpue_data <- subset(
  cpue_data,
  Year >= 1981 & Year <= 2021
)


# ============================================================
# 5. Define CPUE index mapping
# ============================================================

index_mapping <- c(
  BB       = "BB_prev",
  JP_LL_N  = "JP_LL_N_prev",
  JP_LL_S  = "JP_LL_S_prev",
  TAI_LL_N = "TAI_LL_N_prev",
  TAI_LL_S = "TAI_LL_S_prev",
  US_LL_N  = "US_LL_N_prev",
  US_LL_S  = "US_LL_S_prev",
  VEN_LL   = "VEN_LL"
)

cpue_data[index_mapping] <- lapply(
  cpue_data[index_mapping],
  function(x) as.numeric(as.character(x))
)


# ============================================================
# 6. Create SPiCT input object
# ============================================================

spict_input <- list()

spict_input$timeI <- list()
spict_input$obsI <- list()

for (index_name in names(index_mapping)) {
  
  column_name <- index_mapping[index_name]
  
  valid <- !is.na(cpue_data[[column_name]])
  
  spict_input$timeI[[index_name]] <- cpue_data$Year[valid]
  spict_input$obsI[[index_name]] <- cpue_data[[column_name]][valid]
  
}

spict_input$obsC <- as.numeric(catch_vector)[-1]
spict_input$timeC <- as.numeric(names(catch_vector)[-1])


# ============================================================
# 7. Define SPiCT model settings
# ============================================================

spict_input$dteuler <- 1 / 8
spict_input$getReportCovariance <- FALSE

# Priors
spict_input$priors$logbeta <- c(0, 0, 0)
spict_input$priors$logalpha <- c(0, 0, 0)

spict_input$priors$logbkfrac <- c(log(1),0.01^2)

spict_input$priors$logr <- c(log(0.4),0.5,1)

spict_input$priors$logK <- c(log(1.2e6), 0.5, 1)

# Initial values
spict_input$ini$logn <- log(1.001)
spict_input$ini$logsdc <- log(0.0001)

# Fixed parameters
spict_input$phases$logsdc <- -1
spict_input$phases$logn <- -1


# ============================================================
# 8. Define observation uncertainty
# ============================================================

spict_input$stdevfacI <- vector(
  "list",
  length(index_mapping)
)

for (i in seq_along(index_mapping)) {
  
  spict_input$stdevfacI[[i]] <- rep(
    0.2,
    length(spict_input$obsI[[i]])
  )
  
}


# ============================================================
# 9. Fit SPiCT model
# ============================================================

spict_output <- fit.spict(spict_input)


# ============================================================
# 10. Diagnostic plots
# ============================================================

plot(spict_output)

plotspict.diagnostic(
  spict_output
)


# ============================================================
# 11. Model summary
# ============================================================

summary(spict_output)


# ============================================================
# 12. Convergence checks
# ============================================================

spict_output$opt$convergence

all(
  is.finite(
    spict_output$sd
  )
)
