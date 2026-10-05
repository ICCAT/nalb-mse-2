# ============================================================
# Script: OEM_ResidualAutocorrelationAnalysis.R
#
# Purpose:
#   Analyse residual autocorrelation and partial
#   autocorrelation across OEM scenarios.
#
# Inputs:
#   - SS3 report files
#
# Outputs:
#   - ACF plots
#   - PACF plots
#   - AR parameter table
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(r4ss)
library(here)


# ============================================================
# 2. Set project directory
# ============================================================

project_dir <- here::here()
setwd(project_dir)

source("code/OEM/OEMFunctions.R")


source("sharepoint_path.R")
setwd(shrpoint_path)



# ============================================================
# 3. Define stock and fleet
# ============================================================

stock_code <- "ALB"
fleet_id <- 8


# ============================================================
# 4. Define directories
# ============================================================

output_directory <- "OEM/InputMSE_OEM"

plot_directory <- file.path(
  "OEM",
  "OEM_ResidualAnalysis"
)

dir.create(output_directory,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(plot_directory,
           recursive = TRUE,
           showWarnings = FALSE)


# ============================================================
# 5. Define scenarios
# ============================================================

scenario_names <- c(
  "BaseCase",
  "AGE",
  "CPUE",
  "SIZE"
)

scenario_directories <- paste0(
  "OM/",
  scenario_names,
  "/hess"
)

plot_colours <- rainbow(
  length(scenario_directories)
)


# ============================================================
# 6. Calculate ACF and AR parameters
# ============================================================

ar_summary <- data.frame()

png(
  file.path(
    plot_directory,
    paste0("ACF_Fl", fleet_id, "_ALL.png")
  ),
  width = 1000,
  height = 800
)

for (i in seq_along(scenario_directories)) {
  
  residual_data <- extract_residuals(
    scenario_directory = scenario_directories[i],
    fleet_id = fleet_id
  )
  
  acf_object <- acf(
    residual_data$residuals,
    plot = FALSE
  )
  
  if (i == 1) {
    
    plot(
      acf_object,
      ylim = c(-0.5, 1),
      type = "n",
      main = paste(
        stock_code,
        "CPUE",
        residual_data$fleet_name
      )
    )
    
  }
  
  lines(
    acf_object$acf[-1],
    col = plot_colours[i],
    lwd = 3,
    lty = 3
  )
  
  ar_parameters <- calculate_ar_parameters(
    residual_data$residuals
  )
  
  ar_summary <- rbind(
    ar_summary,
    data.frame(
      scenario = scenario_directories[i],
      rho = ar_parameters$rho,
      sigma = ar_parameters$sigma
    )
  )
  
}

dev.off()


# ============================================================
# 7. Save AR parameters
# ============================================================

write.csv(
  ar_summary,
  file.path(
    output_directory,
    paste0(
      "ARpar_Fl",
      fleet_id,
      "_ALL.csv"
    )
  ),
  row.names = FALSE
)