# ============================================================
# Script: OEM_ProjectIndices.R
#
# Purpose:
#   Project CPUE indices including OEM residual error and
#   compare projected FLBEIA indices against SS3 expected
#   indices.
#
# Inputs:
#   - SS3 output files
#   - FLinput_run_<n>.RData
#   - OEM residual projections
#
# Outputs:
#   - Projected FLIndex objects
#   - CPUE comparison figures
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(FLXSA)
library(FLAssess)
library(FLash)
library(FLCore)
library(FLFleet)
library(FLBEIA)
library(ss3om)
library(here)


# ============================================================
# 2. Set project directory
# ============================================================

project_dir <- here::here()
setwd(project_dir)

source("code/Others/VPNInd.R")
source("code/Others/VPBInd.R")

source("sharepoint_path.R")
setwd(shrpoint_path)

# ============================================================
# 3. Define run and scenario settings
# ============================================================

run_id <- 1

scenario_names <- c(
  "BaseCase",
  "AGE",
  "CPUE",
  "SIZE"
)

scenario_directories <- c(
  "OM/BaseCase",
  "OM/Age/hess",
  "OM/CPUE/hess",
  "OM/Size/hess"
)

scenario_id <- 1

run_name <- "TAC"

first_year <- 1930
projection_year <- 2022
last_year <- 2050

n_projection_years <- length(
  projection_year:last_year
)


# ============================================================
# 4. Define input and output directories
# ============================================================

input_directory <- "FLinput/Others/NotHistErrorCPUE_old"

output_directory <- "FLinput/Others/HistError_AR_CPUE_old"

results_directory <- "OEM/ProjectedIndices"

plot_directory <- "OEM/ProjectedIndices"


# ============================================================
# 5. Load FLBEIA inputs
# ============================================================

load(
  file.path(
    input_directory,
    paste0(
      "FLinput_run_",
      run_id,
      ".RData"
    )
  )
)

# ============================================================
# 6. Read SS3 outputs
# ============================================================

ss3_output <- readOutputss3(
  scenario_directories[scenario_id],
  repfile = paste0(
    "Report_",
    run_id,
    ".sso"
  ),
  compfile = paste0(
    "CompReport_",
    run_id,
    ".sso"
  )
)

albacore_indices <- readFLIBss3(
  scenario_directories[scenario_id],
  repfile = paste0(
    "Report_",
    run_id,
    ".sso"
  ),
  compfile = paste0(
    "CompReport_",
    run_id,
    ".sso"
  )
)

names(albacore_indices) <- c(
  "BB",
  "JPLLN",
  "JPLLS",
  "TAILLN",
  "TAILLS",
  "USLLN",
  "USLLS",
  "VENLL"
)

# ============================================================
# 7. Create historical FLIndex objects
# ============================================================

historical_indices <- list(
  ALB = list(
    BB     = albacore_indices$BB,
    JPLLN  = albacore_indices$JPLLN,
    JPLLS  = albacore_indices$JPLLS,
    TAILLN = albacore_indices$TAILLN,
    TAILLS = albacore_indices$TAILLS,
    USLLN  = albacore_indices$USLLN,
    USLLS  = albacore_indices$USLLS,
    VENLL  = albacore_indices$VENLL
  )
)

indices <- historical_indices


fleet_ids <- c(1, 5:11)

survey_months <- c(8.5,11.5, 11.5,2.5,8.5,9,11.5,2.5, 6.5)

mid_year_fraction <-  0.5

weight_ratio <- (
  ss3_output$endgrowth$Wt_Mid[1:16] /
    ss3_output$endgrowth$Wt_Beg[1:16]
)


# ============================================================
# 8. Extend q and selectivity to projection years
# ============================================================

projection_fleet_ids <- c(1, 5:11)

for (index_id in seq_along(names(albacore_indices))) {
  
  indices$ALB[[index_id]] <- window(
    indices$ALB[[index_id]],
    start = indices$ALB[[index_id]]@range["minyear"],
    end = last_year,
    extend = TRUE
  )
  
  # Selection pattern in FLIndex is scaled by the annual maximum.
  # Replace it with the raw SS3 selectivity values.
  
  age_selectivity <- ss3_output$ageselex
  
  historical_years <- dimnames(
    historical_indices$ALB[[index_id]]@sel.pattern)$year
  
  for (year in historical_years) {
    
    ss3_selectivity <- age_selectivity[
      age_selectivity$Factor == "Asel2" &
        age_selectivity$Fleet == projection_fleet_ids[index_id] &
        age_selectivity$Yr == as.numeric(year),
      -c(1:7) ]
    
    indices$ALB[[index_id]]@sel.pattern[, year] <-
      as.numeric(ss3_selectivity)
  }
  
  indices$ALB[[index_id]]@catch.wt[] <-
    ss3_output$endgrowth$Wt_Mid[1:16]
  
  # Extend q and selectivity into projection years
  indices$ALB[[index_id]]@index.q[,
                                  as.character(projection_year:last_year)] <- yearMeans(
                                    indices$ALB[[index_id]]@index.q[ ,
                                                                     as.character(tail(historical_years, 3)) ])
  
  indices$ALB[[index_id]]@sel.pattern[ ,
                                       as.character(projection_year:last_year)] <- yearMeans(
                                         indices$ALB[[index_id]]@sel.pattern[ ,
                                                                              as.character(tail(historical_years, 3))] )
  
  # Add 2021 if missing
  if (!("2021" %in% historical_years)) {
    
    indices$ALB[[index_id]]@index.q[ ,"2021"] <- yearMeans(
      indices$ALB[[index_id]]@index.q[ ,
                                       as.character(tail(historical_years, 3))] )
    
    indices$ALB[[index_id]]@sel.pattern[,"2021" ] <- yearMeans(
      indices$ALB[[index_id]]@sel.pattern[ ,
                                           as.character(tail(historical_years, 3))] )
    
  }
  
}
# ============================================================
# 9. Calculate mid-year abundance
# ============================================================
# ============================================================
# 9. Calculate mid-year abundance
# ============================================================

mid_year_abundance <- (biols[[1]]@n[,
                                    as.character(first_year:(projection_year - 1))] *
                         exp( -biols[[1]]@m[,
                                            as.character(first_year:(projection_year - 1))
                         ] *mid_year_fraction[1])) -
  landStock(fleets, "ALB")[,
                           as.character(first_year:(projection_year - 1))] *
  mid_year_fraction

# ============================================================
# 10. Add OEM residual error to BB index
# ============================================================

load(paste0( "OEM/InputMSE_OEM/ProjRes/",
             "Rand_And_ResidualsAR_Fl1_",
             scenario_names[scenario_id],
             ".RData"))

# ============================================================
# 11. Add OEM residual error to number-based indices
# ============================================================

# ============================================================
# 11. Add OEM residual error to number-based indices
# ============================================================

number_based_indices <- c(2, 3, 6, 7, 8)

number_based_fleets <- c(5, 6, 9, 10, 11)

for (index_id in seq_along(number_based_indices)) {
  
  historical_years <- dimnames(
    albacore_indices[[number_based_indices[index_id]]])$year
  
  load(paste0(
    "OEM/InputMSE_OEM/ProjRes/",
    "Rand_And_ResidualsAR_Fl",
    number_based_fleets[index_id],
    "_",
    scenario_names[scenario_id],
    ".RData"
  )
  )
  
  indices$ALB[[number_based_indices[index_id]]]@index.q[,
                                                        historical_years ] <- exp(log(
                                                          albacore_indices[[number_based_indices[index_id]]]@index.q[,
                                                                                                                     historical_years]) )
  
  indices$ALB[[number_based_indices[index_id]]]@index.q[
    ,
    as.character(projection_year:last_year)
  ] <- exp(
    log(
      indices$ALB[[number_based_indices[index_id]]]@index.q[
        ,
        as.character(projection_year:last_year)
      ]
    ) +
      resProj[
        run_id,
        (length(historical_years) + 1):
          (n_projection_years + length(historical_years))
      ]
  )
  
  indices$ALB[[number_based_indices[index_id]]]@index[,
                                                      historical_years ] <-
    quantSums( mid_year_abundance[, historical_years] *
                 indices$ALB[[number_based_indices[index_id]]]@sel.pattern[ ,
                                                                            historical_years ] ) *
    indices$ALB[[number_based_indices[index_id]]]@index.q[, historical_years ]
  
}

# ============================================================
# 12. Add OEM residual error to biomass-based indices
# ============================================================

biomass_based_indices <- c(4, 5)

biomass_based_fleets <- c(7, 8)

for (index_id in seq_along(biomass_based_indices)) {
  
  historical_years <- dimnames(albacore_indices[[biomass_based_indices[index_id]]])$year
  
  load( paste0(
    "OEM/InputMSE_OEM/ProjRes/",
    "Rand_And_ResidualsAR_Fl",
    biomass_based_fleets[index_id],
    "_",
    scenario_names[scenario_id],
    ".RData"
  )
  )
  
  indices$ALB[[biomass_based_indices[index_id]]]@index.q[,
                                                         historical_years] <- exp(
                                                           log(
                                                             albacore_indices[[biomass_based_indices[index_id]]]@index.q[ ,
                                                                                                                          historical_years] ) )
  
  indices$ALB[[biomass_based_indices[index_id]]]@index.q[,
                                                         as.character(projection_year:last_year)] <- exp(
                                                           log( indices$ALB[[biomass_based_indices[index_id]]]@index.q[,
                                                                                                                       as.character(projection_year:last_year)] ) +
                                                             resProj[run_id,
                                                                     (length(historical_years) + 1):
                                                                       (n_projection_years + length(historical_years))] )
  
  indices$ALB[[biomass_based_indices[index_id]]]@index[ ,
                                                        historical_years] <-
    quantSums(
      mid_year_abundance[, historical_years] *
        indices$ALB[[biomass_based_indices[index_id]]]@sel.pattern[ ,
                                                                    historical_years ] *
        indices$ALB[[biomass_based_indices[index_id]]]@catch.wt[ ,
                                                                 historical_years ]) *
    indices$ALB[[biomass_based_indices[index_id]]]@index.q[ ,
                                                            historical_years ]
  
}

# ============================================================
# 13. Define plotting theme
# ============================================================

theme_oem <- function() {
  
  theme_bw() +
    theme(
      axis.title.x = element_text(
        size = 18,
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 18,
        face = "bold"
      ),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      plot.title = element_text(size = 18),
      legend.text = element_text(size = 12)
    )
  
}



# ============================================================
# 14. Compare FLBEIA and SS3 indices
# ============================================================


index_ids <- seq_along(names(indices$ALB))

fleet_ids <- c(1, 5:11)

for (index_id in index_ids) {
  
  historical_years <- dimnames(
    albacore_indices[[index_id]]
  )$year
  
  flbeia_plot <- plot(
    indices$ALB[[index_id]]@index[, historical_years]
  )
  
  flbeia_index <- as.data.frame(
    albacore_indices[[index_id]]@index
  )
  
  expected_index <- flbeia_index
  
  observed_years <- ss3_output$cpue$Yr[
    ss3_output$cpue$Fleet == fleet_ids[index_id]
  ]
  
  expected_index$data[
    flbeia_index$year %in% observed_years
  ] <- ss3_output$cpue$Exp[
    ss3_output$cpue$Fleet == fleet_ids[index_id]
  ]
  
  comparison_plot <- ggplot() +
    
    geom_line(
      data = flbeia_index,
      aes(
        x = year,
        y = data,
        colour = "FLBEIA"
      ),
      linewidth = 1
    ) +
    
    geom_line(
      data = expected_index,
      aes(
        x = year,
        y = data,
        colour = "SS3 expected"
      ),
      linewidth = 1,
      linetype = 2
    ) +
    
    scale_colour_manual(
      values = c(
        "FLBEIA" = "black",
        "SS3 expected" = "black"
      ),
      name = NULL
    ) +
    
    labs(
      title = names(indices$ALB)[index_id],
      x = "Year",
      y = "Index"
    ) +
    
    theme_oem()
  
  print(comparison_plot)
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0(
        "Comparison_Index_Exp_FLBEIA_",
        names(indices$ALB)[index_id],
        "_Run",
        run_id,
        ".jpg"
      )
    ),
    plot = comparison_plot,
    width = 3000,
    height = 3000,
    units = "px",
    dpi = 400
  )
  
}
