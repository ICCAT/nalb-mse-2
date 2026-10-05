# ============================================================
# Script: Compare_OM_SPiCT_Run1.R
#
# Purpose:
#   Compare OM and MP trajectories from FLBEIA and SPiCT
#   reference points for a selected scenario.
#
# Inputs:
#   - FLBEIA simulation outputs
#   - SPiCT reference points
#
# Outputs:
#   - Comparison_FLBEIA_ssb_spict_<scenario>_Run1_NEW.jpg
#   - Comparison_FLBEIA_ssb_BMSY_spict_<scenario>_Run1_NEW.jpg
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(FLBEIA)
library(here)
library(ggplot2)


# ============================================================
# 2. Set project and SharePoint directories
# ============================================================

project_dir <- here::here()
setwd(project_dir)

source("sharepoint_path.R")

setwd(shrpoint_path)


# ============================================================
# 3. Define directories and scenarios
# ============================================================

plot_directory <- "ModelBasedMp"

input_directory <- "FLoutput/Others/1Step"
output_directory <- "FLoutput/summary_FLBEIA_output"

scenario_names <- c(
  "Ef0", "Ef0_OEM",
  "catch3Ly", "catch3Ly_OEM_AR",
  "catchHigh",
  "TACF_spict", "TACF_spict_OEM",
  "IcesHCR_perfObs",
  "IcesHCR_perfObs_OEM",
  "IcesHCR_spict",
  "AlbHCR_spict",
  "AlbHCR_spict_OEM"
)

scenario_runs <- c(
  "R0a", "R0b",
  "R1a", "R1b",
  "R2b",
  "R3a", "R3b",
  "RO1b", "RO2a",
  "R4a", "R4b"
)

flbeia_object_names <- c(
  "Ef0_spict",
  "Ef0_spict",
  "TAC2020",
  "Catch_0",
  "CatchHigh",
  "TACF_spict",
  "TACF_spict",
  "IcesHCR_perfObs",
  "IcesHCR_spict",
  "AlbHCR_spict",
  "AlbHCR_spict"
)


# ============================================================
# 4. Select scenario
# ============================================================

scenario_id <- 7

scenario_files <- list.files(
  file.path(
    input_directory,
    scenario_runs[scenario_id]
  )
)

available_runs <- unique(
  sort(
    as.numeric(
      gsub(
        ".*?([0-9]+).*",
        "\\1",
        scenario_files
      )
    )
  )
)

print(available_runs)
length(available_runs)


# ============================================================
# 5. Load first run
# ============================================================

run_id <- 1

load(
  file.path(
    input_directory,
    scenario_runs[scenario_id],
    paste0(
      "Output_run_",
      run_id,
      ".RData"
    )
  )
)

flbeia_object <- flbeia_object_names[scenario_id]


# ============================================================
# 6. Extract FLBEIA outputs
# ============================================================

bio_summary <- bioSum(
  get(flbeia_object),
  long = TRUE
)

fleet_summary <- fltSum(
  get(flbeia_object),
  long = TRUE
)

fleet_stock_summary <- fltStkSum(
  get(flbeia_object),
  long = TRUE
)

metier_summary <- mtSum(
  get(flbeia_object),
  long = TRUE
)

metier_stock_summary <- mtStkSum(
  get(flbeia_object),
  long = TRUE
)


# ============================================================
# 7. Extract MP and OM stocks
# ============================================================

simulation <- get(flbeia_object)

albacore_mp <- simulation$stocks[["ALB"]]

albacore_om <- biolfleets2flstock(
  simulation$biols[["ALB"]],
  simulation$fleets
)


# ============================================================
# 8. Create MP versus OM comparison dataset
# ============================================================

comparison_data <- rbind(
  
  data.frame(
    population = "MP",
    indicator = "SSB",
    as.data.frame(ssb(albacore_mp))
  ),
  
  data.frame(
    population = "MP",
    indicator = "Harvest",
    as.data.frame(harvest(albacore_mp))
  ),
  
  data.frame(
    population = "MP",
    indicator = "Catch",
    as.data.frame(catch(albacore_mp))
  ),
  
  data.frame(
    population = "OM",
    indicator = "SSB",
    as.data.frame(ssb(albacore_om))
  ),
  
  data.frame(
    population = "OM",
    indicator = "Harvest",
    as.data.frame(fbar(albacore_om))
  ),
  
  data.frame(
    population = "OM",
    indicator = "Catch",
    as.data.frame(catch(albacore_om))
  )
  
)


# ============================================================
# 9. Define simulation years
# ============================================================

simulation_years <- list(
  initial = 2022,
  final = 2050
)


# ============================================================
# 10. Plot MP and OM trajectories
# ============================================================

comparison_plot <- ggplot(
  comparison_data,
  aes(
    x = year,
    y = data,
    colour = population
  )
) +
  geom_line() +
  facet_grid(
    indicator ~ .,
    scales = "free"
  ) +
  geom_vline(
    xintercept =
      simulation_years$initial - 1,
    linetype = "longdash"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    strip.text = element_text(size = 15),
    legend.position = "top"
  ) +
  ylab("")

print(comparison_plot)


# ============================================================
# 11. Save MP versus OM figure
# ============================================================

ggsave(
  filename = paste0(
    plot_directory,
    "/Comparison_FLBEIA_ssb_spict_",
    scenario_runs[scenario_id],
    "_Run1_NEW.jpg"
  ),
  plot = comparison_plot,
  width = 3000,
  height = 3000,
  units = "px",
  dpi = 400
)


# ============================================================
# 12. Calculate SPiCT reference points
# ============================================================

ssb_msy_mp <- c(
  rep(
    as.vector(
      simulation$covars$ALB$spict_Bmsy[, "2021"]
    ),
    length(1930:2023)
  ),
  rep(
    as.vector(
      simulation$covars$ALB$spict_Bmsy[
        ,
        as.character(seq(2024, 2048, 3))
      ]
    ),
    each = 3
  )
)[-c(120, 121)]

ssb_msy_om <- 94159

f_msy_mp <- c(
  rep(
    as.vector(
      simulation$covars$ALB$spict_Fmsy[, "2021"]
    ),
    length(1930:2023)
  ),
  rep(
    as.vector(
      simulation$covars$ALB$spict_Fmsy[
        ,
        as.character(seq(2024, 2048, 3))
      ]
    ),
    each = 3
  )
)[-c(120, 121)]

f_msy_om <- 0.088


# ============================================================
# 13. Create B/BMSY and F/FMSY comparison dataset
# ============================================================

comparison_refpt_data <- rbind(
  
  data.frame(
    population = "MP",
    indicator = "B/BMSY",
    as.data.frame(
      ssb(albacore_mp) /
        as.vector(ssb_msy_mp)
    )
  ),
  
  data.frame(
    population = "MP",
    indicator = "F/FMSY",
    as.data.frame(
      harvest(albacore_mp) /
        as.vector(f_msy_mp)
    )
  ),
  
  data.frame(
    population = "MP",
    indicator = "Catch",
    as.data.frame(
      catch(albacore_mp)
    )
  ),
  
  data.frame(
    population = "OM",
    indicator = "B/BMSY",
    as.data.frame(
      ssb(albacore_om) /
        ssb_msy_om
    )
  ),
  
  data.frame(
    population = "OM",
    indicator = "F/FMSY",
    as.data.frame(
      fbar(albacore_om) /
        f_msy_om
    )
  ),
  
  data.frame(
    population = "OM",
    indicator = "Catch",
    as.data.frame(
      catch(albacore_om)
    )
  )
  
)


# ============================================================
# 14. Plot B/BMSY and F/FMSY comparison
# ============================================================

reference_point_plot <- ggplot(
  comparison_refpt_data,
  aes(
    x = year,
    y = data,
    colour = population
  )
) +
  geom_line() +
  facet_grid(
    indicator ~ .,
    scales = "free"
  ) +
  geom_vline(
    xintercept =
      simulation_years$initial - 1,
    linetype = "longdash"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    strip.text = element_text(size = 15),
    legend.position = "top"
  ) +
  ylab("")

print(reference_point_plot)


# ============================================================
# 15. Save B/BMSY and F/FMSY figure
# ============================================================

ggsave(
  filename = paste0(
    plot_directory,
    "/Comparison_FLBEIA_ssb_BMSY_spict_",
    scenario_runs[scenario_id],
    "_Run1_NEW.jpg"
  ),
  plot = reference_point_plot,
  width = 3000,
  height = 3000,
  units = "px",
  dpi = 400
)