# ============================================================
# Script: Plot_AggregatedIndices_byScenario.R
#
# Purpose:
#   Plot aggregated FLBEIA index projections and compare them
#   with individual-run and observed SS3 index time series.
#
# Inputs:
#   - FLBEIA aggregated index objects
#   - FLBEIA simulation output
#   - SS3 observed index objects
#
# Outputs:
#   - Aggregated index projection figures
#   - Aggregated projections with individual-run indices
#   - Aggregated projections with observed SS3 indices
#
# Author: AZTI
# ============================================================


# ============================================================
# 1. Load packages
# ============================================================

library(FLBEIA)
library(ss3om)
library(ggplotFL)
library(ggplot2)
library(here)


# ============================================================
# 2. Set project and SharePoint directories
# ============================================================

project_directory <- here::here()
setwd(project_directory)

source("sharepoint_path.R")
setwd(shrpoint_path)


# ============================================================
# 3. Define scenarios
# ============================================================

scenario_names <- c(
  "EF0",
  "TACF",
  "AlbHCR",
  sort(
    apply(
      expand.grid(
        "Btr",
        seq(0.8, 1.2, 0.1),
        "_",
        "Ftg",
        seq(0.8, 1.2, 0.1)
      ),
      1,
      paste,
      collapse = ""
    )
  )
)

scenario_runs <- c(
  "R0b",
  "R3b",
  "R4b",
  sort(
    apply(
      expand.grid("S", 1:5, 1:5),
      1,
      paste,
      collapse = ""
    )
  )
)

flbeia_object_names <- c(
  "Ef0_spict",
  "TACF_spict",
  "AlbHCR_spict",
  rep("AlbHCR_spict", 25)
)


# ============================================================
# 4. Select scenario and run
# ============================================================

scenario_id <- 1
run_id <- 1
last_year <- 2050

input_directory <- file.path(
  "FLinput",
  "R1b"
)

output_directory <- file.path(
  "FLoutput","Others","1Step",
  scenario_runs[scenario_id]
)

plot_directory <- file.path(
  "FLoutput","Others","1Step",
  scenario_runs[scenario_id],"Indices"
)

if (!dir.exists(plot_directory)) {
  dir.create(
    plot_directory,
    recursive = TRUE
  )
}


# ============================================================
# 5. Load FLBEIA outputs
# ============================================================

load(
  file.path(
    output_directory,
    paste0(
      "Output_run_",
      run_id,
      ".RData"
    )
  )
)

load(
  file.path(
    output_directory,
    "Output_res.RData"
  )
)

flbeia_run <- get(
  flbeia_object_names[scenario_id]
)

albacore_indices <- flbeia_run$indices$ALB


# ============================================================
# 6. Read observed SS3 indices
# ============================================================

observed_indices <- readFLIBss3(
  "OM/BaseCase",
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

names(observed_indices) <- c(
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
# 7. Define index settings
# ============================================================

index_names <- names(observed_indices)

aggregated_object_names <- paste0(
  index_names,
  ".ind.all"
)

projection_start_years <- c(
  BB = 2021,
  JPLLN = 2021,
  JPLLS = 2021,
  TAILLN = 2021,
  TAILLS = 2021,
  USLLN = 2021,
  USLLS = 2021,
  VENLL = 2022
)

index_last_years <- c(
  BB = last_year - 2,
  JPLLN = 2009,
  JPLLS = last_year,
  TAILLN = last_year,
  TAILLS = last_year,
  USLLN = last_year,
  USLLS = last_year,
  VENLL = last_year
)

plot_probabilities <- c(
  0.025,
  0.05,
  0.10,
  0.50,
  0.90,
  0.95,
  0.975
)


# ============================================================
# 8. Define plotting theme
# ============================================================

theme_indices <- function() {
  
  theme_bw() +
    theme(
      axis.text = element_text(size = 25),
      axis.title = element_text(
        size = 25,
        face = "bold"
      ),
      plot.title = element_text(
        size = 22,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15)
    )
  
}


# ============================================================
# 9. Create index projection figures
# ============================================================

for (index_name in index_names) {
  
  aggregated_index <- get(
    paste0(
      index_name,
      ".ind.all"
    )
  )
  
  first_year <- min(
    as.numeric(
      dimnames(
        albacore_indices[[index_name]]@index
      )$year
    )
  )
  
  final_year <- index_last_years[index_name]
  
  if (index_name == "BB") {
    
    plot_years <- c(
      first_year:2019,
      2021:final_year
    )
    
  } else {
    
    plot_years <- first_year:final_year
    
  }
  
  plot_years <- intersect(
    plot_years,
    as.numeric(
      dimnames(aggregated_index)$year
    )
  )
  
  # ----------------------------------------------------------
  # 9.1 Aggregated projection
  # ----------------------------------------------------------
  
  projection_plot <- plot(
    aggregated_index[
      ,
      as.character(plot_years)
    ],
    probs = plot_probabilities
  ) +
    xlim(1981, last_year) +
    geom_vline(
      xintercept = projection_start_years[index_name],
      linewidth = 0.4,
      colour = "black",
      linetype = 2
    ) +
    labs(
      title = index_name,
      x = "Year",
      y = index_name
    ) +
    theme_indices()
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0(
        index_name,
        "_projection.png"
      )
    ),
    plot = projection_plot,
    width = 800,
    height = 800,
    units = "px",
    dpi = 100
  )
  
  
  # ----------------------------------------------------------
  # 9.2 Aggregated projection with individual run
  # ----------------------------------------------------------
  
  individual_run_data <- as.data.frame(
    albacore_indices[[index_name]]@index
  )
  
  individual_run_plot <- projection_plot +
    geom_line(
      data = individual_run_data,
      mapping = aes(
        x = year,
        y = data,
        colour = "Individual run"
      ),
      linewidth = 0.4,
      linetype = 2,
      na.rm = TRUE
    ) +
    scale_colour_manual(
      values = c(
        "Individual run" = "black"
      ),
      name = NULL
    )
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0(
        index_name,
        "_projection_individual_run.png"
      )
    ),
    plot = individual_run_plot,
    width = 800,
    height = 800,
    units = "px",
    dpi = 100
  )
  
  
  # ----------------------------------------------------------
  # 9.3 Aggregated projection with observed SS3 index
  # ----------------------------------------------------------
  
  observed_index_data <- as.data.frame(
    observed_indices[[index_name]]@index
  )
  
  observed_index_data <- observed_index_data[
    !is.na(observed_index_data$data),
  ]
  
  observed_index_plot <- projection_plot +
    geom_line(
      data = observed_index_data,
      mapping = aes(
        x = year,
        y = data,
        colour = "Observed SS3 index"
      ),
      linewidth = 0.4,
      linetype = 2,
      na.rm = TRUE
    ) +
    scale_colour_manual(
      values = c(
        "Observed SS3 index" = "black"
      ),
      name = NULL
    )
  
  ggsave(
    filename = file.path(
      plot_directory,
      paste0(
        index_name,
        "_projection_observed.png"
      )
    ),
    plot = observed_index_plot,
    width = 800,
    height = 800,
    units = "px",
    dpi = 100
  )
  
}
