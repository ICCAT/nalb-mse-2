# ============================================================
# Script: Compare_Index_BetweenScenarios.R
#
# Purpose:
#   Compare the behaviour of a selected aggregate index
#   between two management procedure scenarios.
#
# Inputs:
#   - Aggregated FLQuant objects from scenario A
#   - Aggregated FLQuant objects from scenario B
#
# Outputs:
#   - Scenario comparison plots
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Aggregate Index Scenario Comparison
#
# Objectives:
#
#   1. Read aggregated FLQuant outputs from two scenarios.
#
#   2. Calculate annual summaries:
#
#        - Median
#        - 95% confidence interval
#
#   3. Extract representative trajectories:
#
#        - Iteration 1 from scenario A
#        - Iteration 1 from scenario B
#
#   4. Compare uncertainty envelopes between scenarios.
#
#   5. Compare differences in projected index trajectories.
#
#   6. Visualize:
#
#        - Median trajectories
#        - Confidence intervals
#        - Individual trajectories
#        - Projection start year
#
# Notes:
#
#   - The script compares the same index across two
#     management procedure scenarios.
#
#   - Confidence intervals are calculated from the
#     full set of FLQuant iterations.
#
#   - Iteration 1 is displayed only as a representative
#     trajectory and should not be interpreted as the
#     median behaviour.
#
# ---------------------------------------------------------------------------

library(FLCore)
library(ggplot2)
library(dplyr)
library(here)

#============================================================
# Paths
#============================================================
proj_dir <- here::here()
setwd(proj_dir)

source("sharepoint_path.R")
setwd(shrpoint_path)

dir_plot <- "FLoutput/Indices/Jind_Jrat_Figures"
if (!dir.exists(dir_plot)) dir.create(dir_plot, recursive = TRUE)

#============================================================
# USER SETTINGS
#============================================================

# Scenario A
file_a  <- "FLoutput/Indices/Indices_EMPw3.RData"
label_a <- "Emp_J_25-20%_maxTAC"
color_a <- "#E69F00"

# Scenario B
file_b  <- "FLoutput/Indices/Indices_EMPW8.RData"
label_b <- "JW_10%"
color_b <- "#0072B2"

# Plot settings
obj_name <- "J.ind.all"
ylab_txt <- "Index"
title_txt <- "sc 25% and 10%"

first.yr <- 2020
last.yr  <- 2055
projection_year <- 2026

show_ci <- TRUE
xlim_vec <- c(2020, 2057)
output_tag <- "EMPW3_vs_EMPW8"

#============================================================
# Helper: load object from .RData
#============================================================
load_object_from_rdata <- function(file_path, obj_name) {
  env <- new.env()
  load(file_path, envir = env)
  
  if (!exists(obj_name, envir = env, inherits = FALSE)) {
    stop(paste("Object", obj_name, "not found in", file_path))
  }
  
  get(obj_name, envir = env)
}

#============================================================
# Helper: median + 95% CI by year
#============================================================
flq_to_summary_df <- function(flq_obj, scenario_label, first.yr, last.yr) {
  
  yrs <- as.numeric(dimnames(flq_obj)$year)
  yrs <- yrs[yrs >= first.yr & yrs <= last.yr]
  
  out <- lapply(yrs, function(y) {
    vals <- as.vector(flq_obj[, as.character(y), , , , ])
    vals <- vals[is.finite(vals)]
    
    if (length(vals) == 0) {
      return(data.frame(
        year = y,
        q025 = NA_real_,
        med  = NA_real_,
        q975 = NA_real_,
        scenario = scenario_label,
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      year = y,
      q025 = unname(quantile(vals, 0.025)),
      med  = unname(quantile(vals, 0.50)),
      q975 = unname(quantile(vals, 0.975)),
      scenario = scenario_label,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(out)
}

#============================================================
# Helper: iter 1 by year
#============================================================
flq_to_iter1_df <- function(flq_obj, scenario_label, first.yr, last.yr) {
  
  yrs <- as.numeric(dimnames(flq_obj)$year)
  yrs <- yrs[yrs >= first.yr & yrs <= last.yr]
  
  out <- lapply(yrs, function(y) {
    vals <- as.vector(flq_obj[, as.character(y), , , , 1])
    vals <- vals[is.finite(vals)]
    
    if (length(vals) == 0) {
      return(data.frame(
        year = y,
        value = NA_real_,
        scenario = scenario_label,
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      year = y,
      value = median(vals),   # safe if more than one value remains
      scenario = scenario_label,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(out)
}

#============================================================
# Main function
#============================================================
compare_single_index_two_scenarios <- function(file_a, file_b,
                                               label_a, label_b,
                                               color_a, color_b,
                                               obj_name,
                                               ylab_txt,
                                               title_txt,
                                               first.yr,
                                               last.yr,
                                               projection_year,
                                               show_ci = TRUE,
                                               xlim_vec = c(first.yr, last.yr),
                                               output_tag = "comparison",
                                               dir_plot = "Output/Figures/Indices",
                                               width = 12,
                                               height = 7) {
  
  #-------------------------
  # Load data
  #-------------------------
  flq_a <- load_object_from_rdata(file_a, obj_name)
  flq_b <- load_object_from_rdata(file_b, obj_name)
  
  #-------------------------
  # Build data frames
  #-------------------------
  df_med <- bind_rows(
    flq_to_summary_df(flq_a, label_a, first.yr, last.yr),
    flq_to_summary_df(flq_b, label_b, first.yr, last.yr)
  ) %>%
    mutate(series = scenario)
  
  df_iter1 <- bind_rows(
    flq_to_iter1_df(flq_a, label_a, first.yr, last.yr),
    flq_to_iter1_df(flq_b, label_b, first.yr, last.yr)
  ) %>%
    mutate(series = paste0(scenario, " (iter 1)"))
  
  #-------------------------
  # Legend settings
  #-------------------------
  legend_order <- c(
    label_a,
    paste0(label_a, " (iter 1)"),
    label_b,
    paste0(label_b, " (iter 1)")
  )
  
  colour_values <- c(
    setNames(color_a, label_a),
    setNames(color_a, paste0(label_a, " (iter 1)")),
    setNames(color_b, label_b),
    setNames(color_b, paste0(label_b, " (iter 1)"))
  )
  

  linetype_values <- c(
    setNames("solid", label_a),
    setNames("33",    paste0(label_a, " (iter 1)")),
    setNames("solid", label_b),
    setNames("33",    paste0(label_b, " (iter 1)"))
  )
  
  fill_values <- c(
    setNames(color_a, label_a),
    setNames(color_b, label_b)
  )
  
  #-------------------------
  # Plot
  #-------------------------
  p <- ggplot() +
    
    # Vertical line for projection start
    geom_vline(xintercept = projection_year, linetype = "dashed", linewidth = 0.8) +
    
    # X axis and limits
    coord_cartesian(xlim = xlim_vec) +
    scale_x_continuous(
      breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2057)
    ) +
    
    # Labels
    labs(
      x = "Year",
      y = ylab_txt,
      title = title_txt,
      colour = NULL,
      linetype = NULL
    )
  
  # Confidence interval ribbon
  if (show_ci) {
    p <- p +
      geom_ribbon(
        data = df_med,
        aes(x = year, ymin = q025, ymax = q975, fill = scenario),
        alpha = 0.20,
        colour = NA,
        na.rm = TRUE,
        show.legend = FALSE
      )
  }
  
  # Median lines + iter 1 lines
  p <- p +
    geom_line(
      data = df_med,
      aes(x = year, y = med, colour = series, linetype = series),
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    geom_line(
      data = df_iter1,
      aes(x = year, y = value, colour = series, linetype = series),
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    
    # Manual scales
    scale_colour_manual(
      values = colour_values,
      breaks = legend_order
    ) +
    scale_linetype_manual(
      values = linetype_values,
      breaks = legend_order
    ) +
    scale_fill_manual(
      values = fill_values,
      guide = "none"
    ) +
    
    # Legend: force dashed pattern to be visible
    guides(
      colour = guide_legend(
        override.aes = list(
          linetype  = c("solid", "33", "solid", "33"),
          linewidth = c(1.1, 1.3, 1.1, 1.3)
        )
      ),
      linetype = "none"
    ) +
    
    # Theme
    theme_bw(base_size = 18) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "top",
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  #-------------------------
  # Save plot
  #-------------------------
  suffix_ci <- ifelse(show_ci, "withCI", "medianOnly")
  
  plot_file <- file.path(
    dir_plot,
    paste0(obj_name, "_", output_tag, "_", suffix_ci, "_", first.yr, "_", last.yr, ".png")
  )
  
  ggsave(plot_file, p, width = width, height = height, dpi = 300)
  
  return(list(
    plot = p,
    median_df = df_med,
    iter1_df = df_iter1,
    plot_file = plot_file
  ))
}

#============================================================
# RUN
#============================================================
res <- compare_single_index_two_scenarios(
  file_a = file_a,
  file_b = file_b,
  label_a = label_a,
  label_b = label_b,
  color_a = color_a,
  color_b = color_b,
  obj_name = obj_name,
  ylab_txt = ylab_txt,
  title_txt = title_txt,
  first.yr = first.yr,
  last.yr = last.yr,
  projection_year = projection_year,
  show_ci = show_ci,
  xlim_vec = xlim_vec,
  output_tag = output_tag,
  dir_plot = dir_plot,
  width = 12,
  height = 7
)

res$plot
