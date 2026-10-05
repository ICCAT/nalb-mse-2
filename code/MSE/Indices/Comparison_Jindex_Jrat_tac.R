# ============================================================
# Script: Compare_Jindex_Jrat_TAC.R
#
# Purpose:
#   Compare aggregate CPUE indicators (Jind and Jrat)
#   against projected TAC trajectories for a selected
#   management procedure.
#
# Inputs:
#   - Aggregated FLQuant index objects
#   - Aggregated TAC outputs
#
# Outputs:
#   - Comparative plots of:
#       * Jind
#       * Jrat
#       * TAC
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Jind – Jrat – TAC Comparison
#
# Objectives:
#
#   1. Read aggregated FLQuant objects for:
#
#        - Jind
#        - Jrat
#
#   2. Calculate annual summaries:
#
#        - Median
#        - 95% confidence interval
#
#   3. Extract representative trajectories:
#
#        - Iteration 1 of Jind
#        - Iteration 1 of Jrat
#
#   4. Read aggregated TAC projections.
#
#   5. Scale TAC values to allow simultaneous
#      visualization with index values.
#
#   6. Compare:
#
#        - Jind uncertainty
#        - Jrat uncertainty
#        - TAC uncertainty
#
#   7. Produce a publication-ready comparison figure
#      including:
#
#        - Confidence intervals
#        - Medians
#        - Assessment-year values
#        - Projection start year
#        - Secondary TAC axis
#
# Notes:
#
#   - TAC values are rescaled only for plotting.
#
#   - The secondary axis displays TAC in original units.
#
#   - Assessment-year values are displayed as points for
#     Jrat to highlight the TAC evaluation years.
#
# ---------------------------------------------------------------------------

library(FLCore)
library(ggplot2)
library(dplyr)
library(here)

# ============================================================
# Paths
# ============================================================
proj_dir <- here::here()
setwd(proj_dir)

source("sharepoint_path.R")
setwd(shrpoint_path)

dir_plot <- "FLoutput/Indices/Jind_Jrat_Figures"
if (!dir.exists(dir_plot)) dir.create(dir_plot, recursive = TRUE)

# ============================================================
# USER SETTINGS
# ============================================================
file_sc      <- "FLoutput/Indices/Indices_EMPW3.RData"
label_sc     <- "EmpW"

label_a      <- "Jind"
label_b      <- "Jrat"
obj_name_a   <- "J.ind.all"
obj_name_b   <- "Jrat.ind.all"
color_a      <- "#E69F00"
color_b      <- "#0072B2"

file_tac     <- "D:/AZTI/ALB - General/FLoutput/Summary/EMP/EMPW3_AggregatedOutput_ALB.RData"
tac_scenario <- "EMPW3"
color_tac    <- "grey30"

ylab_txt        <- "Index"
title_txt       <- "EmpW — Jind vs Jrat"

first.yr        <- 1999
first.yr_b      <- 1999
first.yr_tac    <- 2026
last.yr         <- 2057
projection_year <- 2026

show_ci         <- TRUE
xlim_vec        <- c(1999, 2057)
assessment_yrs  <- seq(2025, 2057, 3)
output_tag      <- "Jind_vs_Jrat_tac_EMPW3"

# ============================================================
# BLOCK 1: Load both FLQuant objects
# ============================================================
load(file_sc)
flq_a <- get(obj_name_a)
flq_b <- get(obj_name_b)

# ============================================================
# BLOCK 2: FLQuant A → per-year summary (median + 95% CI)
# ============================================================
df_med_a <- as.data.frame(flq_a) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr, year <= last.yr) %>%
  group_by(year) %>%
  summarise(
    q025 = quantile(data, 0.025, na.rm = TRUE),
    med  = quantile(data, 0.500, na.rm = TRUE),
    q975 = quantile(data, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(scenario = label_a, series = label_a)

# ============================================================
# BLOCK 3: FLQuant B → per-year summary (median + 95% CI)
# ============================================================
df_med_b <- as.data.frame(flq_b) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr_b, year <= last.yr) %>%
  group_by(year) %>%
  summarise(
    q025 = quantile(data, 0.025, na.rm = TRUE),
    med  = quantile(data, 0.500, na.rm = TRUE),
    q975 = quantile(data, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(scenario = label_b, series = label_b)

df_med <- bind_rows(df_med_a, df_med_b)

# ============================================================
# BLOCK 4: FLQuant A → iteration 1 (line)
# ============================================================
df_iter1_a <- as.data.frame(flq_a[, , , , , 1]) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr, year <= last.yr) %>%
  group_by(year) %>%
  summarise(value = median(data, na.rm = TRUE), .groups = "drop") %>%
  mutate(series = paste0(label_a, " (iter 1)"))

# ============================================================
# BLOCK 5: FLQuant B → iteration 1, assessment years only (points)
# ============================================================


df_iter1_b_asmnt <- as.data.frame(flq_b[, , , , , 1]) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr_b, year <= last.yr) %>%
  group_by(year) %>%
  summarise(value = median(data, na.rm = TRUE), .groups = "drop") %>%
  filter(year %in% assessment_yrs, !is.na(value)) %>%
  mutate(series = paste0(label_b, " (iter 1)"))

# ============================================================
# BLOCK 6: Load TAC
# ============================================================
load(file_tac)

df_tac <- adv_sc %>%
  ungroup() %>%
  filter(indicator == "tac",
         year >= first.yr_tac,
         year <= last.yr)

if (!is.null(tac_scenario))
  df_tac <- df_tac %>% filter(scenario == tac_scenario)

tac_summary <- df_tac %>%
  group_by(year) %>%
  summarise(
    q025 = quantile(value, 0.025, na.rm = TRUE),
    med  = quantile(value, 0.500, na.rm = TRUE),
    q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

tac_iter1 <- df_tac %>%
  filter(as.numeric(as.character(iter)) == 1) %>%
  select(year, value) %>%
  arrange(year)

# Scale factor: TAC max → 80% of index max
tac_max   <- max(c(tac_summary$q975, tac_iter1$value), na.rm = TRUE)
index_max <- max(df_med$q975, na.rm = TRUE)
k         <- (index_max * 0.8) / tac_max

# ============================================================
# BLOCK 7: Build the ggplot
# ============================================================
legend_order <- c(
  label_a, paste0(label_a, " (iter 1)"),
  label_b, paste0(label_b, " (iter 1)"),
  "TAC"
)

colour_values <- c(
  setNames(color_a,   label_a),
  setNames(color_a,   paste0(label_a, " (iter 1)")),
  setNames(color_b,   label_b),
  setNames(color_b,   paste0(label_b, " (iter 1)")),
  setNames(color_tac, "TAC")
)

linetype_values <- c(
  setNames("solid", label_a),
  setNames("33",    paste0(label_a, " (iter 1)")),
  setNames("solid", label_b),
  setNames("33",    paste0(label_b, " (iter 1)")),
  setNames("solid", "TAC")
)

fill_values <- c(
  setNames(color_a,   label_a),
  setNames(color_b,   label_b),
  setNames(color_tac, "TAC")
)

p <- ggplot() +
  # Vertical line marking the start of the projection period
  geom_vline(xintercept = projection_year, linetype = "dashed", linewidth = 0.8) +
  coord_cartesian(xlim = xlim_vec) +
  scale_x_continuous(
    breaks = seq(floor(xlim_vec[1] / 5) * 5, ceiling(xlim_vec[2] / 5) * 5, 5)
  ) +
  labs(x = "Year", y = ylab_txt, title = title_txt, colour = NULL, linetype = NULL)

# 95% CI ribbons for both indices
if (show_ci) {
  p <- p +
    geom_ribbon(data = df_med,
                aes(x = year, ymin = q025, ymax = q975, fill = scenario),
                alpha = 0.20, colour = NA, na.rm = TRUE, show.legend = FALSE)
}

# Median lines for both indices
p <- p +
  geom_line(data = df_med,
            aes(x = year, y = med, colour = series, linetype = series),
            linewidth = 1.1, na.rm = TRUE) +
  # Index A iter 1: dashed line
  geom_line(data = df_iter1_a,
            aes(x = year, y = value, colour = series, linetype = series),
            linewidth = 1.1, na.rm = TRUE) +
  # Index B iter 1: points at assessment years only
  geom_point(data = df_iter1_b_asmnt,
             aes(x = year, y = value, colour = series),
             size = 3, shape = 16, inherit.aes = FALSE) +
  scale_colour_manual(values  = colour_values,   breaks = legend_order) +
  scale_linetype_manual(values = linetype_values, breaks = legend_order) +
  scale_fill_manual(values = fill_values, guide = "none")

# TAC overlay: 95% CI ribbon + median line + iter 1 line
if (show_ci) {
  p <- p +
    geom_ribbon(data = tac_summary,
                aes(x = year, ymin = q025 * k, ymax = q975 * k, fill = "TAC"),
                alpha = 0.15, colour = NA, inherit.aes = FALSE, show.legend = FALSE)
}

p <- p +
  geom_line(data = tac_summary,
            aes(x = year, y = med * k, colour = "TAC", linetype = "TAC"),
            linewidth = 1.0, inherit.aes = FALSE) +
  geom_line(data = tac_iter1,
            aes(x = year, y = value * k),
            colour = color_tac, linewidth = 1.0, linetype = "33",
            inherit.aes = FALSE, show.legend = FALSE) +
  # Right secondary axis: undo the k scaling to display real TAC values
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ . / k, name = "TAC (t)")
  )

# Legend and theme
p <- p +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype  = c("solid", "33",   "solid", "blank", "solid"),
        linewidth = c(1.1,     1.3,    1.1,     0,       1.0),
        shape     = c(NA,      NA,     NA,      16,      NA),
        fill      = c(NA,      NA,     NA,      NA,
                      adjustcolor(color_tac, alpha.f = 0.30))
      )
    ),
    linetype = "none",
    fill     = "none"
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.text          = element_text(size = 14),
    axis.title         = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(colour = color_tac),
    axis.text.y.right  = element_text(colour = color_tac),
    plot.title         = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position    = "top",
    legend.text        = element_text(size = 14),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ============================================================
# BLOCK 8: Save the plot
# ============================================================
suffix_ci <- ifelse(show_ci, "withCI", "medianOnly")
plot_file <- file.path(
  dir_plot,
  paste0(output_tag, "_", suffix_ci, "_",
         first.yr, "_", last.yr, ".png")
)
ggsave(plot_file, p, width = 12, height = 7, dpi = 300)
cat("Plot saved to:", plot_file, "\n")

# ============================================================
# BLOCK 9: Display the plot
# ============================================================
p
