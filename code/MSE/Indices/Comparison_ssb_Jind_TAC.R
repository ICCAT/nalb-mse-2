# ============================================================
# Script: Comparison_SSB_Jind_TAC.R
#
# Purpose:
#   Compare historical and projected trajectories of SSB,
#   Jind and TAC for a selected management procedure.
#
# Inputs:
#   - Aggregated FLQuant objects (Jind and Jrat)
#   - Aggregated TAC outputs
#   - Observed CPUE indices from SS3
#
# Outputs:
#   - SSB versus Jind comparison plot
#   - Cross-correlation analysis (CCF)
#   - SSB versus Jind scatter plot
#   - Weighted observed Jind time series
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SSB – Jind – TAC Comparison
#
# Objectives:
#
#   1. Read aggregated FLBEIA outputs for:
#
#        - Jind
#        - Jrat
#        - TAC
#
#   2. Calculate annual summaries:
#
#        - Median
#        - 95% confidence intervals
#
#   3. Extract the first simulation trajectory
#      for comparison purposes.
#
#   4. Construct a weighted observed Jind index
#      from the historical assessment CPUE series.
#
#   5. Compare:
#
#        - Historical observed Jind
#        - Simulated Jind
#        - Spawning stock biomass (SSB)
#        - TAC trajectories
#
#   6. Visualize uncertainty through median
#      trajectories and confidence intervals.
#
#   7. Evaluate the relationship between
#      SSB and observed Jind using:
#
#        - Pearson correlation
#        - Cross-correlation analysis (CCF)
#        - Scatter plots
#
# Notes:
#
#   - TAC values are rescaled only for plotting
#     purposes and displayed on a secondary axis.
#
#   - Assessment years are highlighted using
#     Jrat values.
#
#   - The weighted observed Jind uses fleet-specific
#     weights derived from historical index variability.
#
# ---------------------------------------------------------------------------

library(FLCore)
library(FLBEIA)
library(ss3om)
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

dir_plot <- "FLoutput/Indices/SSB_Jind"
if (!dir.exists(dir_plot)) dir.create(dir_plot, recursive = TRUE)

#============================================================
# USER SETTINGS
#============================================================

# Jind (from OEM)
file_sc       <- "FLoutput/Indices/Indices_EMPW3.RData"
obj_name      <- "J.ind.all"
label_jind    <- "Jind"
color_jind    <- "#E69F00"

# SSB
file_bio      <- "D:/AZTI/ALB - General/FLoutput/Summary/EMP/EMPW3_AggregatedOutput_ALB.RData"
bio_scenario  <- "EMPW3"
color_ssb     <- "#0072B2"

# Jind observed (weighted mean of the observed indices)
ss3_dir       <- "OM/BaseCase"
ss3_nrun      <- 1
color_obs     <- "black"   

# the weights of each index
weights_df <- data.frame(
  index  = c("BB",  "JPLLS", "TAILLN", "TAILLS", "USLLN", "USLLS"),
  weight = c( 1.53,  1.30,    1.70,     1.15,     0.93,    1.05),
  stringsAsFactors = FALSE
)

# Plot settings
title_txt       <- "SSB and Jind"

first.yr        <- 1999
last.yr         <- 2055
projection_year <- 2026

show_ci    <- TRUE
xlim_vec   <- c(1999, 2021)
output_tag <- "SSB_Jind_EMPW3_hist"




# ============================================================
# BLOCK 1: Load the FLQuant object with the simulated index (Jind)
# ============================================================

load(file_sc)
flq <- get(obj_name)

# ============================================================
# BLOCK 2: FLQuant → per-year summary (median + 95% CI)
# ============================================================

df_jind_med <- as.data.frame(flq) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr, year <= last.yr) %>%
  group_by(year) %>%
  summarise(
    q025  = quantile(data, 0.025, na.rm = TRUE),
    med   = quantile(data, 0.500, na.rm = TRUE),
    q975  = quantile(data, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(serie = label_jind)

# ============================================================
# BLOCK 3: FLQuant → values from iteration 1
# ============================================================

df_jind_iter1 <- as.data.frame(flq[, , , , , 1]) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= first.yr, year <= last.yr) %>%
  group_by(year) %>%
  summarise(value = median(data, na.rm = TRUE), .groups = "drop") %>%
  mutate(serie = label_jind)

# ============================================================
# BLOCK 4: Load SSB from bio_sc
# ============================================================


load(file_bio)

df_ssb <- bio_sc %>%
  filter(indicator == "ssb",
         year >= first.yr,
         year <= last.yr)

if (!is.null(bio_scenario))
  df_ssb <- df_ssb %>% filter(scenario == bio_scenario)

ssb_summary <- df_ssb %>%
  group_by(year) %>%
  summarise(
    q025 = quantile(value, 0.025, na.rm = TRUE),
    med  = quantile(value, 0.500, na.rm = TRUE),
    q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# BLOCK 5: Observed index (weighted mean across surveys)
# ============================================================

df_obs     <- NULL
df_obs_csv <- NULL



# Read observed indices from SS3 output files
indices_raw <- readFLIBss3(
  ss3_dir,
  repfile  = paste0("Report_", ss3_nrun, ".sso"),
  compfile = paste0("CompReport_", ss3_nrun, ".sso")
)
names(indices_raw) <- c("BB", "JPLLN", "JPLLS", "TAILLN",
                        "TAILLS", "USLLN", "USLLS", "VENLL")

# Convert each survey to a long data.frame and bind all together
df_obs_csv <- data.frame()

for (i in seq_along(weights_df$index)) {
  nm <- weights_df$index[i]
  
  df_survey <- as.data.frame(indices_raw[[nm]]@index) %>%
    mutate(year   = as.numeric(as.character(year)),
           survey = nm,
           weight = weights_df$weight[i]) %>%
    filter(!is.na(data), year >= first.yr) %>%
    select(year, survey, weight, value = data)
  
  df_obs_csv <- bind_rows(df_obs_csv, df_survey)
}

# Weighted mean per year across surveys
df_obs <- df_obs_csv %>%
  group_by(year) %>%
  summarise(value = weighted.mean(value, weight, na.rm = TRUE), .groups = "drop")


# ============================================================
# BLOCK 6: Scale factor for the secondary axis
# ============================================================
# SSB is plotted on the left axis and Jind on the right.
# To make both series visually comparable, we multiply Jind by 'k'
# so its maximum reaches ~80% of the SSB maximum.
# The secondary axis undoes this scaling (÷ k) to show real Jind values.

ssb_max  <- max(ssb_summary$q975, na.rm = TRUE)
jind_max <- max(c(df_jind_med$q975,
                  if (!is.null(df_obs)) df_obs$value else NULL),
                na.rm = TRUE)
k <- (ssb_max * 0.8) / jind_max


# ============================================================
# BLOCK 7: Build the ggplot
# ============================================================

p <- ggplot() +
  # Vertical dashed line marking the start of the projection period
  geom_vline(xintercept = projection_year,
             linetype = "dashed", linewidth = 0.8) +
  coord_cartesian(xlim = xlim_vec) +
  scale_x_continuous(breaks = seq(1995, 2055, 5)) +
  labs(x = "Year", y = "SSB (t)", title = title_txt)

# SSB: 95% CI ribbon + median line
if (show_ci) {
  p <- p +
    geom_ribbon(data = ssb_summary,
                aes(x = year, ymin = q025, ymax = q975),
                fill = color_ssb, alpha = 0.20, colour = NA, na.rm = TRUE)
}
p <- p +
  geom_line(data = ssb_summary,
            aes(x = year, y = med),
            colour = color_ssb, linewidth = 1.1, na.rm = TRUE)

# Simulated Jind (OEM): 95% CI ribbon + median line — scaled by k
if (show_ci) {
  p <- p +
    geom_ribbon(data = df_jind_med,
                aes(x = year, ymin = q025 * k, ymax = q975 * k),
                fill = color_jind, alpha = 0.20, colour = NA, na.rm = TRUE)
}
p <- p +
  geom_line(data = df_jind_med,
            aes(x = year, y = med * k),
            colour = color_jind, linewidth = 1.1, linetype = "solid", na.rm = TRUE)

# Jind iteration 1 — scaled by k
p <- p +
  geom_line(data = df_jind_iter1,
            aes(x = year, y = value * k),
            colour = color_jind, linewidth = 0.8, linetype = "33", na.rm = TRUE)

# Observed Jind (weighted mean) — scaled by k
if (!is.null(df_obs)) {
  p <- p +
    geom_line(data = df_obs,
              aes(x = year, y = value * k),
              colour = color_obs, linewidth = 1.1, linetype = "solid",
              inherit.aes = FALSE)
}

# Right secondary axis: undo the k scaling to display real Jind values
p <- p +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ . / k, name = "Jind")
  )

# Manual legend: we create a data.frame with NAs so ggplot registers
# the colour and linetype levels without drawing anything real
legend_entries <- c("SSB [95% CI]", "Jind [95% CI]",
                    "Jind (iter 1)", "Jind observed (wtd mean)")

col_leg <- c("SSB [95% CI]"             = color_ssb,
             "Jind [95% CI]"            = color_jind,
             "Jind (iter 1)"            = color_jind,
             "Jind observed (wtd mean)" = color_obs)

lt_leg  <- c("SSB [95% CI]"             = "solid",
             "Jind [95% CI]"            = "solid",
             "Jind (iter 1)"            = "33",
             "Jind observed (wtd mean)" = "solid")

df_leg <- data.frame(
  x     = NA_real_,
  y     = NA_real_,
  serie = factor(legend_entries, levels = legend_entries)
)

p <- p +
  geom_line(data = df_leg,
            aes(x = x, y = y, colour = serie, linetype = serie),
            linewidth = 1.1, na.rm = TRUE) +
  scale_colour_manual(values = col_leg, name = NULL) +
  scale_linetype_manual(values = lt_leg, name = NULL) +
  scale_fill_manual(values = c(color_ssb, color_jind), guide = "none") +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype  = c("solid", "solid", "33",  "solid"),
        linewidth = c(1.1,     1.1,     0.8,   1.1),
        fill      = c(NA,      NA,      NA,    NA)   # remove fill squares from legend
      )
    ),
    linetype = "none"
  )

# Visual theme
p <- p +
  theme_bw(base_size = 18) +
  theme(
    axis.text            = element_text(size = 14),
    axis.title           = element_text(size = 16, face = "bold"),
    axis.title.y.left    = element_text(colour = color_ssb),
    axis.text.y.left     = element_text(colour = color_ssb),
    axis.title.y.right   = element_text(colour = color_jind),
    axis.text.y.right    = element_text(colour = color_jind),
    plot.title           = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position      = "top",
    legend.text          = element_text(size = 13),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank()
  )


# ============================================================
# BLOCK 8: Save the plot
# ============================================================

suffix_ci <- ifelse(show_ci, "withCI", "medianOnly")
plot_file <- file.path(
  dir_plot,
  paste0("SSB_Jind_", output_tag, "_", suffix_ci, "_",
         first.yr, "_", last.yr, ".png")
)
ggsave(plot_file, p, width = 12, height = 7, dpi = 300)
cat("Plot saved to:", plot_file, "\n")


# ============================================================
# BLOCK 9: Save CSV with the observed index (if available)
# ============================================================


csv_file <- file.path(
  dir_plot,
  paste0("Obs_indices_weighted_mean_", output_tag, ".csv")
)
write.csv(df_obs_csv, file = csv_file, row.names = FALSE)
cat("CSV saved to:", csv_file, "\n")



# ============================================================
# BLOCK 10: Display the plot
# ============================================================
p


#============================================================
# Correlation between ssb median (OM) and observed Jind
# range year: 1999 - 2021
#============================================================

corr_period_start <- 1999
corr_period_end   <- 2021

# ── 1. create two data frames to compare them ───────────
df_ssb_corr <- res$ssb_summary %>%
  filter(year >= corr_period_start, year <= corr_period_end) %>%
  select(year, ssb = med)

df_jind_corr <- res$jind_obs %>%
  filter(year >= corr_period_start, year <= corr_period_end) %>%
  select(year, jind = value)

df_corr <- inner_join(df_ssb_corr, df_jind_corr, by = "year")

cat("Years in comon :", nrow(df_corr),
    "(", min(df_corr$year), "-", max(df_corr$year), ")\n")

# ── 2. Pearson correlation (lag 0) ──────────────────────
# Linear relation between SSB y Jind .
cor_test <- cor.test(df_corr$ssb, df_corr$jind, method = "pearson")

cat("\n--- Pearson correlation (lag 0) ---\n")
cat("r =", round(cor_test$estimate, 3), "\n")
cat("p-value =", round(cor_test$p.value, 4), "\n")
cat("IC 95%: [", round(cor_test$conf.int[1], 3),
    ",", round(cor_test$conf.int[2], 3), "]\n")

# ── 3. cross correlation analysis between both series (CCF) ────────────────
# correlation between Jind and SSB
# negative lag Jind anticipate ssb pattern 
# positive lag Jind inform ssb pattern  with delay
ccf_result <- ccf(
  x     = df_corr$ssb,
  y     = df_corr$jind,
  lag.max = 5,          # lag between -5 a +5 years
  plot  = FALSE
)

# convert data.frame to plot
df_ccf <- data.frame(
  lag  = as.numeric(ccf_result$lag),
  corr = as.numeric(ccf_result$acf)
)

# Significancy: ±1.96 / sqrt(n)
n_obs     <- nrow(df_corr)
threshold <- 1.96 / sqrt(n_obs)

# ── 4. Plot CCF ──────────────────────────────────────
p_ccf <- ggplot(df_ccf, aes(x = lag, y = corr)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_hline(yintercept =  threshold, linetype = "dashed",
             colour = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = -threshold, linetype = "dashed",
             colour = "steelblue", linewidth = 0.8) +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = corr),
               linewidth = 1.0, colour = "grey30") +
  geom_point(aes(x = lag, y = corr),
             size = 3, colour = "grey30") +
  # Destacar lag 0
  geom_point(data = df_ccf %>% filter(lag == 0),
             aes(x = lag, y = corr),
             size = 4, colour = "#D55E00") +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  labs(
    x     = "Lag (years)",
    y     = "Cross-correlation",
    title = paste0("CCF: SSB (OM median) vs Jind observed (",
                   corr_period_start, "–", corr_period_end, ")"),
    subtitle = paste0("Pearson r (lag 0) = ", round(cor_test$estimate, 3),
                      "  |  p = ", round(cor_test$p.value, 4),
                      "  |  n = ", n_obs, " years\n",
                      "Dashed lines: significance threshold ±1.96/√n")
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey40"),
    panel.grid.minor = element_blank()
  )

# ── 5. Scatter plot SSB vs Jind ────────────────────────────
p_scatter <- ggplot(df_corr, aes(x = ssb, y = jind)) +
  geom_point(size = 3, colour = "grey30") +
  geom_text(aes(label = year), vjust = -0.7, size = 3.5,
            colour = "grey50") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#D55E00", fill = "#D55E00", alpha = 0.15) +
  labs(
    x     = "SSB median OM (t)",
    y     = "Jind observed (wtd mean)",
    title = paste0("SSB vs Jind observed (",
                   corr_period_start, "–", corr_period_end, ")")
  ) +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

# ── 6. save ─────────────────────────────────────────────
ggsave(file.path(dir_plot, paste0("CCF_SSB_Jind_", output_tag, ".png")),
       p_ccf,     width = 8, height = 6, dpi = 300)

ggsave(file.path(dir_plot, paste0("Scatter_SSB_Jind_", output_tag, ".png")),
       p_scatter, width = 7, height = 6, dpi = 300)

# ── 7. Summary table ─────────────────────────────
cat("\n--- Cross correlation by lag ---\n")
print(df_ccf %>% mutate(
  corr      = round(corr, 3),
  sig       = ifelse(abs(corr) > threshold, "*", "")
))

p_ccf
p_scatter