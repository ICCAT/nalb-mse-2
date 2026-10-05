# ============================================================
# Script: Plot_OEM_Indices.R
#
# Purpose:
#   Compare historical observed CPUE indices with simulated
#   index trajectories from the Operating Model and visualise
#   index uncertainty through time. Supports two modes:
#   'projection' (pre-combined object) and 'historical'
#   (individual runs loaded and combined on the fly).
#
# Inputs:
#   [projection] FLoutput/Indices/Indices_<FL_sc>.RData
#   [historical] FLinput/FLinput_run_<i>.RData  (n_runs files)
#   SS3 report files: Report_1.sso, CompReport_1.sso
#
# Outputs:
#   - Individual index comparison plots (.png)
#   - Combined panel figure (.png)
#   [projection only] J index and J ratio plots
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Operating Model Index Diagnostics
#
# Objectives:
#
#   1. Select run mode ('projection' or 'historical') and set
#      the associated parameters (FL_sc, last.yr, data source).
#
#   2. Read historical observed CPUE indices from SS3.
#
#   3. Load simulated index trajectories:
#        [projection] from a single pre-combined .RData file.
#        [historical] by iterating over n_runs individual files
#                     and combining with FLCore::combine().
#
#   4. Calculate uncertainty envelopes for each index:
#        - Median
#        - 95% confidence interval
#
#   5. Compare observed and simulated trajectories for:
#        BB, JPLLN, JPLLS, TAILLN, TAILLS, USLLN, USLLS, VENLL
#
#   6. [projection only] Plot J index and J ratio (simulation only).
#
#   7. Produce publication-ready individual and panel figures.
#
# Notes:
#
#   - Switch between modes by changing the `mode` variable only.
#   - The combine() call uses FLCore::combine() explicitly to
#     avoid conflict with the deprecated dplyr::combine().
#   - The vertical dashed line marks the start of the projection
#     period (2021).
#
# ---------------------------------------------------------------------------


library(FLBEIA)
library(ss3om)
library(ggplotFL)
library(ggpubr)
library(here)

proj_dir <- here::here()
setwd(proj_dir)
source("sharepoint_path.R")
setwd(shrpoint_path)


# =============================================================================
# 1. USER SETTINGS  ← only section that needs editing between runs
# =============================================================================

mode <- "projection"   # "projection"  or  "historical"

# Settings per mode
if (mode == "projection") {
  FL_sc   <- "EMPW8"
  last.yr <- 2057
  dir_in  <- file.path("FLoutput", "Indices", paste0("Indices_", FL_sc, ".RData"))
  dir_plot <- file.path("FLoutput", "Indices", paste0("Indices_",FL_sc))
} else {
  FL_sc       <- "Historic_Indices"
  last.yr     <- 2021
  n_runs      <- 400
  flinput_dir <- "D:/AZTI/ALB - General/FLinput/R1b"
  dir_plot <- file.path("FLoutput", "Indices", paste0("Indices_",FL_sc))
}

# SS3 scenario used to read observed indices
nm   <- c("BaseCase", "AGE", "CPUE", "SIZE")
sc   <- paste0("OM/", nm)
sc.i <- 1

# Index names (must match names inside the FLIndex objects)
index_names <- c("BB", "JPLLN", "JPLLS", "TAILLN", "TAILLS", "USLLN", "USLLS", "VENLL")

# Output directory

if (!dir.exists(dir_plot)) dir.create(dir_plot, recursive = TRUE)


# =============================================================================
# 2. LOAD OBSERVED INDICES FROM SS3
# =============================================================================

indices_ALB <- readFLIBss3(
  sc[sc.i],
  repfile  = "Report_1.sso",
  compfile = "CompReport_1.sso"
)
names(indices_ALB) <- index_names


# =============================================================================
# 3. LOAD SIMULATED INDICES (mode-dependent)
# =============================================================================

if (mode == "projection") {
  
  # Pre-combined multi-iteration object — load directly
  load(dir_in)
  
} else {
  
  # Load n_runs individual files and combine along the iter dimension
  index_lists <- setNames(vector("list", length(index_names)), index_names)
  for (nm_i in index_names) index_lists[[nm_i]] <- vector("list", n_runs)
  
  cat("── Loading all runs ──────────────────────────────\n")
  for (i in seq_len(n_runs)) {
    
    if (i %% 50 == 0) cat(sprintf("   Run %d / %d\n", i, n_runs))
    
    env_i <- new.env()
    load(file.path(flinput_dir, paste0("FLinput_run_", i, ".RData")), envir = env_i)
    flinput_i <- get(ls(env_i)[13], envir = env_i)
    
    for (nm_i in index_names) {
      index_lists[[nm_i]][[i]] <- flinput_i$ALB[[nm_i]]@index
    }
  }
  
  # Stack iterations with FLCore::combine (explicit to avoid dplyr conflict)
  cat("\n── Combining into multi-iteration FLQuant objects ──\n")
  BB.ind.all     <- Reduce(FLCore::combine, index_lists[["BB"]])
  JPLLN.ind.all  <- Reduce(FLCore::combine, index_lists[["JPLLN"]])
  JPLLS.ind.all  <- Reduce(FLCore::combine, index_lists[["JPLLS"]])
  TAILLN.ind.all <- Reduce(FLCore::combine, index_lists[["TAILLN"]])
  TAILLS.ind.all <- Reduce(FLCore::combine, index_lists[["TAILLS"]])
  USLLN.ind.all  <- Reduce(FLCore::combine, index_lists[["USLLN"]])
  USLLS.ind.all  <- Reduce(FLCore::combine, index_lists[["USLLS"]])
  VENLL.ind.all  <- Reduce(FLCore::combine, index_lists[["VENLL"]])
  
  cat(sprintf("   Done — %d iterations per index\n\n", dim(BB.ind.all)[6]))
}


# =============================================================================
# 4. HELPER FUNCTIONS (shared by both modes)
# =============================================================================

make_index_plot <- function(index_name,
                            sim_obj,
                            ylab_txt,
                            indices_list,
                            last.yr      = 2055,
                            xlim_vec     = c(1981, last.yr),
                            ylim_vec     = c(0, 5),
                            drop_obs_row = NA,
                            drop_sim_col = NA,
                            show.legend  = FALSE) {
  
  df.ind <- as.data.frame(indices_list[[index_name]]@index)
  
  if (!is.na(drop_obs_row) && drop_obs_row <= nrow(df.ind))
    df.ind <- df.ind[-drop_obs_row, , drop = FALSE]
  
  first.yr     <- min(as.numeric(dimnames(indices_list[[index_name]]@index)$year))
  proj.yr      <- max(as.numeric(dimnames(indices_list[[index_name]]@index)$year))
  last.plot.yr <- ifelse(proj.yr < 2021, proj.yr, last.yr)
  
  sim_dat <- sim_obj[, as.character(first.yr:last.plot.yr), drop = FALSE]
  
  if (!is.na(drop_sim_col) && drop_sim_col <= ncol(sim_dat))
    sim_dat <- sim_dat[, -drop_sim_col, drop = FALSE]
  
  p <- plot(sim_dat, probs = c(0.025, 0.5, 0.975)) +
    coord_cartesian(xlim = xlim_vec, ylim = ylim_vec) +
    labs(x = "Year", y = ylab_txt, colour = NULL, linetype = NULL, fill = NULL) +
    theme_bw(base_size = 20) +
    theme(
      axis.text          = element_text(size = 15),
      axis.title.x       = element_text(size = 16, face = "bold"),
      axis.title.y       = element_text(size = 19, face = "bold", margin = margin(r = 10)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_blank(),
      strip.background   = element_blank(),
      legend.position    = if (show.legend) "bottom" else "none",
      legend.direction   = "vertical",
      legend.box         = "vertical",
      legend.background  = element_rect(fill = "white", colour = "grey50"),
      legend.text        = element_text(size = 15)
    ) +
    geom_vline(aes(xintercept = 2021, colour = "Projection", linetype = "Projection"),
               linewidth = 0.9) +
    geom_line(data = subset(df.ind, !is.na(data)),
              aes(x = year, y = data, colour = "Observed index", linetype = "Observed index"),
              linewidth = 0.9) +
    geom_ribbon(data = data.frame(x = 1, ymin = 0, ymax = 1),
                aes(x = x, ymin = ymin, ymax = ymax, fill = "95% CI"),
                alpha = 0.2, inherit.aes = FALSE) +
    geom_line(data = data.frame(x = 1, y = 1),
              aes(x = x, y = y, colour = "Median (simulated index)",
                  linetype = "Median (simulated index)"),
              linewidth = 0.9, inherit.aes = FALSE) +
    scale_colour_manual(values = c("Observed index"           = "#D55E00",
                                   "Median (simulated index)" = "black",
                                   "Projection"               = "black")) +
    scale_fill_manual(values = c("95% CI" = "grey70")) +
    scale_linetype_manual(values = c("Observed index"           = "solid",
                                     "Median (simulated index)" = "solid",
                                     "Projection"               = "dashed")) +
    guides(fill     = guide_legend(order = 1, override.aes = list(alpha = 0.2)),
           colour   = guide_legend(order = 2),
           linetype = guide_legend(order = 2))
  
  return(p)
}

# ---

make_sim_only_plot <- function(sim_obj,
                               ylab_txt,
                               last.yr     = 2055,
                               xlim_vec    = c(1981, 2055),
                               ylim_vec    = c(0, 5),
                               show.legend = FALSE) {
  
  yrs     <- as.numeric(dimnames(sim_obj)$year)
  sim_dat <- sim_obj[, as.character(yrs[yrs <= last.yr]), drop = FALSE]
  
  p <- plot(sim_dat, probs = c(0.025, 0.5, 0.975)) +
    coord_cartesian(xlim = xlim_vec, ylim = ylim_vec) +
    labs(x = "Year", y = ylab_txt, colour = NULL, linetype = NULL, fill = NULL) +
    theme_bw(base_size = 20) +
    theme(
      axis.text          = element_text(size = 15),
      axis.title.x       = element_text(size = 16, face = "bold"),
      axis.title.y       = element_text(size = 19, face = "bold", margin = margin(r = 10)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_blank(),
      strip.background   = element_blank(),
      legend.position    = if (show.legend) "bottom" else "none",
      legend.direction   = "vertical",
      legend.box         = "vertical",
      legend.background  = element_rect(fill = "white", colour = "grey50"),
      legend.text        = element_text(size = 15)
    ) +
    geom_vline(aes(xintercept = 2021, colour = "Projection", linetype = "Projection"),
               linewidth = 0.9) +
    geom_ribbon(data = data.frame(x = 1, ymin = 0, ymax = 1),
                aes(x = x, ymin = ymin, ymax = ymax, fill = "95% CI"),
                alpha = 0.2, inherit.aes = FALSE) +
    geom_line(data = data.frame(x = 1, y = 1),
              aes(x = x, y = y, colour = "Median (simulated index)",
                  linetype = "Median (simulated index)"),
              linewidth = 0.9, inherit.aes = FALSE) +
    scale_colour_manual(values = c("Median (simulated index)" = "black",
                                   "Projection"               = "black")) +
    scale_fill_manual(values = c("95% CI" = "grey70")) +
    scale_linetype_manual(values = c("Median (simulated index)" = "solid",
                                     "Projection"               = "dashed")) +
    guides(fill     = guide_legend(order = 1, override.aes = list(alpha = 0.2)),
           colour   = guide_legend(order = 2),
           linetype = guide_legend(order = 2))
  
  return(p)
}


# =============================================================================
# 5. BUILD PLOTS
# =============================================================================

p1 <- make_index_plot("BB",     BB.ind.all,     "BB",              indices_ALB, last.yr, drop_obs_row = 40)
p2 <- make_index_plot("JPLLN",  JPLLN.ind.all,  "Japan LL North",  indices_ALB, last.yr, drop_sim_col = 41)
p3 <- make_index_plot("JPLLS",  JPLLS.ind.all,  "Japan LL South",  indices_ALB, last.yr)
p4 <- make_index_plot("TAILLN", TAILLN.ind.all, "Taiwan LL North", indices_ALB, last.yr)
p5 <- make_index_plot("TAILLS", TAILLS.ind.all, "Taiwan LL South", indices_ALB, last.yr)
p6 <- make_index_plot("USLLN",  USLLN.ind.all,  "US LL North",     indices_ALB, last.yr)
p7 <- make_index_plot("USLLS",  USLLS.ind.all,  "US LL South",     indices_ALB, last.yr)
p8 <- make_index_plot("VENLL",  VENLL.ind.all,  "Venezuelan LL",   indices_ALB, last.yr)

# J index plots — projection mode only
if (mode == "projection") {
  p9  <- make_sim_only_plot(J.ind.all, "J index", last.yr)
  p10 <- make_sim_only_plot(J.ind.all, "J rat",   last.yr)
}


# =============================================================================
# 6. SAVE INDIVIDUAL PLOTS
# =============================================================================

ggsave(file.path(dir_plot, "BB_proj_LN_CI.png"),  p1, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "JPLLN_proj_CI.png"),  p2, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "JPLLS_proj_CI.png"),  p3, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "TAILLN_proj_CI.png"), p4, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "TAILLS_proj_CI.png"), p5, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "USLLN_proj_CI.png"),  p6, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "USLLS_proj_CI.png"),  p7, width = 8, height = 8, dpi = 300)
ggsave(file.path(dir_plot, "VENLL_proj_CI.png"),  p8, width = 8, height = 8, dpi = 300)

if (mode == "projection") {
  ggsave(file.path(dir_plot, "J_proj_CI.png"),    p9,  width = 8, height = 8, dpi = 300)
  ggsave(file.path(dir_plot, "Jrat_proj_CI.png"), p10, width = 8, height = 8, dpi = 300)
}


# =============================================================================
# 7. COMBINED PANEL FIGURE
# =============================================================================

p1_leg <- make_index_plot("BB", BB.ind.all, "BB", indices_ALB,
                          last.yr, drop_obs_row = 40, show.legend = TRUE)
legend  <- ggpubr::get_legend(p1_leg)
pLegend <- ggpubr::as_ggplot(legend)

# Layout differs by mode: projection adds J index columns
if (mode == "projection") {
  pAll <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, pLegend, ncol = 5, nrow = 2)
} else {
  pAll <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, pLegend, ncol = 3, nrow = 3)
}

ggsave(
  file.path(dir_plot, paste0("Indices_OEM_", FL_sc, ".png")),
  pAll, width = 30, height = 19, dpi = 300
)

cat("══ All plots saved to:", dir_plot, "══\n")