# ============================================================
# Script: OEM_AR1ResidualProjection.R
#
# Purpose:
#   Generate future CPUE observation errors using an AR(1)
#   process based on SS3 residuals and estimated OEM
#   parameters. Produce residual projections under different
#   initialization assumptions and evaluate their statistical
#   properties.
#
# Inputs:
#   - SS3 output files (Report.sso)
#   - AR(1) parameter file (ARpar_Fl<n>_ALL.csv)
#   - Historical CPUE residuals from SS3
#
# Outputs:
#   - Rand_And_ResidualsAR_Fl<n>_<Scenario>.RData
#     (stationary initialization)
#   - Rand_And_ResidualsAR_Fl<n>_<Scenario>.RData
#     (last-observation initialization, stored separately)
#   - Diagnostic residual projection figures
#   - Aggregated comparison plots across OM scenarios
#   - Exponential-scale bias validation plots
#
# Description:
#   The script estimates common AR(1) parameters (rho and
#   sigma) across uncertainty-grid scenarios and generates
#   Monte Carlo projections of future observation errors.
#   Residual trajectories are simulated under:
#     1) Stationary AR(1) initialization
#     2) Initialization from the last observed residual
#     3) Independent corrected residuals without
#        autocorrelation
#
#   Results are saved for later use in OEM index projection
#   workflows and summarized through diagnostic plots on both
#   log and exponential scales.
#
# Author: AZTI
# ============================================================


# ==============================================================================
# OEM: Observation Error Model — AR(1) Residual Projection
# Species : North Atlantic Albacore Tuna (ALB)
# Purpose : Generate independent future errors per scenario
# ==============================================================================

library(r4ss)
library(here)

proj_dir <- here::here()
setwd(proj_dir)

source('sharepoint_path.R')
setwd(shrpoint_path)

# ==============================================================================
# 1. GLOBAL PARAMETERS
# ==============================================================================
Fl        <- 11   # Fleet of interest
proj.yr   <- 200      # Number of projection years per iteration
nit       <- 200      # Number of Monte Carlo iterations
sp        <- "ALB"    # Species name for plot titles
start_yr  <- 2022     # First projection year

# Projection year axis — defined once, used in all plots
future_yrs <- seq(start_yr, start_yr + proj.yr - 1)

# Input / output directories
in.dir   <- "OEM/InputMSE_OEM"
out.dir  <- in.dir
plot.dir <- "OEM/Simulation"

# Create output folders if they do not exist
dir.create(paste0(out.dir, "/ProjRes"),         recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(out.dir, "/ProjRes_lastobs"), recursive = TRUE, showWarnings = FALSE)
dir.create(plot.dir,                            recursive = TRUE, showWarnings = FALSE)

# Operating model scenarios
nm   <- c("BaseCase", "AGE", "CPUE", "SIZE")
sc   <- paste0("OM/", nm, "/hess")
n_sc <- length(sc)

# Colors per scenario — one color per scenario for the aggregated plots
sc_cols <- c("steelblue", "darkorange", "forestgreen", "purple")

# ==============================================================================
# 2. AR(1) PARAMETERS — loaded once, shared across all scenarios
# ==============================================================================
grid.AR <- read.csv(paste0(in.dir, "/ARpar_Fl", Fl, "_ALL.csv"))

# Row 5 is the overall mean of the other rows → excluded to avoid double-counting
rho   <- mean(grid.AR$rho[-5])
sigma <- mean(grid.AR$sigma[-5])

cat(sprintf("AR(1) parameters: rho = %.4f | sigma = %.4f\n\n", rho, sigma))

# ==============================================================================
# 3. STRUCTURES TO ACCUMULATE DATA ACROSS SCENARIOS
# ==============================================================================
# Each list will hold one matrix per scenario; after the loop we rbind them
resProj_list        <- vector("list", n_sc)
resProj_lastobs_list <- vector("list", n_sc)
randRes_mat_list    <- vector("list", n_sc)
res_list            <- vector("list", n_sc)
hist_yrs_list       <- vector("list", n_sc)

# ==============================================================================
# 4. MAIN LOOP — one INDEPENDENT resProj per scenario
# ==============================================================================
for (sc_idx in seq_along(sc)) {
  
  nm.sc      <- nm[sc_idx]
  ss3.out.wd <- sc[sc_idx]
  
  cat(sprintf("── Scenario %d/%d: %s ──────────────────────────\n",
              sc_idx, n_sc, nm.sc))
  
  # ── 4.1 Unique seed per scenario ────────────────────────────────────────────
  set.seed(500 + Fl + sc_idx)
  
  # ── 4.2 Random innovations — shared by both projections ─────────────────────
  nProj   <- nit * proj.yr
  randRes <- rnorm(nProj, mean = 0, sd = sigma)
  
  # ── 4.3 Corrected random residuals (no autocorrelation case) ────────────────
  randRes_corrected <- rnorm(nProj, mean = -sigma^2 / 2, sd = sigma)
  
  # ── 4.4 Read SS3 output and compute observed residuals ──────────────────────
  ss3  <- SS_output(dir = ss3.out.wd, repfile = "Report.sso", covar = FALSE,
                    verbose = FALSE, printstats = FALSE)
  cpue <- ss3$cpue[ss3$cpue$Fleet == Fl, ]
  res  <- log(cpue$Obs) - log(cpue$Exp)
  ny   <- length(res)
  
  hist_yrs <- cpue$Yr
  
  # ── 4.5 AR(1) projection — init from stationary distribution ────────────────
  resProj <- matrix(0, nrow = nit, ncol = proj.yr)
  
  for (j in 1:nit) {
    idx_base <- (j - 1) * proj.yr
    prev_val <- rnorm(1, mean = -sigma^2 / 2, sd = sigma)
    
    resProj[j, 1] <- rho * prev_val +
      sqrt(1 - rho^2) * randRes[idx_base + 1] -
      (1 - rho) * sigma^2 / 2
    
    for (i in 2:proj.yr) {
      resProj[j, i] <- rho * resProj[j, i - 1] +
        sqrt(1 - rho^2) * randRes[idx_base + i] -
        (1 - rho) * sigma^2 / 2
    }
  }
  
  # ── 4.6 AR(1) projection — init from last observed residual res[ny] ─────────
  resProj_lastobs <- matrix(0, nrow = nit, ncol = proj.yr)
  
  for (j in 1:nit) {
    idx_base <- (j - 1) * proj.yr
    
    resProj_lastobs[j, 1] <- rho * res[ny] +
      sqrt(1 - rho^2) * randRes[idx_base + 1] -
      (1 - rho) * sigma^2 / 2
    
    for (i in 2:proj.yr) {
      resProj_lastobs[j, i] <- rho * resProj_lastobs[j, i - 1] +
        sqrt(1 - rho^2) * randRes[idx_base + i] -
        (1 - rho) * sigma^2 / 2
    }
  }
  
  # ── 4.7 Quick bias correction check ─────────────────────────────────────────
  cat(sprintf("   ✔ mean(exp(resProj))           = %.4f (expected ≈ 1.0)\n",
              mean(exp(resProj))))
  cat(sprintf("   ✔ mean(exp(resProj_lastobs))   = %.4f (expected ≈ 1.0)\n",
              mean(exp(resProj_lastobs))))
  cat(sprintf("   ✔ mean(exp(randRes_corrected)) = %.4f (expected ≈ 1.0)\n",
              mean(exp(randRes_corrected))))
  cat(sprintf("   ℹ res[ny] = %.4f (%s)\n", res[ny], nm.sc))
  
  # ── 4.8 Save — same filename, different subfolders ──────────────────────────
  out_filename <- paste0("Rand_And_ResidualsAR_Fl", Fl, "_", nm.sc, ".RData")
  
  save(resProj, randRes, randRes_corrected, rho, sigma, res, ss3,
       file = paste0(out.dir, "/ProjRes/", out_filename))
  cat(sprintf("   Saved: ProjRes/%s\n", out_filename))
  
  save(resProj_lastobs, randRes, randRes_corrected, rho, sigma, res, ss3,
       file = paste0(out.dir, "/ProjRes_lastobs/", out_filename))
  cat(sprintf("   Saved: ProjRes_lastobs/%s\n\n", out_filename))
  
  # ── 4.9 Accumulate data for aggregated plots ─────────────────────────────────
  resProj_list[[sc_idx]]         <- resProj
  resProj_lastobs_list[[sc_idx]] <- resProj_lastobs
  randRes_mat_list[[sc_idx]]     <- matrix(randRes_corrected,
                                           nrow = nit, ncol = proj.yr,
                                           byrow = TRUE)
  res_list[[sc_idx]]      <- res
  hist_yrs_list[[sc_idx]] <- hist_yrs
}

cat("══ All scenarios processed — generating aggregated plots ══\n\n")

# ==============================================================================
# 5. AGGREGATED PLOTS — all scenarios combined
# ==============================================================================
# Stack all scenario matrices: (nit * n_sc) rows × proj.yr columns
resProj_all        <- do.call(rbind, resProj_list)
resProj_lastobs_all <- do.call(rbind, resProj_lastobs_list)
randRes_mat_all    <- do.call(rbind, randRes_mat_list)

# Use historical years from the first scenario (same for all)
hist_yrs_plot <- hist_yrs_list[[1]]
time_full     <- c(hist_yrs_plot, future_yrs)

# Global summary statistics (across all scenarios and iterations)
rp_median <- apply(resProj_all, 2, median)
rp_lower  <- apply(resProj_all, 2, quantile, probs = 0.025)
rp_upper  <- apply(resProj_all, 2, quantile, probs = 0.975)

lo_median <- apply(resProj_lastobs_all, 2, median)
lo_lower  <- apply(resProj_lastobs_all, 2, quantile, probs = 0.025)
lo_upper  <- apply(resProj_lastobs_all, 2, quantile, probs = 0.975)

exp_mean  <- apply(exp(resProj_all), 2, mean)
exp_lower <- apply(exp(resProj_all), 2, quantile, probs = 0.025)
exp_upper <- apply(exp(resProj_all), 2, quantile, probs = 0.975)

rc_median <- apply(randRes_mat_all, 2, median)
rc_lower  <- apply(randRes_mat_all, 2, quantile, probs = 0.025)
rc_upper  <- apply(randRes_mat_all, 2, quantile, probs = 0.975)

exp_rc_mean  <- apply(exp(randRes_mat_all), 2, mean)
exp_rc_lower <- apply(exp(randRes_mat_all), 2, quantile, probs = 0.025)
exp_rc_upper <- apply(exp(randRes_mat_all), 2, quantile, probs = 0.975)

# Per-scenario medians — one line per scenario for reference
rp_sc_median <- sapply(resProj_list,        function(m) apply(m, 2, median))
lo_sc_median <- sapply(resProj_lastobs_list, function(m) apply(m, 2, median))

cex <- 1.5

# ── Plot 1: resProj (stationary) — all scenarios aggregated ──────────────────
png(file.path(plot.dir, paste0("resProj_stat_Fl", Fl, "_ALL.png")),
    width = 1000, height = 700)
par(cex.lab = cex, cex.axis = cex, cex.main = cex)
plot(future_yrs, rp_median, type = "n", ylim = c(-1.5, 1.5),
     main = paste0(sp, " Fl", Fl, ": AR(1) stationary init — all scenarios — 95% CI"),
     ylab = "Residual", xlab = "Year")
# All iterations in grey
for (j in 1:nrow(resProj_all))
  lines(future_yrs, resProj_all[j, ], col = "grey90", lwd = 1)
# 95% CI overall
polygon(c(future_yrs, rev(future_yrs)), c(rp_lower, rev(rp_upper)),
        col = rgb(0, 0, 0, 0.15), border = NA)
# Median per scenario (colored lines)
for (sc_idx in seq_along(sc))
  lines(future_yrs, rp_sc_median[, sc_idx], col = sc_cols[sc_idx], lwd = 2, lty = 2)
# Overall median (black)
lines(future_yrs, rp_median, col = "black", lwd = 2, lty = 1)
legend("bottomleft",
       legend = c("Overall median", nm),
       col = c("black", sc_cols), lty = c(1, rep(2, n_sc)),
       lwd = 2, bty = "n", cex = cex * 0.8)
dev.off()

# ── Plot 2: resProj_lastobs — all scenarios aggregated ───────────────────────
png(file.path(plot.dir, paste0("resProj_lastobs_Fl", Fl, "_ALL.png")),
    width = 1000, height = 700)
par(cex.lab = cex, cex.axis = cex, cex.main = cex)
plot(future_yrs, lo_median, type = "n", ylim = c(-1.5, 1.5),
     main = paste0(sp, " Fl", Fl, ": AR(1) init from res[ny] — all scenarios — 95% CI"),
     ylab = "Residual", xlab = "Year")
for (j in 1:nrow(resProj_lastobs_all))
  lines(future_yrs, resProj_lastobs_all[j, ], col = "grey90", lwd = 1)
polygon(c(future_yrs, rev(future_yrs)), c(lo_lower, rev(lo_upper)),
        col = rgb(0, 0, 0, 0.15), border = NA)
for (sc_idx in seq_along(sc))
  lines(future_yrs, lo_sc_median[, sc_idx], col = sc_cols[sc_idx], lwd = 2, lty = 2)
lines(future_yrs, lo_median, col = "black", lwd = 2, lty = 1)
legend("bottomleft",
       legend = c("Overall median", nm),
       col = c("black", sc_cols), lty = c(1, rep(2, n_sc)),
       lwd = 2, bty = "n", cex = cex * 0.8)
dev.off()

# ── Plot 3: comparison stationary vs lastobs + observed residuals ─────────────
png(file.path(plot.dir, paste0("resProj_comparison_Fl", Fl, "_ALL.png")),
    width = 1000, height = 700)
par(cex.lab = cex, cex.axis = cex, cex.main = cex)
plot(time_full, c(rep(NA, length(hist_yrs_plot)), rp_median),
     type = "n", ylim = c(-1.5, 1.5),
     main = paste0(sp, " Fl", Fl, ": stationary vs last obs — all scenarios — 95% CI"),
     ylab = "Residual", xlab = "Year")
polygon(c(future_yrs, rev(future_yrs)), c(rp_lower, rev(rp_upper)),
        col = rgb(1, 0, 0, 0.2), border = NA)
polygon(c(future_yrs, rev(future_yrs)), c(lo_lower, rev(lo_upper)),
        col = rgb(0, 0, 1, 0.2), border = NA)
# Observed residuals per scenario
for (sc_idx in seq_along(sc))
  lines(hist_yrs_list[[sc_idx]], res_list[[sc_idx]],
        col = sc_cols[sc_idx], lwd = 1.5, lty = 1)
lines(future_yrs, rp_median, col = "red",  lwd = 2, lty = 2)
lines(future_yrs, lo_median, col = "blue", lwd = 2, lty = 2)
abline(v = max(hist_yrs_plot), lty = 3, col = "grey40")
legend("bottomleft",
       legend = c(paste0("Observed ", nm), "Median stationary", "Median last obs"),
       col = c(sc_cols, "red", "blue"),
       lty = c(rep(1, n_sc), 2, 2),
       lwd = 2, bty = "n", cex = cex * 0.8)
dev.off()

# ── Plot 4: exponential scale — exp(resProj) — all scenarios ─────────────────
png(file.path(plot.dir, paste0("exp_resProj_Fl", Fl, "_ALL.png")),
    width = 1000, height = 700)
par(cex.lab = cex, cex.axis = cex, cex.main = cex)
plot(future_yrs, exp_mean, type = "n", ylim = exp(c(-1.5, 1.5)),
     main = paste0(sp, " Fl", Fl, ": exp(AR(1) stationary) — all scenarios — 95% CI"),
     ylab = "exp(Residual)", xlab = "Year")
for (j in 1:nrow(resProj_all))
  lines(future_yrs, exp(resProj_all[j, ]), col = "grey90", lwd = 1)
polygon(c(future_yrs, rev(future_yrs)), c(exp_lower, rev(exp_upper)),
        col = rgb(0, 0, 0, 0.15), border = NA)
for (sc_idx in seq_along(sc))
  lines(future_yrs, apply(exp(resProj_list[[sc_idx]]), 2, mean),
        col = sc_cols[sc_idx], lwd = 2, lty = 2)
lines(future_yrs, exp_mean,              col = "black", lwd = 2, lty = 1)
lines(future_yrs, exp(resProj_all[1, ]), col = "green", lwd = 2, lty = 2)  # iter 1
abline(h = 1, lty = 3, col = "grey40")
legend("bottomleft",
       legend = c("Overall mean", nm, "Iter 1"),
       col = c("black", sc_cols, "green"),
       lty = c(1, rep(2, n_sc), 2),
       lwd = 2, bty = "n", cex = cex * 0.8)
dev.off()

# ── Plot 6: exponential scale — exp(randRes_corrected) — all scenarios ────────
png(file.path(plot.dir, paste0("exp_randRes_corrected_Fl", Fl, "_ALL.png")),
    width = 1000, height = 700)
par(cex.lab = cex, cex.axis = cex, cex.main = cex)
plot(future_yrs, exp_rc_mean, type = "n", ylim = exp(c(-1.5, 1.5)),
     main = paste0(sp, " Fl", Fl,
                   ": exp(randRes corrected) (no autocorrelation) — all scenarios — 95% CI"),
     ylab = "exp(Residual)", xlab = "Year")
for (j in 1:nrow(randRes_mat_all))
  lines(future_yrs, exp(randRes_mat_all[j, ]), col = "grey90", lwd = 1)
polygon(c(future_yrs, rev(future_yrs)), c(exp_rc_lower, rev(exp_rc_upper)),
        col = rgb(0, 0, 0, 0.15), border = NA)
for (sc_idx in seq_along(sc))
  lines(future_yrs, apply(exp(randRes_mat_list[[sc_idx]]), 2, mean),
        col = sc_cols[sc_idx], lwd = 2, lty = 2)
lines(future_yrs, exp_rc_mean,                col = "black", lwd = 2, lty = 1)
lines(future_yrs, exp(randRes_mat_all[1, ]),   col = "green", lwd = 2, lty = 2)  # iter 1
abline(h = 1, lty = 3, col = "grey40")
legend("bottomleft",
       legend = c("Overall mean", nm, "Iter 1"),
       col = c("black", sc_cols, "green"),
       lty = c(1, rep(2, n_sc), 2),
       lwd = 2, bty = "n", cex = cex * 0.8)
dev.off()

cat("══ All aggregated plots saved ══\n")
