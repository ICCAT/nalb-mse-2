# ============================================================
# Script: Aggregate_Indices_Cluster_EmpHCR.R
#
# Purpose:
#   Aggregate simulated CPUE indices across all runs for a
#   given empirical HCR scenario (EMPW3, EMPW5, EMPW8).
#   Produces multi-iteration FLQuant objects for each index,
#   ready for plotting and diagnostic analysis.
#   Designed to run as a SLURM array job on the cluster.
#
# Inputs:
#   - Output_run_<nrun>.RData : FLBEIA output per run,
#     containing resInd and AlbHCR_ind objects
#     (dir: FLoutput/2Steps/Emp/<scenario>/)
#
# Outputs:
#   - Output_res.RData : multi-iteration FLQuant objects
#     for all indices (BB, JPLLN, JPLLS, TAILLN, TAILLS,
#     USLLN, USLLS, VENLL, J, Jrat) and aggregated bio
#     (dir: FLoutput/2Steps/Emp/<scenario>/)
#
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Index aggregation — Empirical HCR scenarios
#
# Objectives:
#
#   1. Identify all available runs for the selected scenario.
#
#   2. Load run 1 and initialise multi-iteration FLQuant
#      objects for each index using propagate().
#
#   3. Loop over remaining runs and fill each iteration slot
#      with the corresponding run's index trajectory.
#
#   4. Repeat for composite indices J (aggregate) and Jrat
#      (index ratio), extracted from advice covariates.
#
#   5. Save all multi-iteration objects to Output_res.RData.
#
# Notes:
#
#   - sc.i is set by SLURM_ARRAY_TASK_ID; set manually
#     for local testing.
#
#   - Indices covered: BB, JPLLN, JPLLS, TAILLN, TAILLS,
#     USLLN, USLLS, VENLL, J (aggregate), Jrat (ratio).
#
#   - The number of iterations is derived automatically
#     from the number of available runs.
#
#   - J and Jrat come from AlbHCR_ind$advice$covars,
#     not from resInd (which stores individual indices).
#
# ---------------------------------------------------------------------------


# =============================================================================
# SECTION 1 — SETUP
# =============================================================================

# Cluster library path
.libPaths(c("/scratch/aurtizberea/rlibs_new", .libPaths()))

library(FLBEIA)


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

# Scenario index: set by SLURM on cluster; set manually for local testing
sc.i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting sc.i =", sc.i, "\n")

# Scenarios and associated FLBEIA result object name
sc_run    <- c("EMPW3", "EMPW5", "EMPW8")
FL_run_nm <- rep("AlbHCR_ind", 3)

# Selected scenario for this job
out.dir <- sc_run[sc.i]
FLout   <- FL_run_nm[sc.i]

# Year range for J and Jrat composite indices
J_yrs <- as.character(1981:2056)

# Input / output base directory
base_dir <- "FLoutput/2Steps/Emp"


# =============================================================================
# SECTION 3 — IDENTIFY AVAILABLE RUNS
# =============================================================================

file_nm <- list.files(file.path(base_dir, out.dir))
runs    <- unique(sort(as.numeric(gsub(".*?([0-9]+).*", "\\1", file_nm))))
n_iter  <- length(runs)   # used to set the number of iterations in propagate()

cat("Scenario:", out.dir, "| Runs found:", n_iter, "\n")


# =============================================================================
# SECTION 4 — LOAD RUN 1 AND INITIALISE MULTI-ITERATION OBJECTS
# =============================================================================

nrun <- runs[1]
cat("  Initialising from run", nrun, "\n")

load(file.path(base_dir, out.dir, paste0("Output_run_", nrun, ".RData")))

s1      <- get(FLout)
indices <- s1$indices

# Initialise multi-iteration FLQuants (one slot per run)
BB.ind.all     <- propagate(indices$ALB$BB@index,     n_iter, fill.iter = FALSE)
JPLLN.ind.all  <- propagate(indices$ALB$JPLLN@index,  n_iter, fill.iter = FALSE)
JPLLS.ind.all  <- propagate(indices$ALB$JPLLS@index,  n_iter, fill.iter = FALSE)
TAILLN.ind.all <- propagate(indices$ALB$TAILLN@index, n_iter, fill.iter = FALSE)
TAILLS.ind.all <- propagate(indices$ALB$TAILLS@index, n_iter, fill.iter = FALSE)
USLLN.ind.all  <- propagate(indices$ALB$USLLN@index,  n_iter, fill.iter = FALSE)
USLLS.ind.all  <- propagate(indices$ALB$USLLS@index,  n_iter, fill.iter = FALSE)
VENLL.ind.all  <- propagate(indices$ALB$VENLL@index,  n_iter, fill.iter = FALSE)
J.ind.all      <- propagate(indices$ALB$BB@index,     n_iter, fill.iter = FALSE)
Jrat.ind.all   <- propagate(indices$ALB$BB@index,     n_iter, fill.iter = FALSE)

# Fill iteration 1 with run 1 values
iter(BB.ind.all,     nrun) <- resInd$ALB$BB@index
iter(JPLLN.ind.all,  nrun) <- resInd$ALB$JPLLN@index
iter(JPLLS.ind.all,  nrun) <- resInd$ALB$JPLLS@index
iter(TAILLN.ind.all, nrun) <- resInd$ALB$TAILLN@index
iter(TAILLS.ind.all, nrun) <- resInd$ALB$TAILLS@index
iter(USLLN.ind.all,  nrun) <- resInd$ALB$USLLN@index
iter(USLLS.ind.all,  nrun) <- resInd$ALB$USLLS@index
iter(VENLL.ind.all,  nrun) <- resInd$ALB$VENLL@index
iter(J.ind.all,      nrun)[, J_yrs] <- AlbHCR_ind$advice$covars$ALB[["Ind_J"]]
iter(Jrat.ind.all,   nrun)[, J_yrs] <- AlbHCR_ind$advice$covars$ALB[["Jrat"]]

# Initialise bio accumulator
outdf <- bio


# =============================================================================
# SECTION 5 — LOOP OVER REMAINING RUNS
# =============================================================================

for (nrun in runs[-1]) {
  
  cat("  Processing run", nrun, "/", max(runs), "\n")
  
  load(file.path(base_dir, out.dir, paste0("Output_run_", nrun, ".RData")))
  
  outdf <- rbind(bio, outdf)
  
  iter(BB.ind.all,     nrun) <- resInd$ALB$BB@index
  iter(JPLLN.ind.all,  nrun) <- resInd$ALB$JPLLN@index
  iter(JPLLS.ind.all,  nrun) <- resInd$ALB$JPLLS@index
  iter(TAILLN.ind.all, nrun) <- resInd$ALB$TAILLN@index
  iter(TAILLS.ind.all, nrun) <- resInd$ALB$TAILLS@index
  iter(USLLN.ind.all,  nrun) <- resInd$ALB$USLLN@index
  iter(USLLS.ind.all,  nrun) <- resInd$ALB$USLLS@index
  iter(VENLL.ind.all,  nrun) <- resInd$ALB$VENLL@index
  iter(J.ind.all,      nrun)[, J_yrs] <- AlbHCR_ind$advice$covars$ALB[["Ind_J"]]
  iter(Jrat.ind.all,   nrun)[, J_yrs] <- AlbHCR_ind$advice$covars$ALB[["Jrat"]]
}


# =============================================================================
# SECTION 6 — SAVE
# =============================================================================

save(outdf,
     BB.ind.all,
     JPLLN.ind.all,
     JPLLS.ind.all,
     TAILLN.ind.all,
     TAILLS.ind.all,
     USLLN.ind.all,
     USLLS.ind.all,
     VENLL.ind.all,
     J.ind.all,
     Jrat.ind.all,
     file = file.path(base_dir, out.dir, "Output_res.RData"))

cat("Scenario", out.dir, "saved successfully.\n")