# ============================================================
# Script: Aggregate_MSE_Output_Cluster_EmpHCR.R
#
# Purpose:
#   Aggregate FLBEIA outputs across all runs for a given
#   empirical HCR scenario (PCC, EMPW3, EMPW8 and their
#   robustness trials). Reference points are fixed (Ftarget,
#   Btarget, FFmsy = 1); BBmsy is extracted from the advice
#   covariate IvalGM. Designed to run as a SLURM array job.
#
# Inputs:
#   - Output_run_<nrun>.RData : FLBEIA output per run
#     (dir: FLoutput/2Steps/Emp/<scenario>/)
#
# Outputs:
#   - <sc_run>_AggregatedOutput_ALB.RData : aggregated
#     summaries across all runs for the scenario
#     (dir: FLoutput/Summary/Summary_Emp/)
#
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# MSE cluster aggregation — Empirical HCR scenarios
#
# Objectives:
#
#   1. Identify available runs for the selected scenario.
#
#   2. For each run, load the FLBEIA output and extract:
#        - Reference points for years 2025-2055:
#            Ftarget = 1, Btarget = 1, FFmsy = 1 (fixed),
#            BBmsy from advice covariate IvalGM.
#        - Summary tables: biological, fleet, metier, advice.
#
#   3. Accumulate all runs into scenario-level objects
#      (bio_sc, flt_sc, fltStk_sc, mt_sc, mtStk_sc, adv_sc).
#
#   4. Tag all outputs with the scenario label and save.
#
# Notes:
#
#   - sc_idx is set by SLURM_ARRAY_TASK_ID; set manually
#     for local testing.
#
#   - Scenarios covered: PCC2 and all combinations of
#     EMPW3/EMPW8 with robustness trials (_R0dw, _R0up,
#     _sigma). Base EMPW3/EMPW8 (without suffix) are not
#     included in this script.
#
#   - Unlike the SPiCT-based script, reference points here
#     are fixed (empirical HCR does not estimate Fmsy/Bmsy).
#     BBmsy is replaced by the IvalGM index ratio.
#
# ---------------------------------------------------------------------------


# =============================================================================
# SECTION 1 — SETUP
# =============================================================================

# Cluster library path
.libPaths(c("/scratch/aurtizberea/rlibs_new", .libPaths()))

library(FLBEIA)

# Source auxiliary functions here if needed, e.g.:
# source("name.R")


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

# Scenario index: set by SLURM on cluster; set manually for local testing
sc_idx <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting run sc_idx =", sc_idx, "\n")

# Base scenarios and robustness trials
scs     <- c("PCC2", "EMPW3", "EMPW8")
rob     <- c("_R0dw", "_R0up", "_sigma")
scs_rob <- expand.grid(scs = scs, rob = rob)

# Note: only PCC2 base included; EMPW3/EMPW8 base run via separate script
sc_nm  <- c("PCC2", paste0(scs_rob$scs, scs_rob$rob))
sc_run <- c("PCC2", paste0(scs_rob$scs, scs_rob$rob))

# Name of the FLBEIA result object inside each .RData file
FL_run_nm <- rep("AlbHCR_ind", 10)

# Stock name
stknm <- "ALB"

# Years for reference point extraction
ref_yrs <- 2025:2055

# Input / output directories (cluster paths)
dir_in  <- "/scratch/aurtizberea/ALB_MSE/FLoutput/2Steps/Emp"
dir_out <- "/scratch/aurtizberea/ALB_MSE/FLoutput/Summary/Summary_Emp"


# =============================================================================
# SECTION 3 — IDENTIFY AVAILABLE RUNS FOR THIS SCENARIO
# =============================================================================

file_nm <- list.files(file.path(dir_in, sc_run[sc_idx]))
result  <- sub(".*_", "", file_nm)
nruns   <- unique(sort(as.numeric(gsub(".*?([0-9]+).*", "\\1", result))))

FLout <- FL_run_nm[sc_idx]

cat("Scenario:", sc_run[sc_idx], "| Runs found:", length(nruns), "\n")


# =============================================================================
# SECTION 4 — INITIALISE ACCUMULATOR OBJECTS
# =============================================================================

brp_all   <- NULL
bio_sc    <- NULL
flt_sc    <- NULL
fltStk_sc <- NULL
mt_sc     <- NULL
mtStk_sc  <- NULL
adv_sc    <- NULL


# =============================================================================
# SECTION 5 — LOOP OVER RUNS: EXTRACT AND ACCUMULATE
# =============================================================================

for (nrun in nruns) {
  
  cat("  Processing run", nrun, "/", max(nruns), "\n")
  
  # --- 5.1  Load FLBEIA output ---
  load(file.path(dir_in, sc_run[sc_idx], paste0("Output_run_", nrun, ".RData")))
  s1 <- get(FLout)
  
  # --- 5.2  Build reference point table for this run ---
  # Note: empirical HCR does not estimate Fmsy/Bmsy from a model.
  # Ftarget, Btarget and FFmsy are set to 1 (normalised).
  # BBmsy is replaced by the IvalGM index ratio from the advice covariates.
  year     <- NULL
  stock    <- NULL
  iter_brp <- NULL
  Ftarget  <- NULL
  Btarget  <- NULL
  BBmsy    <- NULL
  FFmsy    <- NULL
  
  for (stk in stknm) {
    for (yr in ref_yrs) {
      yr_ac    <- as.character(yr)
      year     <- c(year,     yr)
      stock    <- c(stock,    stk)
      iter_brp <- c(iter_brp, nrun)
      Ftarget  <- c(Ftarget,  1)
      Btarget  <- c(Btarget,  1)
      BBmsy    <- c(BBmsy,    s1$advice$covars[[stk]]$IvalGM[, yr_ac])
      FFmsy    <- c(FFmsy,    1)
    }
  }
  
  # Assemble reference point data frame for this run
  brp <- data.frame(
    stock   = stock,
    iter    = iter_brp,
    year    = year,
    Ftarget = Ftarget,
    Btarget = Btarget,
    BBmsy   = BBmsy,
    FFmsy   = FFmsy,
    Bpa     = Btarget * 1,
    Blim    = Btarget * 0.4,
    Fpa     = Ftarget * 1,
    Flim    = Ftarget * 0.1
  )
  
  # --- 5.3  Compute FLBEIA summary tables ---
  flt    <- fltSum(s1,    long = TRUE)
  fltStk <- fltStkSum(s1, long = TRUE)
  mt     <- mtSum(s1,     long = TRUE)
  mtStk  <- mtStkSum(s1,  long = TRUE)
  adv    <- advSum(s1,    long = TRUE)
  
  # Use reference points from the last projection year for bioSum
  brp_sc      <- brp[brp$year == max(ref_yrs) & brp$iter == nrun, -3]
  brp_sc$iter <- 1
  bio <- bioSum(s1, brp = brp_sc, long = TRUE)
  
  # --- 5.4  Tag all summaries with run number ---
  bio$iter    <- nrun
  flt$iter    <- nrun
  fltStk$iter <- nrun
  mt$iter     <- nrun
  mtStk$iter  <- nrun
  adv$iter    <- nrun
  
  # --- 5.5  Accumulate across runs ---
  brp_all   <- rbind(brp_all,   brp)
  bio_sc    <- rbind(bio_sc,    bio)
  flt_sc    <- rbind(flt_sc,    flt)
  fltStk_sc <- rbind(fltStk_sc, fltStk)
  mt_sc     <- rbind(mt_sc,     mt)
  mtStk_sc  <- rbind(mtStk_sc,  mtStk)
  adv_sc    <- rbind(adv_sc,    adv)
}


# =============================================================================
# SECTION 6 — TAG WITH SCENARIO AND SAVE
# =============================================================================

brp_all$scenario   <- sc_nm[sc_idx]
bio_sc$scenario    <- sc_nm[sc_idx]
flt_sc$scenario    <- sc_nm[sc_idx]
fltStk_sc$scenario <- sc_nm[sc_idx]
mt_sc$scenario     <- sc_nm[sc_idx]
mtStk_sc$scenario  <- sc_nm[sc_idx]
adv_sc$scenario    <- sc_nm[sc_idx]

# Track which runs converged (all runs that produced output)
df_conv <- data.frame(iter_conv = nruns)

save(brp_all,
     bio_sc,
     flt_sc,
     fltStk_sc,
     mt_sc,
     mtStk_sc,
     adv_sc,
     df_conv,
     file = file.path(dir_out,
                      paste0(sc_run[sc_idx], "_AggregatedOutput_ALB.RData")))

cat("Scenario", sc_run[sc_idx], "saved successfully.\n")