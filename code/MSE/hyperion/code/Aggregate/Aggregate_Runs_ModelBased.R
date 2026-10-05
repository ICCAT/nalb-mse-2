# ============================================================
# Script: Aggregate_Runs_ModelBased.R
#
# Purpose:
#   Aggregate FLBEIA outputs across all runs for a given
#   scenario, extract SPiCT-based reference points per year,
#   and save combined summaries for downstream analysis.
#   Designed to run as a SLURM array job on the cluster.
#
# Inputs:
#   - Output_run_<nrun>.RData : FLBEIA output per run
#     (dir: FLoutput/2Steps/MB_25var/<scenario>/)
#
# Outputs:
#   - <sc_run>_AggregatedOutput_ALB.RData : aggregated
#     summaries across all runs for the scenario
#     (dir: FLoutput/Summary/Summary_MB25var/)
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# MSE cluster aggregation
#
# Objectives:
#
#   1. Identify available runs for the selected scenario.
#
#   2. For each run, load the FLBEIA output and extract:
#        - SPiCT reference points (Fmsy, Bmsy, B/Bmsy, F/Fmsy)
#          for years 2025-2055.
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
#   - Scenarios covered: 2S31, 2S33 and all combinations
#     with robustness trials (_R0dw, _R0up, _sigma).
#
#   - brp reference points are extracted from SPiCT covars
#     stored inside the FLBEIA output object.
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
sc_idx <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting run sc_idx =", sc_idx, "\n")

# Base scenarios and robustness trials
scs     <- c("2S31", "2S33")
rob     <- c("_R0dw", "_R0up", "_sigma")
scs_rob <- expand.grid(scs = scs, rob = rob)

sc_nm  <- c("2S31", "2S33", paste0(scs_rob$scs, scs_rob$rob))
sc_run <- c("2S31", "2S33", paste0(scs_rob$scs, scs_rob$rob))

# Name of the FLBEIA result object inside each .RData file
FL_run_nm <- rep("AlbHCR_spict", 12)

# Stock name
stknm <- "ALB"

# Years for reference point extraction
ref_yrs <- 2025:2055

# Input / output directories (cluster paths)
dir_in  <- "/scratch/aurtizberea/ALB_MSE/FLoutput/2Steps/MB_25var"
dir_out <- "/scratch/aurtizberea/ALB_MSE/FLoutput/Summary/Summary_MB25var"


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

brp_all  <- NULL
bio_sc   <- NULL
flt_sc   <- NULL
fltStk_sc <- NULL
mt_sc    <- NULL
mtStk_sc <- NULL
adv_sc   <- NULL


# =============================================================================
# SECTION 5 — LOOP OVER RUNS: EXTRACT AND ACCUMULATE
# =============================================================================

for (nrun in nruns) {
  
  cat("  Processing run", nrun, "/", max(nruns), "\n")
  
  # --- 5.1  Load FLBEIA output ---
  load(file.path(dir_in, sc_run[sc_idx], paste0("Output_run_", nrun, ".RData")))
  s1 <- get(FLout)
  
  # --- 5.2  Extract SPiCT reference points for each year ---
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
      Ftarget  <- c(Ftarget,  s1$covars[[stk]]$spict_Fmsy[, yr_ac])
      Btarget  <- c(Btarget,  s1$covars[[stk]]$spict_Bmsy[, yr_ac])
      BBmsy    <- c(BBmsy,    s1$covars[[stk]]$spict_BBmsy[, yr_ac])
      FFmsy    <- c(FFmsy,    s1$covars[[stk]]$spict_FFmsy[, yr_ac])
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
  
  # Use reference points from the last projection year (2055) for bioSum
  brp_sc       <- brp[brp$year == max(ref_yrs) & brp$iter == nrun, -3]
  brp_sc$iter  <- 1
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
df_conv           <- data.frame(iter_conv = nruns)

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