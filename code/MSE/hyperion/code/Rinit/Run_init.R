# ============================================================
# Script: Run_Rinit_Cluster.R
#
# Purpose:
#   Run the FLBEIA initialisation stage (Rinit, 2022-2025)
#   for a single iteration using SPiCT as assessment model.
#   Designed to run as a SLURM array job on the cluster,
#   one job per iteration.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData : FLBEIA input
#     data for the given iteration
#   - Auxiliary functions: VPNInd.R, VPBInd.R, AlbHCR.R,
#     spict2flbeiaALB.R
#
# Outputs:
#   - FLoutput/Rinit/Output_run_<nrun>.RData : FLBEIA Rinit
#     output including bio, adv, flt, fltStk, mt, mtStk,
#     resInd and Rinit objects
#
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Rinit cluster run — SPiCT assessment
#
# Objectives:
#
#   1. Load the FLinput data for the assigned iteration.
#
#   2. Set up observation and assessment controls for SPiCT.
#
#   3. Configure fleet control (landing obligation off,
#      discard TAC overshoot off).
#
#   4. Set fixed TAC values for the initialisation period
#      (2022-2026) and deactivate unused index.q series.
#
#   5. Run FLBEIA for 2022-2025 (Rinit stage).
#
#   6. Compute and save summary tables and indices.
#
# Notes:
#
#   - nrun is set by SLURM_ARRAY_TASK_ID.
#
#   - Natural mortality correction (biolsMOD@m * 7/12) is
#     commented out in this version; uncomment if needed.
#
#   - advice.ctrl from the input file is used directly
#     (no custom AlbHCR advice control in this stage).
#
# ---------------------------------------------------------------------------


# =============================================================================
# SECTION 1 — SETUP
# =============================================================================

# Cluster library path
.libPaths(c("/scratch/aurtizberea/rlibs_new", .libPaths()))

library(FLXSA)
library(FLAssess)
library(FLash)
library(FLCore)
library(FLFleet)
library(FLBEIA)
# library(ss3om)
library(spict)

# Source auxiliary functions
source(file.path("code", "Others", "VPNInd.R"))
source(file.path("code", "Others", "VPBInd.R"))
source(file.path("code", "Others", "AlbHCR.R"))
source(file.path("code", "Others", "spict2flbeiaALB.R"))


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

# Iteration index: set by SLURM on cluster
nrun <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting run nrun =", nrun, "\n")

# Input / output directories
in.data <- "FLinput/R1b"
out.dir <- "Rinit"

# Simulation window
yr_start <- 2022
yr_end   <- 2025

# Fixed TAC values for initialisation period (2022-2026)
tac_fixed <- c(31601, 28115, 23800, 47251, 47251)


# =============================================================================
# SECTION 3 — LOAD INPUT DATA
# =============================================================================

load(file.path(in.data, paste0("FLinput_run_", nrun, ".RData")))

main.ctrl$sim.years["initial"] <- yr_start
main.ctrl$sim.years["final"]   <- yr_end


# =============================================================================
# SECTION 4 — CONFIGURE INPUTS
# =============================================================================

# --- 4.1  Natural mortality correction (uncomment if needed) ---
# biolsMOD$ALB@m[1, ] <- biols$ALB@m[1, ] * 7 / 12

# --- 4.2  Deactivate index.q for indices with no projection data ---
indices$ALB$JPLLN@index.q[, as.character(2010:2057)] <- NA
indices$ALB$VENLL@index.q[, as.character(2018:2057)] <- NA

# --- 4.3  Set TAC: historical from observed landings + fixed 2022-2026 ---
advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]
advice$TAC[, as.character(2022:2026)]  <- tac_fixed

# --- 4.4  Observation control (SPiCT) ---
obs.ctrl.spict <- obs.ctrl
obs.ctrl.spict$ALB$stkObs$stkObs.model   <- "age2bioDat"
obs.ctrl.spict$ALB$stkObs$land.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$disc.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$TAC.ovrsht    <- fleets[[1]]@effort

for (ind.nm in names(indices$ALB)) {
  obs.ctrl.spict$ALB$indObs[[ind.nm]]$yrs <- 3
}

# --- 4.5  Assessment control (SPiCT) ---
assess.spict                        <- assess.ctrl
assess.spict$ALB$assess.model       <- "spict2flbeiaALB"
assess.spict[["ALB"]]$harvest.units <- "f"
assess.spict[["ALB"]]$work_w_Iter   <- TRUE

# --- 4.6  Fleet control: disable landing obligation and discard TAC overshoot ---

for (fl in names(fleets)) {
  fleets.ctrl.SMFB[[fl]]$LandObl <- FALSE
  for (st in names(fleets[[fl]]@metiers[[1]]@catches)) {
    fleets.ctrl.SMFB[[fl]][[st]]$discard.TAC.OS <- FALSE
  }
}


# =============================================================================
# SECTION 5 — RUN FLBEIA (Rinit stage)
# =============================================================================

Rinit <- FLBEIA(
  biols       = biolsMOD,
  SRs         = SRs,
  BDs         = NULL,
  fleets      = fleets,
  covars      = NULL,
  indices     = indices,
  advice      = advice,
  main.ctrl   = main.ctrl,
  biols.ctrl  = biols.ctrl,
  fleets.ctrl = fleets.ctrl.SMFB,
  covars.ctrl = covars.ctrl,
  obs.ctrl    = obs.ctrl.spict,
  assess.ctrl = assess.spict,
  advice.ctrl = advice.ctrl
)


# =============================================================================
# SECTION 6 — SUMMARISE AND SAVE
# =============================================================================

bio    <- bioSum(Rinit, byyear = TRUE, ssb_season = 1)
plotbioSum(bio)
bio[bio$year > 2020, ]
flt    <- fltSum(Rinit, long = TRUE)
fltStk <- fltStkSum(Rinit, long = TRUE)
mt     <- mtSum(Rinit, long = TRUE)
mtStk  <- mtStkSum(Rinit, long = TRUE)
adv    <- advSum(Rinit, long = TRUE)

bio$iter    <- nrun
flt$iter    <- nrun
fltStk$iter <- nrun
mt$iter     <- nrun
mtStk$iter  <- nrun
adv$iter    <- nrun

resInd <- Rinit$indices

save(resInd, bio, adv, flt, fltStk, mt, mtStk, Rinit,
     file = file.path("FLoutput", out.dir,
                      paste0("Output_run_", nrun, ".RData")))

cat("Run", nrun, "saved successfully.\n")