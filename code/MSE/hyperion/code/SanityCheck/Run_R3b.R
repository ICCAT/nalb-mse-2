# ============================================================
# Script: Run_R3b.R
#
# Purpose:
#   Run FLBEIA with fixed effort and fixed advice (TACF
#   scenario) using SPiCT as assessment model. Fleets operate
#   at their input effort level with no HCR applied. Designed
#   to run as a SLURM array job, one job per iteration.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData : FLBEIA input
#     data for the given iteration
#   - Auxiliary functions: VPNInd.R, VPBInd.R, AlbHCR.R,
#     spict2flbeiaALB.R
#
# Outputs:
#   - FLoutput/R3b/Output_run_<nrun>.RData : FLBEIA fixed-TAC
#     output including bio, adv, flt, fltStk, mt, mtStk,
#     resInd and TACF_spict objects
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Fixed-TAC reference run — SPiCT assessment, fixed advice
#
# Objectives:
#
#   1. Load the FLinput data for the assigned iteration.
#
#   2. Set up observation and assessment controls for SPiCT.
#      Index observation frequency set to 1 year (annual).
#
#   3. Configure fleet control:
#        - Effort model set to fixedEffort (actual fleets used).
#        - Landing obligation off, discard TAC overshoot off.
#        - Advice control set to fixedAdvice (no HCR applied).
#
#   4. Deactivate unused index.q series (JPLLN, VENLL).
#
#   5. Set historical TAC from observed landings.
#
#   6. Adjust natural mortality (7/12 seasonal correction).
#
#   7. Run FLBEIA for 2022-2050 and save summary tables.
#
# Notes:
#
#   - nrun is set by SLURM_ARRAY_TASK_ID.
#
#   - Unlike Run_Ef0_Cluster.R, this scenario uses actual
#     fleet effort (not zeroed). The effort model is fixed
#     (fixedEffort) so effort does not respond to the HCR.
#
#   - Index observation frequency is 1 year (annual), unlike
#     the 3-year frequency used in the projection scripts.
#
#   - Natural mortality is seasonally corrected (x 7/12).
#
# ---------------------------------------------------------------------------


# =============================================================================
# SECTION 1 — SETUP
# =============================================================================

# Cluster library path
lib.dir <- file.path("/scratch/aurtizberea/rlibs")
.libPaths(c(.libPaths(), lib.dir))
.libPaths(.libPaths()[grep(lib.dir, .libPaths())])

library(FLXSA)
library(FLAssess)
library(FLash)
library(FLCore)
library(FLFleet)
library(FLBEIA)
# library(ss3om)
library(spict)

# Source auxiliary functions
source("VPNInd.R")
source("VPBInd.R")
source("AlbHCR.R")
source("spict2flbeiaALB.R")


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

# Iteration index: set by SLURM on cluster
nrun <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting run nrun =", nrun, "\n")

# Input / output directories
in.data <- "FLinput/R1b"
out.dir <- "R3b"

# Simulation window
yr_start <- 2022
yr_end   <- 2050


# =============================================================================
# SECTION 3 — LOAD INPUT DATA
# =============================================================================

load(file.path(in.data, paste0("FLinput_run_", nrun, ".RData")))

main.ctrl$sim.years["initial"] <- yr_start
main.ctrl$sim.years["final"]   <- yr_end


# =============================================================================
# SECTION 4 — CONFIGURE INPUTS
# =============================================================================

# --- 4.1  Deactivate index.q for indices with no projection data ---
indices$ALB$JPLLN@index.q[, as.character(2010:yr_end)] <- NA
indices$ALB$VENLL@index.q[, as.character(2018:yr_end)] <- NA

# --- 4.2  Set TAC: historical from observed landings ---
advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]

# --- 4.3  Observation control (SPiCT, annual index frequency) ---
obs.ctrl.spict <- obs.ctrl
obs.ctrl.spict$ALB$stkObs$stkObs.model   <- "age2bioDat"
obs.ctrl.spict$ALB$stkObs$land.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$disc.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$TAC.ovrsht    <- fleets[[1]]@effort

for (ind.nm in names(indices$ALB)) {
  obs.ctrl.spict$ALB$indObs[[ind.nm]]$yrs <- 1  # annual (vs. 3-year in projection runs)
}

# --- 4.4  Assessment control (SPiCT) ---
assess.spict                        <- assess.ctrl
assess.spict$ALB$assess.model       <- "spict2flbeiaALB"
assess.spict[["ALB"]]$harvest.units <- "f"
assess.spict[["ALB"]]$work_w_Iter   <- TRUE

# --- 4.5  Fixed effort model: actual fleets, effort does not respond to HCR ---
fleets.ctrl.fixed <- fleets.ctrl.SMFB
for (fl in names(fleets)) {
  fleets.ctrl.fixed[[fl]]$effort.model[] <- "fixedEffort"
  fleets.ctrl.fixed[[fl]]$LandObl        <- FALSE
  for (st in names(fleets[[fl]]@metiers[[1]]@catches)) {
    fleets.ctrl.fixed[[fl]][[st]]$discard.TAC.OS <- FALSE
  }
}

# --- 4.6  Fixed advice control (no HCR applied) ---
advice.ctrl <- create.advice.ctrl(
  stksnames  = "ALB",
  HCR.models = rep("fixedAdvice", length("ALB"))
)

# --- 4.7  Natural mortality seasonal correction (7 of 12 months) ---
biolsMOD$ALB@m[1, ] <- biols$ALB@m[1, ] * 7 / 12


# =============================================================================
# SECTION 5 — RUN FLBEIA (fixed-TAC reference)
# =============================================================================

TACF_spict <- FLBEIA(
  biols       = biolsMOD,
  SRs         = SRs,
  BDs         = NULL,
  fleets      = fleets,
  covars      = NULL,
  indices     = indices,
  advice      = advice,
  main.ctrl   = main.ctrl,
  biols.ctrl  = biols.ctrl,
  fleets.ctrl = fleets.ctrl.fixed,
  covars.ctrl = covars.ctrl,
  obs.ctrl    = obs.ctrl.spict,
  assess.ctrl = assess.spict,
  advice.ctrl = advice.ctrl
)


# =============================================================================
# SECTION 6 — SUMMARISE AND SAVE
# =============================================================================

bio    <- bioSum(TACF_spict, byyear = TRUE, ssb_season = 1)
flt    <- fltSum(TACF_spict, long = TRUE)
fltStk <- fltStkSum(TACF_spict, long = TRUE)
mt     <- mtSum(TACF_spict, long = TRUE)
mtStk  <- mtStkSum(TACF_spict, long = TRUE)
adv    <- advSum(TACF_spict, long = TRUE)   

bio$iter    <- nrun
flt$iter    <- nrun
fltStk$iter <- nrun
mt$iter     <- nrun
mtStk$iter  <- nrun
adv$iter    <- nrun                         

resInd <- TACF_spict$indices

save(resInd, bio, adv, flt, fltStk, mt, mtStk, TACF_spict,  
     file = file.path("FLoutput", out.dir,
                      paste0("Output_run_", nrun, ".RData")))

cat("Run", nrun, "saved successfully.\n")