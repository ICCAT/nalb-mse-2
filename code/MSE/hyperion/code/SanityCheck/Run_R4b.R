# ============================================================
# Script: Run_4b.R
#
# Purpose:
#   Run FLBEIA with the AlbHCR management procedure and SPiCT
#   assessment for a single iteration on the cluster. This is
#   a direct projection run (2022-2050), without an explicit
#   initialisation stage. Designed to run as a SLURM array
#   job, one job per iteration.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData : FLBEIA input
#     data for the given iteration
#   - Auxiliary functions: VPNInd.R, VPBInd.R, AlbHCR.R,
#     spict2flbeiaALB.R
#
# Outputs:
#   - FLoutput/R4b/Output_run_<nrun>.RData : FLBEIA output
#     including bio, adv, flt, fltStk, mt, mtStk,
#     resInd and AlbHCR_spict objects
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# AlbHCR + SPiCT projection run (2022-2050)
#
# Objectives:
#
#   1. Load the FLinput data for the assigned iteration.
#
#   2. Build ICES HCR control as a basis, then configure
#      the custom AlbHCR control with its reference points.
#
#   3. Set up SPiCT observation and assessment controls
#      with 3-year index observation frequency.
#
#   4. Set historical TAC from observed landings and
#      deactivate unused index.q series (JPLLN, VENLL).
#
#   5. Run FLBEIA for 2022-2050 with AlbHCR + SPiCT.
#
#   6. Compute and save summary tables.
#
# Notes:
#
#   - nrun is set by SLURM_ARRAY_TASK_ID.
#
#   - Advice years start from 2022 (every 3 years), unlike
#     the 2-stage scripts where advice starts from 2026.
#
#   - AlbHCR reference points: Ftar = 0.8, Btrigger = 1
#     (relative), Blim = 0.4, maxTAC = 50000.
#
#   - Natural mortality correction (x 7/12) is not applied
#     in this script (unlike Rinit scripts).
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
out.dir <- "R4b"

# Simulation window
yr_start <- 2022
yr_end   <- 2050

# ICES reference points
ni  <- 1
Btg <- 200000
Ftg <- 0.075

# Stock name
stknms <- "ALB"


# =============================================================================
# SECTION 3 — LOAD INPUT DATA
# =============================================================================

load(file.path(in.data, paste0("FLinput_run_", nrun, ".RData")))

main.ctrl$sim.years["initial"] <- yr_start
main.ctrl$sim.years["final"]   <- yr_end


# =============================================================================
# SECTION 4 — BUILD ADVICE CONTROL OBJECTS
# =============================================================================

# --- 4.1  ICES HCR (basis for AlbHCR) ---
ref.pts.ALB <- matrix(c(Ftg, 0.4 * Btg, 0.8 * Btg), 3, ni,
                      dimnames = list(c("Fmsy", "Blim", "Btrigger"), 1:ni))

advice.ctrl.ICES <- create.advice.ctrl(
  stksnames   = stknms,
  HCR.models  = rep("IcesHCR", 1),
  ref.pts.ALB = ref.pts.ALB,
  first.yr    = 1930,
  last.yr     = yr_end
)
advice.ctrl.ICES$ALB$intermediate.year <- "catch"

# --- 4.2  AlbHCR (custom management procedure) ---
# Note: Ftar = 0.8 (relative), Btrigger = 1 (relative)
ref.pts.ALB <- matrix(c(0.8, 0.1, 0.4, 1, 0.25, 0.2, 50000), 7, ni,
                      dimnames = list(c("Ftar", "Fmin", "Blim", "Btrigger",
                                        "maxRange", "minRange", "maxTAC"), 1:ni))

advice.ctrl.AlbHCR                        <- advice.ctrl.ICES
advice.ctrl.AlbHCR$ALB$HCR.model          <- "AlbHCR"
advice.ctrl.AlbHCR$ALB$ref.pts            <- ref.pts.ALB
advice.ctrl.AlbHCR[["ALB"]][["adv.year"]] <- seq(yr_start, yr_end, 3)
advice.ctrl.AlbHCR[[1]]$nyears[]          <- 3
advice.ctrl.AlbHCR[[1]]$AdvCatch          <- rep(TRUE, length(1930:yr_end))
names(advice.ctrl.AlbHCR[[1]]$AdvCatch)   <- as.character(1930:yr_end)


# =============================================================================
# SECTION 5 — CONFIGURE INPUTS
# =============================================================================

# --- 5.1  Observation control (SPiCT, 3-year index frequency) ---
obs.ctrl.spict <- obs.ctrl
obs.ctrl.spict$ALB$stkObs$stkObs.model   <- "age2bioDat"
obs.ctrl.spict$ALB$stkObs$land.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$disc.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$TAC.ovrsht    <- fleets[[1]]@effort

for (ind.nm in names(indices$ALB)) {
  obs.ctrl.spict$ALB$indObs[[ind.nm]]$yrs <- 3
}

# --- 5.2  Assessment control (SPiCT) ---
assess.spict                        <- assess.ctrl
assess.spict$ALB$assess.model       <- "spict2flbeiaALB"
assess.spict[["ALB"]]$harvest.units <- "f"
assess.spict[["ALB"]]$work_w_Iter   <- TRUE

# --- 5.3  TAC: historical from observed landings ---
advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]

# --- 5.4  Deactivate index.q for indices with no projection data ---
indices$ALB$JPLLN@index.q[, as.character(2010:yr_end)] <- NA
indices$ALB$VENLL@index.q[, as.character(2018:yr_end)] <- NA


# =============================================================================
# SECTION 6 — RUN FLBEIA (AlbHCR + SPiCT)
# =============================================================================

AlbHCR_spict <- FLBEIA(
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
  advice.ctrl = advice.ctrl.AlbHCR
)


# =============================================================================
# SECTION 7 — SUMMARISE AND SAVE
# =============================================================================

bio    <- bioSum(AlbHCR_spict, byyear = TRUE, ssb_season = 1)
flt    <- fltSum(AlbHCR_spict, long = TRUE)
fltStk <- fltStkSum(AlbHCR_spict, long = TRUE)
mt     <- mtSum(AlbHCR_spict, long = TRUE)
mtStk  <- mtStkSum(AlbHCR_spict, long = TRUE)
adv    <- advSum(AlbHCR_spict, long = TRUE)   

bio$iter    <- nrun
flt$iter    <- nrun
fltStk$iter <- nrun
mt$iter     <- nrun
mtStk$iter  <- nrun
adv$iter    <- nrun                           

resInd <- AlbHCR_spict$indices

save(resInd, bio, adv, flt, fltStk, mt, mtStk, AlbHCR_spict,
     file = file.path("FLoutput", out.dir,
                      paste0("Output_run_", nrun, ".RData")))

cat("Run", nrun, "saved successfully.\n")