# ============================================================
# Script: Run_2S33.R
#
# Purpose:
#   Run the FLBEIA projection stage (2026-2057) for scenario
#   2S33 using AlbHCR and SPiCT assessment. Identical to 2S31
#   except that Ftar = 1 (higher fishing target). Loads the
#   Rinit output as starting point, updates indices for 2025
#   using VPB/VPN, and runs the full projection.
#   Designed to run as a SLURM array job.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData  : FLBEIA input
#     data for the given iteration
#   - FLoutput/Rinit/Output_run_<nrun>.RData : Rinit output
#     (initialisation stage, 2022-2025)
#   - Auxiliary functions: VPNInd.R, VPBInd.R, AlbHCR.R,
#     spict2flbeiaALB.R
#
# Outputs:
#   - FLoutput/2Steps/MB_25var/2S33/Output_run_<nrun>.RData :
#     projection output including bio, flt, fltStk, mt,
#     mtStk, resInd and AlbHCR_spict objects
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# AlbHCR + SPiCT projection run — Scenario 2S33 (2026-2057)
#
# Objectives:
#
#   1. Load FLinput data and Rinit output for the assigned
#      iteration.
#
#   2. Set the projection period (2026-2057).
#
#   3. Build ICES HCR control as a basis, then configure
#      the custom AlbHCR control with its reference points.
#
#   4. Set up SPiCT observation and assessment controls
#      with 3-year index observation frequency.
#
#   5. Configure fleet control (landing obligation off,
#      discard TAC overshoot off).
#
#   6. Update indices for 2025 using VPB/VPN from Rinit.
#
#   7. Run FLBEIA for 2026-2057 with AlbHCR + SPiCT.
#
#   8. Compute and save summary tables.
#
# Notes:
#
#   - nrun is set by SLURM_ARRAY_TASK_ID.
#
#   - Key difference from 2S31: Ftar = 1 (vs. 0.8 in 2S31),
#     implying a higher fishing mortality target.
#
#   - Indices 4 and 5 use biomass-based VPB (catch weight
#     included); all others use numbers-based VPN.
#
#   - SRs is used unmodified from Rinit (no robustness trial).
#
# ---------------------------------------------------------------------------


# =============================================================================
# SECTION 1 — SETUP
# =============================================================================

.libPaths(c("/scratch/aurtizberea/rlibs_new", .libPaths()))

library(FLXSA)
library(FLAssess)
library(FLash)
library(FLCore)
library(FLFleet)
library(FLBEIA)
library(spict)

source(file.path("code", "Others", "VPNInd.R"))
source(file.path("code", "Others", "VPBInd.R"))
source(file.path("code", "Others", "AlbHCR.R"))
source(file.path("code", "Others", "spict2flbeiaALB.R"))


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

FLinput.data <- "FLinput/R1b"
in.data      <- "FLoutput/Rinit"
out.dir      <- "2S33"

nrun <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("Starting run nrun =", nrun, "\n")


# =============================================================================
# SECTION 3 — LOAD INPUT DATA
# =============================================================================

load(file.path(FLinput.data, paste0("FLinput_run_", nrun, ".RData")))
load(file.path(in.data,      paste0("Output_run_",  nrun, ".RData")))


# =============================================================================
# SECTION 4 — PROJECTION PERIOD
# =============================================================================

main.ctrl$sim.years["initial"] <- 2026
main.ctrl$sim.years["final"]   <- 2057


# =============================================================================
# SECTION 5 — ADVICE CONTROL
# =============================================================================

advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]

ni  <- 1
Btg <- 200000
Ftg <- 0.075

advice.ctrl.ICES <- advice.ctrl
HCR.models       <- rep("IcesHCR", 1)
stknms           <- c("ALB")

ref.pts.ALB <- matrix(c(0.075, 0.4 * Btg, 0.8 * Btg), 3, ni,
                      dimnames = list(c("Fmsy", "Blim", "Btrigger"), 1:ni))

advice.ctrl.ICES <- create.advice.ctrl(
  stksnames   = stknms,
  HCR.models  = HCR.models,
  ref.pts.ALB = ref.pts.ALB,
  first.yr    = 1930,
  last.yr     = 2057
)

# Note: Ftar = 1 (higher fishing target than 2S31 where Ftar = 0.8)
ref.pts.ALB <- matrix(c(1, 0.1, 0.4, 1, 0.25, 0.2, 50000), 7, ni,
                      dimnames = list(c("Ftar", "Fmin", "Blim", "Btrigger",
                                        "maxRange", "minRange", "maxTAC"), 1:ni))

advice.ctrl.AlbHCR                        <- advice.ctrl.ICES
advice.ctrl.AlbHCR$ALB$HCR.model          <- "AlbHCR"
advice.ctrl.AlbHCR$ALB$ref.pts            <- ref.pts.ALB
advice.ctrl.AlbHCR[["ALB"]][["adv.year"]] <- seq(2026, 2057, 3)
advice.ctrl.AlbHCR[[1]]$nyears[]          <- 3
advice.ctrl.AlbHCR[[1]]$AdvCatch          <- rep(TRUE, length(1930:2057))
names(advice.ctrl.AlbHCR[[1]]$AdvCatch)   <- as.character(1930:2057)


# =============================================================================
# SECTION 6 — OBSERVATION, ASSESSMENT AND FLEET CONTROLS
# =============================================================================

#........................................................
#...OBS.CTRL
#....................................................

obs.ctrl.spict <- obs.ctrl
obs.ctrl.spict$ALB$stkObs$stkObs.model   <- "age2bioDat"
obs.ctrl.spict$ALB$stkObs$land.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$disc.bio.error <- fleets[[1]]@effort
obs.ctrl.spict$ALB$stkObs$TAC.ovrsht    <- fleets[[1]]@effort

for (ind.nm in names(indices$ALB)) {
  obs.ctrl.spict$ALB$indObs[[ind.nm]]$yrs <- 3
}

#........................................................
#...ASSESS.CTRL
#....................................................

assess.spict                        <- assess.ctrl
assess.spict$ALB$assess.model       <- "spict2flbeiaALB"
assess.spict[["ALB"]]$harvest.units <- "f"
assess.spict[["ALB"]]$work_w_Iter   <- TRUE

#........................................................
#...FLEETS.CTRL
#....................................................

for (fl in names(fleets)) {
  fleets.ctrl.SMFB[[fl]]$LandObl <- FALSE
  for (st in names(fleets[[fl]]@metiers[[1]]@catches)) {
    fleets.ctrl.SMFB[[fl]][[st]]$discard.TAC.OS <- FALSE
  }
}


# =============================================================================
# SECTION 7 — UPDATE INDICES FOR 2025 (VPB/VPN)
# =============================================================================

biol <- Rinit$biols$ALB

it         <- dim(biol@n)[6]
ns         <- dim(biol@n)[4]
obs.yrs    <- 2025
index.proj <- c(1, 3:7)

for (j in index.proj) {
  year   <- obs.yrs - 1930
  index  <- Rinit$indices$ALB[[j]]
  fleets <- Rinit$fleets
  
  yrnm.1 <- dimnames(biol@n)[[2]][year]
  sInd   <- 1
  
  if (j %in% 4:5) {
    # Biomass-based index: includes catch weight
    VPB <- (biol@n[, yrnm.1, , sInd, ] * exp(-biol@m[, yrnm.1, , sInd, ] / 2) -
              landStock(fleets, name(biol))[, yrnm.1, , sInd, ] / 2) *
      index@sel.pattern[, yrnm.1, , sInd, ] * index@catch.wt[, yrnm.1, , sInd, ]
  } else {
    # Numbers-based index: no catch weight
    VPB <- (biol@n[, yrnm.1, , sInd, ] * exp(-biol@m[, yrnm.1, , sInd, ] / 2) -
              landStock(fleets, name(biol))[, yrnm.1, , sInd, ] / 2) *
      index@sel.pattern[, yrnm.1, , sInd, ]
  }
  
  B <- quantSums(VPB[, yrnm.1, , sInd, ])
  Rinit$indices$ALB[[j]]@index[, yrnm.1] <- B * index@index.q[, yrnm.1]
}


# =============================================================================
# SECTION 8 — RUN FLBEIA (AlbHCR + SPiCT)
# =============================================================================

AlbHCR_spict <- FLBEIA(
  biols       = Rinit$biols,
  SRs         = Rinit$SRs,
  BDs         = NULL,
  fleets      = Rinit$fleets,
  covars      = NULL,
  indices     = Rinit$indices,
  advice      = Rinit$advice,
  main.ctrl   = main.ctrl,
  biols.ctrl  = biols.ctrl,
  fleets.ctrl = fleets.ctrl.SMFB,
  covars.ctrl = covars.ctrl,
  obs.ctrl    = obs.ctrl.spict,
  assess.ctrl = assess.spict,
  advice.ctrl = advice.ctrl.AlbHCR
)


# =============================================================================
# SECTION 9 — SUMMARISE AND SAVE
# =============================================================================

bio    <- bioSum(AlbHCR_spict, byyear = TRUE, ssb_season = 1)
flt    <- fltSum(AlbHCR_spict, long = TRUE)
fltStk <- fltStkSum(AlbHCR_spict, long = TRUE)
mt     <- mtSum(AlbHCR_spict, long = TRUE)
mtStk  <- mtStkSum(AlbHCR_spict, long = TRUE)

bio$iter    <- nrun
flt$iter    <- nrun
fltStk$iter <- nrun
mt$iter     <- nrun
mtStk$iter  <- nrun

resInd <- AlbHCR_spict$indices

save(resInd, bio, flt, fltStk, mt, mtStk, AlbHCR_spict,
     file = file.path("FLoutput", "2Steps", "MB_25var", out.dir,
                      paste0("Output_run_", nrun, ".RData")))

cat("Run", nrun, "saved successfully.\n")