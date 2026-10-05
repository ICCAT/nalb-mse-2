# ============================================================
# Script: Run_EMPW8.R
#
# Purpose:
#   Run the FLBEIA projection stage (2026-2057) for scenario
#   EMPW8 using an empirical aggregate index HCR
#   (ALB_AggInd_HCR). Identical to EMPW3 except that TAC
#   interannual change is constrained to ±10%
#   (maxRange = minRange = 0.1). No stock assessment model
#   is used; observation is set to perfectObs.
#   Designed to run as a SLURM array job.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData  : FLBEIA input
#     data for the given iteration
#   - FLoutput/Rinit/Output_run_<nrun>.RData : Rinit output
#     (initialisation stage, 2022-2025)
#   - Auxiliary functions: VPNInd.R, VPBInd.R,
#     ALB_Emp_AggInd_HCR.R
#
# Outputs:
#   - FLoutput/2Steps/Emp/EMPW8/Output_run_<nrun>.RData :
#     projection output including bio, flt, fltStk, mt,
#     mtStk, resInd and AlbHCR_ind objects
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Empirical aggregate index HCR projection run — Scenario EMPW8 (2026-2057)
#
# Objectives:
#
#   1. Load FLinput data and Rinit output for the assigned
#      iteration.
#
#   2. Set the projection period (2026-2057).
#
#   3. Set historical TAC from observed landings.
#
#   4. Build the aggregate index advice control object with
#      its parameters and index uncertainty values.
#
#   5. Set observation control to perfectObs with 3-year
#      index frequency.
#
#   6. Configure fleet control (landing obligation off,
#      discard TAC overshoot off).
#
#   7. Update indices for 2025 using VPB/VPN from Rinit.
#
#   8. Run FLBEIA for 2026-2057 with the aggregate index HCR.
#
#   9. Compute and save summary tables.
#
# Notes:
#
#   - nrun is set by SLURM_ARRAY_TASK_ID.
#
#   - No stock assessment model: obs.ctrl uses perfectObs
#     and assess.ctrl is used unmodified.
#
#   - Key difference from EMPW3: maxRange = minRange = 0.1,
#     restricting interannual TAC variation to ±10%
#     (vs. +25%/-20% in EMPW3).
#
#   - HCR parameters: maxRange = 0.1, minRange = 0.1,
#     maxTAC = 50000, alpha = 1. HCR type = 2, nass = 3.
#
#   - Index uncertainty: SD and AC estimated from OEM
#     analysis (see Tables/InputMSE_OEM).
#
#   - Indices 4 and 5 use biomass-based VPB (catch weight
#     included); all others use numbers-based VPN.
#
#   - SRs is used unmodified from Rinit.
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
# library(spict)  # not used in empirical HCR scenario

source(file.path("code", "Others", "VPNInd.R"))
source(file.path("code", "Others", "VPBInd.R"))
source(file.path("code", "Others", "ALB_Emp_AggInd_HCR.R"))


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

FLinput.data <- "FLinput/R1b"
in.data      <- "FLoutput/Rinit"
out.dir      <- "EMPW8"

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
# SECTION 5 — ADVICE CONTROL (aggregate index HCR)
# =============================================================================

advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]

advice.ctrl.ind <- list()
advice.ctrl.ind$ALB$HCR.model      <- "ALB_AggInd_HCR"
advice.ctrl.ind[["ALB"]][["index"]] <- c("BB", "JPLLS", "TAILLN", "TAILLS", "USLLN", "USLLS")

# HCR parameters matrix
# Note: maxRange = minRange = 0.1 (±10% TAC constraint, tighter than EMPW3)
mat <- matrix(c(NA, NA, NA, NA), ncol = 1)
rownames(mat) <- c("maxRange", "minRange", "maxTAC", "alpha")
advice.ctrl.ind[["ALB"]][["param"]]              <- mat
advice.ctrl.ind[["ALB"]][["param"]]["maxRange", 1] <- 0.1
advice.ctrl.ind[["ALB"]][["param"]]["minRange", 1] <- 0.1
advice.ctrl.ind[["ALB"]][["param"]]["maxTAC",   1] <- 50000
advice.ctrl.ind[["ALB"]][["param"]]["alpha",    1] <- 1

advice.ctrl.ind[["ALB"]][["type"]]  <- 2
advice.ctrl.ind[["ALB"]][["nass"]]  <- 3

# Index uncertainty: SD and AC estimated from OEM analysis
advice.ctrl.ind[["ALB"]][["sd_Ind"]] <- c(0.38, 0.36, 0.29, 0.33, 0.39, 0.37)
advice.ctrl.ind[["ALB"]][["AC_Ind"]] <- c(0.11, 0.39, 0.16, 0.56, 0.66, 0.59)

advice.ctrl.ind$ALB$adv.year <- seq(2026, 2057, 3)


# =============================================================================
# SECTION 6 — OBSERVATION AND FLEET CONTROLS
# =============================================================================

#........................................................
#...OBS.CTRL
#....................................................

obs.ctrl$ALB$stkObs$stkObs.model <- "perfectObs"

for (ind.nm in names(indices$ALB)) {
  obs.ctrl$ALB$indObs[[ind.nm]]$yrs <- 3
}

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
# SECTION 8 — RUN FLBEIA (aggregate index HCR)
# =============================================================================

AlbHCR_ind <- FLBEIA(
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
  obs.ctrl    = obs.ctrl,
  assess.ctrl = assess.ctrl,
  advice.ctrl = advice.ctrl.ind
)


# =============================================================================
# SECTION 9 — SUMMARISE AND SAVE
# =============================================================================

bio    <- bioSum(AlbHCR_ind, byyear = TRUE, ssb_season = 1)
flt    <- fltSum(AlbHCR_ind, long = TRUE)
fltStk <- fltStkSum(AlbHCR_ind, long = TRUE)
mt     <- mtSum(AlbHCR_ind, long = TRUE)
mtStk  <- mtStkSum(AlbHCR_ind, long = TRUE)

bio$iter    <- nrun
flt$iter    <- nrun
fltStk$iter <- nrun
mt$iter     <- nrun
mtStk$iter  <- nrun

resInd <- AlbHCR_ind$indices

save(resInd, bio, flt, fltStk, mt, mtStk, AlbHCR_ind,
     file = file.path("FLoutput", "2Steps", "Emp", out.dir,
                      paste0("Output_run_", nrun, ".RData")))

cat("Run", nrun, "saved successfully.\n")