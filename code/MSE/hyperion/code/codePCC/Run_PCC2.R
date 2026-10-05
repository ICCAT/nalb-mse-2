# ============================================================
# Script: Run_PCC2_Cluster.R
#
# Purpose:
#   Run the FLBEIA projection stage (2026-2057) for scenario
#   PCC2 using a Pseudo Constant Catch (PCC) index-based HCR
#   (ALB_PCCatch_Jindex). No stock assessment model is used;
#   observation is set to perfectObs. Loads the Rinit output
#   as starting point and updates indices for 2025.
#   Designed to run as a SLURM array job.
#
# Inputs:
#   - FLinput/R1b/FLinput_run_<nrun>.RData  : FLBEIA input
#     data for the given iteration
#   - FLoutput/Rinit/Output_run_<nrun>.RData : Rinit output
#     (initialisation stage, 2022-2025)
#   - Auxiliary functions: VPNInd.R, VPBInd.R,
#     ALB_PCCatch_Jindex.R
#
# Outputs:
#   - FLoutput/2Steps/Emp/PCC2/Output_run_<nrun>.RData :
#     projection output including bio, flt, fltStk, mt,
#     mtStk, resInd and AlbHCR_ind objects
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# PCC index-based HCR projection run — Scenario PCC2 (2026-2057)
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
#   4. Build the PCC advice control object with its
#      reference points and index uncertainty parameters.
#
#   5. Set observation control to perfectObs with 3-year
#      index frequency.
#
#   6. Configure fleet control (landing obligation off,
#      discard TAC overshoot off).
#
#   7. Update indices for 2025 using VPB/VPN from Rinit.
#
#   8. Run FLBEIA for 2026-2057 with PCC HCR.
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
#   - PCC reference points: minRange = maxRange = 0.15,
#     maxTAC = 42000, Jref = 1.08 (2010), Cmin = 0,
#     nyear = 1. HCR type = 2, nass = 3.
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
# library(spict)  # not used in PCC scenario

source(file.path("code", "Others", "VPNInd.R"))
source(file.path("code", "Others", "VPBInd.R"))
source(file.path("code", "Others", "ALB_PCCatch_Jindex.R"))


# =============================================================================
# SECTION 2 — USER SETTINGS
# =============================================================================

FLinput.data <- "FLinput/R1b"
in.data      <- "FLoutput/Rinit"
out.dir      <- "PCC2"

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
# SECTION 5 — ADVICE CONTROL (PCC index-based HCR)
# =============================================================================

advice$TAC[1, as.character(1930:2021)] <- tlandStock(fleets, "ALB")[1, as.character(1930:2021)]

advice.ctrl.PCC <- list()
advice.ctrl.PCC$ALB$HCR.model      <- "ALB_PCCatch_Jindex_HCR"
advice.ctrl.PCC[["ALB"]][["index"]] <- c("BB", "JPLLS", "TAILLN", "TAILLS", "USLLN", "USLLS")

# Reference points matrix
mat <- matrix(c(NA, NA, NA, NA, NA, NA), ncol = 1)
rownames(mat) <- c("minRange", "maxRange", "maxTAC", "Jref", "Cmin", "nyear")
advice.ctrl.PCC[["ALB"]][["ref.pts"]]            <- mat
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["minRange", 1] <- 0.15
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["maxRange", 1] <- 0.15
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["maxTAC",  1] <- 42000
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["Jref",    1] <- 1.08  # reference year: 2010
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["Cmin",    1] <- 0
advice.ctrl.PCC[["ALB"]][["ref.pts"]]["nyear",   1] <- 1

advice.ctrl.PCC[["ALB"]][["type"]]  <- 2
advice.ctrl.PCC[["ALB"]][["nass"]]  <- 3

# Index uncertainty: SD and AC estimated from OEM analysis
advice.ctrl.PCC[["ALB"]][["sd_Ind"]] <- c(0.38, 0.36, 0.29, 0.33, 0.39, 0.37)
advice.ctrl.PCC[["ALB"]][["AC_Ind"]] <- c(0.11, 0.39, 0.16, 0.56, 0.66, 0.59)

advice.ctrl.PCC$ALB$adv.year <- seq(2026, 2057, 3)


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
# SECTION 8 — RUN FLBEIA (PCC index-based HCR)
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
  advice.ctrl = advice.ctrl.PCC
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