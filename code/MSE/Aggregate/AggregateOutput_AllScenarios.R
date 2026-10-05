# ============================================================
# Script: AggregateOutput_AllScenarios.R
#
# Purpose:
#   Aggregate FLBEIA outputs across all scenario groups
#   (ModelBased 25% TAC var, ModelBased 10% TAC var, PCC and EMPW),
#   compute model-based reference points, generate quantile
#   summaries, and combine all scenarios for Shiny visualisation.
#
# Inputs:
#   - <sc_run>_AggregatedOutput_ALB.RData : FLBEIA aggregated
#     output per run, one file per scenario group directory
#   - RefPts_FLBRP_format.csv             : operating model
#     reference points (dir: Output/Tables/)
#   - AuxiliaryFunctions.R                : auxiliary functions
#   - sharepoint_path.R                   : Sharepoint path config
#
# Outputs:
#   - <sc_run>_Aggregated_bio_RefPts.RData         : biological
#     output with reference points applied, per run
#   - <sc_run>_Aggregated_bio_FLBRP_RefPts_Q.RData : quantile
#     summaries for ModelBased 25var runs
#   - <sc_run>_Aggregated_bio_FLBRP_RefPts_Q_10var.RData : quantile
#     summaries for ModelBased 10var runs
#   - <sc_run>_Aggregated_bio_RefPts_Q.RData        : quantile
#     summaries for PCC runs
#   - Shiny_input.RData : all scenarios combined for Shiny
#     (dir: FLoutput/ShinyInput/)
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Aggregation of scenario outputs and reference point computation
#
# Objectives:
#
#   1. Define all scenario groups (ModelBased 25var, ModelBased
#      10var, PCC) in a single configuration list.
#
#   2. For each group and each run, load the aggregated FLBEIA
#      output and assign scenario labels.
#
#   3. Compute relative reference points (SSB/SSBmsy, F/Fmsy)
#      using transfBioSRmod() with the appropriate alpha:
#
#        - alpha = 1.0  (default runs)
#        - alpha = 0.8  (R0 down scenarios)
#        - alpha = 1.2  (R0 up  scenarios)
#
#   4. Save the biological output with reference points applied.
#
#   5. Compute and save quantile summaries per run:
#
#        - Biological component   (bioQ)
#        - Fleet component        (fltQ, fltStkQ)
#        - Metier component       (mtQ,  mtStkQ)
#        - Advice component       (advQ)
#
#   6. Combine all scenario groups into a single object and
#      save it for Shiny visualisation.
#
#   7. Launch the FLBEIAshiny interactive application.
#
# Notes:
#
#   - All scenario-specific settings (directory, run IDs,
#     scenario names, output suffix, alpha runs) are defined
#     in the `scenario_groups` list. To add a new group,
#     add a new entry to that list.
#
#   - The alpha parameter scales Bmsy to account for
#     uncertainty in R0 across sensitivity scenarios.
#
#   - Quantile summaries use the default 90% CI from
#     bioSumQ(), fltSumQ(), mtSumQ() and advSumQ().
#
#   - The combined Shiny input is saved to
#     FLoutput/ShinyInput/Shiny_input.RData.
#
# ---------------------------------------------------------------------------


library(FLBEIA)
library(FLBEIAshiny)
library(here)

proj_dir <- here::here()
setwd(proj_dir)

# Sharepoint path
source(file.path('code', 'Others', 'AuxiliaryFunctions.R'))
source('sharepoint_path.R')
setwd(shrpoint_path)

# Reference points OM (shared across all groups)
ref.pts <- read.csv(file.path("RefPts", "RefPts_FLBRP_format.csv"))


# =============================================================================
# Scenario group configuration
#
# Each entry in this list defines one scenario group with:
#   dir        : directory containing the input .RData files
#   sc_run     : vector of run identifiers
#   sc_nm_all  : human-readable scenario names (same order as sc_run)
#   out_suffix : suffix for the quantile summary output file
#   r0dw_runs  : run IDs that use alpha = 0.8 (R0 down)
#   r0up_runs  : run IDs that use alpha = 1.2 (R0 up)
# =============================================================================

scenario_groups <- list(
  
  ModelBased_25var = list(
    dir       = file.path("FLoutput", "Summary", "ModelBased_25var"),
    sc_run    = c("2S31", "2S33",
                  "2S31_R0dw", "2S31_R0up", "2S31_sigma",
                  "2S33_R0dw", "2S33_R0up", "2S33_sigma"),
    sc_nm_all = c("Ftg0.8_25%",       "Ftg1_25%",
                  "Ftg0.8_25%_R-",    "Ftg0.8_25%_R+",   "Ftg0.8_25%_sigmaR+",
                  "Ftg1_25%_R-",      "Ftg1_25%_R+",     "Ftg1_25%_sigmaR+"),
    out_suffix = "_Aggregated_bio_RefPts_Q.RData",
    r0dw_runs  = c("2S31_R0dw", "2S33_R0dw"),
    r0up_runs  = c("2S31_R0up", "2S33_R0up")
  ),
  
  ModelBased_10var = list(
    dir       = file.path("FLoutput", "Summary", "ModelBased_10var"),
    sc_run    = c("2S31", "2S33",
                  "2S31_R0dw", "2S31_R0up", "2S31_sigma",
                  "2S33_R0dw", "2S33_R0up", "2S33_sigma"),
    sc_nm_all = c("Ftg0.8_10%",       "Ftg1_10%",
                  "Ftg0.8_10%_R-",    "Ftg0.8_10%_R+",   "Ftg0.8_10%_sigmaR+",
                  "Ftg1_10%_R-",      "Ftg1_10%_R+",     "Ftg1_10%_sigmaR+"),
    out_suffix = "_Aggregated_bio_RefPts_Q_10var.RData",
    r0dw_runs  = c("2S31_R0dw", "2S33_R0dw"),
    r0up_runs  = c("2S31_R0up", "2S33_R0up")
  ),
  
  PCC = list(
    dir       = file.path("FLoutput", "Summary", "PCC"),
    sc_run    = c("PCC2", "PCC2_R0dw", "PCC2_sigma", "PCC2_R0up"),
    sc_nm_all = c("PCC",  "PCC_R0-",   "PCC_sigmaR+","PCC_R-"),
    out_suffix = "_Aggregated_bio_RefPts_Q.RData",
    r0dw_runs  = c("PCC2_R0dw"),
    r0up_runs  = c("PCC2_R0up")
  ),
  EMPW = list(
    dir       = file.path("FLoutput", "Summary", "EMP"),
    sc_run    = c("EMPW3", "EMPW8",
                  "EMPW3_R0dw", "EMPW3_sigma", "EMPW3_R0up",
                  "EMPW8_R0dw", "EMPW8_sigma", "EMPW8_R0up" ),
    sc_nm_all = c("EMPW3", "EMPW8",
                  "EMPW3_R0-",   "EMPW3_sigmaR+","EMPW3_R-",
                  "EMPW8_R0-",   "EMPW8_sigmaR+","EMPW8_R-"),
    out_suffix = "_Aggregated_bio_RefPts_Q.RData",
    r0dw_runs  = c("EMPW3_R0dw","EMPW8_R0dw"),
    r0up_runs  = c("EMPW3_R0up","EMPW8_R0up")
  )
)


# =============================================================================
# Main loop: process each scenario group
# =============================================================================

for (grp_nm in names(scenario_groups)) {
  
  grp       <- scenario_groups[[grp_nm]]
  dir_in    <- grp$dir
  dir_out   <- grp$dir
  sc_run    <- grp$sc_run
  sc_nm_all <- grp$sc_nm_all
  
  message("\n--- Processing group: ", grp_nm, " ---")
  
  file_nm <- paste0(sc_run, "_AggregatedOutput_ALB.RData")
  
  for (j in seq_along(sc_run)) {
    
    load(file.path(dir_in, file_nm[j]))
    sc_nm <- sc_nm_all[j]
    
    # Assign scenario label to all components
    bio_sc$scenario    <- sc_nm
    flt_sc$scenario    <- sc_nm
    fltStk_sc$scenario <- sc_nm
    mt_sc$scenario     <- sc_nm
    mtStk_sc$scenario  <- sc_nm
    adv_sc$scenario    <- sc_nm
    
    iter <- unique(bio_sc$iter)
    
    # Set alpha to scale Bmsy for R0 sensitivity runs
    if (sc_run[j] %in% grp$r0dw_runs) {
      alpha <- 0.8        # R0 down: Bmsy decreases by 20%
    } else if (sc_run[j] %in% grp$r0up_runs) {
      alpha <- 1.2        # R0 up:   Bmsy increases by 20%
    } else {
      alpha <- 1.0        # Default
    }
    
    # Compute relative reference points (SSB/SSBmsy, F/Fmsy)
    bio_refpts <- transfBioSRmod(bio_sc, iter, ref.pts, alpha)
    
    # Save biological output with reference points applied
    save(bio_refpts,
         file = file.path(dir_out,
                          paste0(sc_run[j], "_Aggregated_bio_RefPts.RData")))
    
    # Compute quantile summaries (default 90% CI)
    bioQ    <- bioSumQ(bio_refpts)
    fltQ    <- fltSumQ(flt_sc)
    fltStkQ <- fltStkSumQ(fltStk_sc)
    mtQ     <- mtSumQ(mt_sc)
    mtStkQ  <- mtStkSumQ(mtStk_sc)
    advQ    <- advSumQ(adv_sc)
    
    # Save quantile summaries
    save(bioQ, fltQ, fltStkQ, mtQ, mtStkQ, advQ,
         file = file.path(dir_out, paste0(sc_run[j], grp$out_suffix)))
    
    message("  Saved: ", sc_run[j], grp$out_suffix)
  }
}


# =============================================================================
# Combine all scenarios for Shiny visualisation
# =============================================================================

bio    <- NULL
flt    <- NULL
fltStk <- NULL
mt     <- NULL
mtStk  <- NULL
adv    <- NULL

for (grp_nm in names(scenario_groups)) {
  
  grp    <- scenario_groups[[grp_nm]]
  sc_run <- grp$sc_run
  
  for (i in seq_along(sc_run)) {
    
    load(file.path(grp$dir, paste0(sc_run[i], grp$out_suffix)))
    
    bio    <- rbind(bio,    bioQ)
    flt    <- rbind(flt,    fltQ)
    fltStk <- rbind(fltStk, fltStkQ)
    mt     <- rbind(mt,     mtQ)
    mtStk  <- rbind(mtStk,  mtStkQ)
    adv    <- rbind(adv,    advQ)
  }
}

bio    <- as.data.frame(bio)
flt    <- as.data.frame(flt)
fltStk <- as.data.frame(fltStk)
mt     <- as.data.frame(mt)
mtStk  <- as.data.frame(mtStk)
adv    <- as.data.frame(adv)

# Save combined Shiny input
dir_shiny <- file.path("FLoutput", "ShinyInput")
save(bio, flt, fltStk, mt, mtStk, adv,
     file = file.path(dir_shiny, "Shiny_input.RData"))

# Launch Shiny app
flbeiaApp(bio    = bio,
          flt    = flt,
          fltStk = fltStk,
          mt     = mt,
          mtStk  = mtStk,
          adv    = adv)