# ============================================================
# Script: shiny_FinalScenario.R
#
# Purpose:
#   Combine aggregated FLBEIA outputs from model-based,
#   empirical and pseudo-constant catch management procedures
#   and generate a unified input file for FLBEIAShiny.
#
# Inputs:
#   - Aggregated FLBEIA outputs:
#       * Model-based MP (25% constraints)
#       * Model-based MP (10% constraints)
#       * Pseudo-Constant Catch MP
#       * Empirical MP
#
# Outputs:
#   - Shiny_input.RData
#   - Interactive FLBEIAShiny application
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# FLBEIAShiny Input Builder
#
# Objectives:
#
#   1. Read aggregated outputs from multiple management
#      procedures.
#
#   2. Merge performance metrics across:
#
#        - Biological indicators
#        - Fleet indicators
#        - Metier indicators
#        - Advice metrics
#
#   3. Standardize outputs from:
#
#        - Model-based MPs
#        - Empirical MPs
#        - Pseudo-Constant Catch MPs
#        - Robustness scenarios
#
#   4. Create a single dataset compatible with
#      FLBEIAShiny.
#
#   5. Launch the FLBEIAShiny application for
#      scenario comparison and visualization.
#
# Notes:
#
#   - Both 25% and 10% variability model-based MPs
#     are included.
#
#   - EMP and PCC scenarios are merged together with
#     model-based scenarios.
#
#   - The output file is intended only for interactive
#     visualization and comparison of MSE results.
#
# ---------------------------------------------------------------------------


library(FLBEIA)
library(FLBEIAshiny)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source(file.path('code','Others','AuxiliaryFunctions.R'))
source('sharepoint_path.R')
setwd(shrpoint_path)


#file_nm <- c(paste0(sc_run, "_AggregatedOutput_ALB.RData"))

dir_in_MB <-  file.path("FLoutput","Summary","ModelBased_25var")
dir_in_MB10 <-  file.path("FLoutput","Summary","ModelBased_10var")
dir_in_Emp <-  file.path("FLoutput","Summary","EMP")
dir_in_PCC <-  file.path("FLoutput","Summary","PCC")


sc_MB <-  sort(apply(expand.grid("2S",3, c(1,3)), 1, paste, collapse=""))
sc_RT <-  c(apply(expand.grid("2S3",c(1),c("_R0dw","_sigma","_R0up")), 1, paste, collapse=""),
            apply(expand.grid("2S3",c(3),c("_R0dw","_sigma","_R0up")), 1, paste, collapse=""))
sc_PCC <-  c("PCC2","PCC2_sigma","PCC2_R0up","PCC2_R0dw")
sc_Emp <- c("EMPW3","EMPW8",c(paste0("EMPW8",c("_R0dw","_sigma","_R0up"))),c(paste0("EMPW3",c("_R0dw","_sigma","_R0up"))))
sc_run <- c(sc_MB,
            sc_RT,
            sc_MB, #modified after
            sc_RT,
            sc_PCC,
            sc_Emp)
#sc_run <- c(sc_RT)

file_nm <- c(paste0(sc_run, "_Aggregated_bio_RefPts_Q.RData"))


bio <- flt <-fltStk <- mt <- mtStk <- adv <- NULL

sc_vec <- c(1:length(sc_run))

index_MB <- c(1:length(c(sc_MB,sc_RT)))
index_MB10var <- max(index_MB)+c(1:length(c(sc_MB,sc_RT)))
index_PC <- max(index_MB10var)+c(1:length(sc_PCC))
index_EMP<- max(index_PC)+c(1:length(sc_Emp))

for(i in 1:length(sc_run)){ 
  
  if(i %in% index_MB){
    file.path <- paste0(dir_in_MB,"/", paste0(sc_run[i], "_Aggregated_bio_FLBRP_RefPts_Q.RData"))
    load(file.path)}
  if(i %in% index_MB10var){
    file.path <- paste0(dir_in_MB10,"/", paste0(sc_run[i], "_Aggregated_bio_FLBRP_RefPts_Q_10var.RData"))
    load(file.path)}
  if(i %in% index_PC){
    file.path <- paste0(dir_in_PCC,"/", file_nm[i])
    load(file.path)}
  if(i %in% index_EMP){
    file.path <- paste0(dir_in_Emp,"/", file_nm[i])
    load(paste0(dir_in_Emp,"/", file_nm[i]))}
  
  bio <- rbind(bio,bioQ)
  flt <- rbind(flt,fltQ)
  fltStk <- rbind(fltStk,fltStkQ)
  mt <- rbind(mt,mtQ)
  mtStk <- rbind(mtStk,mtStkQ)
  adv <- rbind(adv,advQ)
}

bio <- as.data.frame(bio)
flt <- as.data.frame(flt)
fltStk <- as.data.frame(fltStk)
mt <- as.data.frame(mt)
mtStk <- as.data.frame(mtStk)
adv <- as.data.frame(adv)

dir_out <- file.path("FLoutput","ShinyInput")
save(bio,flt,fltStk,mt,mtStk,adv,file=file.path(dir_out,"Shiny_input.RData"))

flbeiaApp(bio = bio,
          flt = flt,
          fltStk = fltStk,
          mt = mt,
          mtStk = mtStk,
          adv=adv)
