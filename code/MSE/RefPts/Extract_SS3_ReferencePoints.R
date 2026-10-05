# ============================================================
# Script: Extract_SS3_ReferencePoints.R
#
# Purpose:
#   Extract biological reference points from the selected
#   SS3 realizations used to condition the Atlantic albacore
#   Operating Model.
#
# Inputs:
#   - SelectedRuns4OM.csv
#   - SS3 Report_<run>.sso files
#   - SS3 CompReport_<run>.sso files
#
# Outputs:
#   - RefPts<Scenario>_ss3.csv
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SS3 Reference Point Extraction
#
# Objectives:
#
#   1. Read the selected SS3 realizations used for Operating
#      Model conditioning.
#
#   2. Extract biological reference points from each selected
#      run:
#
#        - SSBMSY
#        - FMSY
#        - MSY
#        - SigmaR
#
#   3. Characterize uncertainty in reference points across
#      the selected realizations.
#
#   4. Store the reference points in a format suitable for
#      subsequent FLBEIA and MSE analyses.
#
# Notes:
#
#   - The script currently extracts reference points from the
#     selected runs of a single uncertainty scenario.
#
#   - Each row of the output represents one SS3 realization.
#
#   - The resulting file is used to compare reference point
#     variability among SS3 runs and to support Operating
#     Model conditioning.
#
# ---------------------------------------------------------------------------


rm(list=ls())

library(r4ss)
library(ss3om)
library(here)

proj_dir = here::here()
setwd(proj_dir)


# Section 2-3: Directory and Load the data------------------- ####

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

irun<-1  #test

df <- NULL
SSB_MSY <- NULL
F_MSY <- NULL
MSY <- NULL
SIGMA <- NULL

for(irun in  1:100){

  nm <- c("BaseCase","AGE","CPUE","SIZE")
  sc <- paste0("OM/",nm)

  #CAREFUL NOW ONLY BASE CASE
  sc.i <- 1
  sel_runs <- read.csv(paste0(sc[sc.i],"/Results/SelectedRuns4OM.csv"))
  nrun <- sel_runs[irun,1]
  replist <- SS_output(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"),verbose=F,printstats=F)
  ss3.stk <- replist
  
  SSB_MSY_sc <- ss3.stk$derived_quants$Value[ss3.stk$derived_quants$Label=="SSB_MSY"]
  SSB_MSY <- c(SSB_MSY,SSB_MSY_sc)   
  F_MSY_sc<- ss3.stk$derived_quants$Value[ss3.stk$derived_quants$Label=="annF_MSY"]   
  F_MSY <- c(F_MSY,F_MSY_sc ) 
  MSY_sc<- ss3.stk$derived_quants$Value[ss3.stk$derived_quants$Label=="Dead_Catch_MSY"]   
  MSY <- c(MSY,MSY_sc) 
  sigma_sc <- (replist$parameters$Value[replist$parameters$Label=="SR_sigmaR"])
  SIGMA <- c(SIGMA,sigma_sc)
}

df$SSB_MSY <- SSB_MSY
df$F_MSY <- F_MSY
df$MSY <- MSY
df$sigma <- SIGMA
df <- as.data.frame(df)
df$scenario <- sc[sc.i]
df$iter <- 1:100

write.csv(df,file.path("RefPts",paste0("RefPts",sc[sc.i],"_ss3.csv")))
