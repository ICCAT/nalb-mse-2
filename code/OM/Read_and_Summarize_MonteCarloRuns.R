# ============================================================
# Script: Read_and_Summarize_MonteCarloRuns.R
#
# Purpose:
#   Aggregate and summarize the outputs of multiple SS3
#   Monte Carlo runs.
#
# Inputs:
#   - Report_<run>.sso
#   - CompReport_<run>.sso
#
# Outputs:
#   - Output.RData
#   - Output_summary.RData
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SS3 Monte Carlo Summary
#
# Objectives:
#
#   1. Read all SS3 Monte Carlo runs produced during the
#      conditioning process.
#
#   2. Extract output information from:
#
#        - Stock status trajectories
#        - Recruitment
#        - Biomass
#        - Fishing mortality
#        - Derived quantities
#
#   3. Combine all Monte Carlo realizations into a single
#      object.
#
#   4. Generate a summarized representation of the Monte
#      Carlo ensemble.
#
# Notes:
#
#   - This script is intended to run on high-memory
#     systems because all SS3 outputs are loaded at once.
#
#   - The resulting Summary object is used in subsequent
#     convergence diagnostics, uncertainty analyses and
#     Operating Model conditioning.
#
#   - Forecast outputs are included.
#
# ---------------------------------------------------------------------------


#NOT ENOUGH MEMORY TO RUN IN A PC

#WD <- ALB/AFTERs

usnam   <- R.utils::getUsername.System()
lib.dir <- file.path("/home",usnam,"rlibs")

# load libraries

.libPaths( c( .libPaths(), lib.dir) ) # Add new library path
.libPaths(.libPaths()[4])  

library(r4ss)


mywd="CPUE"
setwd(mywd)
ntrials=400	# Number of Monte Carlo Draws/SS Model Iterations

SumReport=SSgetoutput(keyvec=paste0("_",1:ntrials),getcovar=FALSE,getcomp=FALSE,forecast=TRUE)
save(SumReport,  file="Results/Output.RData")
Summary=SSsummarize(SumReport,SpawnOutputUnits="biomass")
save(Summary,SumReport, file="Results/Output_summary.RData")


