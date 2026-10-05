
# ============================================================
# Script: SS3_MonteCarlo.R
#
# Purpose:
#   Generate Monte Carlo realizations of the SS3 assessment
#   by sampling key biological parameters and running
#   independent SS3 model fits. (Run in the cluster)
#
# Inputs:
#   - ss.par
#   - data.ss
#   - control.ss
#   - starter.ss
#   - forecast.ss
#   - SS3 executable
#
# Outputs:
#   - ss<run>.par
#   - Report_<run>.sso
#   - CompReport_<run>.sso
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SS3 Monte Carlo Conditioning
#
# Objectives:
#
#   1. Generate Monte Carlo realizations of:
#
#        - Natural mortality (M)
#        - Recruitment variability (SigmaR)
#
#   2. Create an independent SS3 configuration for
#      each Monte Carlo iteration.
#
#   3. Run SS3 using modified parameter values.
#
#   4. Produce a collection of assessment outputs
#      representing parameter uncertainty.
#
# Method:
#
#   - M is sampled from a normal distribution with
#     coefficient of variation equal to natM_CV.
#
#   - SigmaR is sampled from a normal distribution with
#     coefficient of variation equal to sigR_CV.
#
#   - Each iteration is executed independently and
#     can be distributed across SLURM array jobs.
#
# Notes:
#
#   - One SS3 directory is created per Monte Carlo run.
#
#   - The resulting Report.sso and CompReport.sso files
#     are copied to the parent directory for subsequent
#     summarization.
#
#   - The script is designed for cluster execution using
#     SLURM job arrays.
#
# ---------------------------------------------------------------------------

rm(list=ls())

usnam   <- R.utils::getUsername.System()
lib.dir <- file.path("/home",usnam,"rlibs")

# load libraries

.libPaths( c( .libPaths(), lib.dir) ) # Add new library path
.libPaths(.libPaths()[2])             # Select the default

library(r4ss, lib.loc = lib.dir)

mywd <- file.path(getwd(), Sys.getenv("SLURM_JOB_NAME")) #'bc'
tmpdir  <- Sys.getenv("TMP_DIR") # /tmp/jobs/<usnam>/<jobtaskid>
maindir <- getwd()

print(maindir)

prefix <- "./"
exefile_to_run <- "ss"
extras  <- "-nox -nohess"

iter <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Define MC Terms
ntrials=100	    # Number of Monte Carlo Draws/SS Model Iterations
sigR_CV=0.20		# Coefficient of Variation for Normal Distribution MC of SigmaR
natM_CV=0.20	  # Coefficient of Variation for Normal Distribution MC of Natural Mortality
start_yr=1930	  # SS Model Start Year
end_yr=2051	    # SS Model End Year Including Projection Years

# Create the Function to Run SS Monte Carlo of Fixed Parameters and Save Iterations Parameters and Reports; Here Set-up for SigmaR and NatM MC

  sigCV <- sigR_CV
  natCV <- natM_CV
  setwd(mywd)
  dir.create(file.path(mywd,iter))
  file.copy(file.path(mywd,c('control.ss','data.ss','starter.ss','forecast.ss','ss')),to=file.path(mywd,iter))
  
  SSpar=SS_readpar_3.30(paste0(mywd,'/ss.par'),paste0(mywd,'/data.ss'),paste0(mywd,'/control.ss'), verbose = TRUE)
  # sigmaR Monte Carlo
  SSpar$SR_parms[3,]
  set.seed(2)
  vSigma <- rnorm(100,SSpar$SR_parms[3,1],SSpar$SR_parms[3,1]*sigCV)
  SSpar$SR_parms[3,2]=vSigma[iter]
  SSpar$SR_parms[3,]
  # natM Monte Carlo
  SSpar$MG_parms[1,]
  set.seed(1)
  vM <- rnorm(100,SSpar$MG_parms[1,1],SSpar$MG_parms[1,1]*natCV)
  SSpar$MG_parms[1,2]=vM[iter]
  SSpar$MG_parms[1,]	
  SS_writepar_3.30(SSpar, paste0(iter,'/ss.par'), overwrite = TRUE, verbose = FALSE)
  
  file.copy(from=paste0(mywd,'/',iter,'/ss.par'),to=paste0(mywd,'/ss',iter,'.par'), overwrite=TRUE)
  
  setwd(paste0(mywd,'/',iter))
  system("chmod +x ss")
 # system('ss -nohess')
  command <- paste0(prefix, exefile_to_run, " ", extras)
  
  message("Running model in ", getwd(), "\n", "using the command:\n   ", 
          command, sep = "")
  ADMBoutput <- system(command, intern = FALSE)
  
  file.copy(from=paste0(mywd,'/',iter,'/Report.sso'),to=paste0(mywd,'/Report_',iter,'.sso'),overwrite=TRUE)
  file.copy(from=paste0(mywd,'/',iter,'/CompReport.sso'),to=paste0(mywd,'/CompReport_',iter,'.sso'),overwrite=TRUE)

