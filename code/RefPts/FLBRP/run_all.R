# ============================================================
# Script: SS3_MonteCarlo.R
#
# Purpose:
#   Estimate MSY biological reference points (Fmsy, SSBmsy,
#   MSY) for the North Atlantic albacore Operating Model
#   across 400 Monte Carlo runs. For each run, sets the
#   parameters, loads the SS3 input objects, runs a short
#   FLBEIA projection to obtain the required biological
#   quantities, and calculates reference points using FLBRP
#   and a custom R function.
#
# Inputs:
#   - 1_set_params.R       : run-specific parameter settings
#   - 2_load_objects.R     : SS3 input objects for each run
#   - 3_run_short_proj.R   : short-term FLBEIA projection
#   - 4_calculate_refpts.R : MSY reference point estimation
#
# Outputs:
#   - estimates/ : one CSV file per run with Fmsy, SSBmsy
#                  and MSY estimates
#   - Fpattern/  : fishing pattern outputs per run
#   - check_long/: diagnostic outputs per run
#
#
# Author: AZTI
# ============================================================
# Load required libraries
library(mvtnorm)
library(triangle)
library(XLConnect)
library(FLXSA)
library(FLAssess)
library(FLash)
library(nloptr)
library(FLCore)     
library(FLFleet)
library(FLBEIA)
library(devtools)
library(r4ss)
library(ss3om)

# Create folder where estimates will be saved:
# dir.create("estimates")
# dir.create("Fpattern")
# dir.create("check_long")

# Run number (1 to 400 for ALB):
all_oms = 1:400

# Run in loop:
for(irun in all_oms) {
  
  # Run scripts:
  source('1_set_params.R')
  invisible(capture.output(suppressMessages(suppressWarnings( source('2_load_objects.R') ))))
  invisible(capture.output(suppressMessages(suppressWarnings( source('3_run_short_proj.R') ))))
  invisible(capture.output(suppressMessages(suppressWarnings( source('4_calculate_refpts.R') ))))
  
  # Print:
  print(irun)
  
  # Clear WS:
  rm(list = setdiff(ls(), "irun"))
  
}
