# ============================================================
# Script: Aggregate_Indices.R
#
# Purpose:
#   Aggregate FLBEIA index outputs across all simulation runs
#   and create FLQuant objects containing the full index
#   uncertainty distribution.
#
# Inputs:
#   - Output_run_<run>.RData
#
# Outputs:
#   - Output_res.RData
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Aggregate Index Outputs
#
# Objectives:
#
#   1. Read the index outputs from all FLBEIA simulation runs.
#
#   2. Combine biological outputs across iterations.
#
#   3. Create FLQuant objects containing the complete
#      distribution of:
#
#        - BB
#        - JPLLN
#        - JPLLS
#        - TAILLN
#        - TAILLS
#        - USLLN
#        - USLLS
#        - VENLL
#
#   4. Store all simulated trajectories as separate
#      iterations within a single FLQuant object.
#
#   5. Generate the aggregated index objects used by
#      subsequent plotting and uncertainty analyses.
#
# Notes:
#
#   - Each simulation run is stored as one iteration
#     of the aggregated FLQuant object.
#
#   - Historical biological outputs are merged into a
#     single data frame (outdf).
#
#   - The resulting Output_res.RData file is used by
#     the plotting scripts to calculate quantiles and
#     visualize uncertainty envelopes.
#
# ---------------------------------------------------------------------------



rm(list=ls())

library(FLBEIA)
library(ss3om)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Section 2-3: Directory and Load the data------------------- ####

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

#.......LOAD DATA .......................

in.data <- "FLInput/R1b"
out.dir <- "FLoutput/Others/R3b"


nrun<-1  #test

#........................................................
#....Aggregate runs
#....................................................



file_nm <- list.files(out.dir)
runs = unique(sort(as.numeric(gsub(".*?([0-9]+).*", "\\1", file_nm))))           

nrun <- 1
load(paste0(out.dir,"/Output_run_",nrun,".RData"))

indices <- AlbHCR_spict$indices



aux <- bio
BB.ind.all<- propagate(indices$ALB$BB@index,400,fill.iter=FALSE)
iter(BB.ind.all,1) <- resInd$ALB$BB@index
JPLLN.ind.all<- propagate(indices$ALB$JPLLN@index,400,fill.iter=FALSE)
iter(JPLLN.ind.all,1) <- resInd$ALB$JPLLN@index
JPLLS.ind.all<- propagate(indices$ALB$JPLLS@index,400,fill.iter=FALSE)
iter(JPLLS.ind.all,1) <- resInd$ALB$JPLLS@index
TAILLN.ind.all<- propagate(indices$ALB$TAILLN@index,400,fill.iter=FALSE)
iter(TAILLN.ind.all,1) <- resInd$ALB$TAILLN@index
TAILLS.ind.all<- propagate(indices$ALB$TAILLS@index,400,fill.iter=FALSE)
iter(TAILLS.ind.all,1) <- resInd$ALB$TAILLS@index
USLLN.ind.all<- propagate(indices$ALB$USLLN@index,400,fill.iter=FALSE)
iter(USLLN.ind.all,1) <- resInd$ALB$USLLN@index
USLLS.ind.all<- propagate(indices$ALB$USLLS@index,400,fill.iter=FALSE)
iter(USLLS.ind.all,1) <- resInd$ALB$USLLS@index
VENLL.ind.all<- propagate(indices$ALB$VENLL@index,400,fill.iter=FALSE)
iter(VENLL.ind.all,1) <- resInd$ALB$VENLL@index
for(nrun in runs[-1]){
  load(paste0(out.dir,"/Output_run_",nrun,".RData"))
  aux <- rbind(bio,aux)
  iter(BB.ind.all,nrun) <- resInd$ALB$BB@index
  iter(JPLLN.ind.all,nrun) <- resInd$ALB$JPLLN@index
  iter(JPLLS.ind.all,nrun) <- resInd$ALB$JPLLS@index
  iter(TAILLN.ind.all,nrun) <- resInd$ALB$TAILLN@index
  iter(TAILLS.ind.all,nrun) <- resInd$ALB$TAILLS@index
  iter(USLLN.ind.all,nrun) <- resInd$ALB$USLLN@index
  iter(USLLS.ind.all,nrun) <- resInd$ALB$USLLS@index
  iter(VENLL.ind.all,nrun) <- resInd$ALB$VENLL@index
}

outdf <- aux
save(outdf, BB.ind.all,JPLLN.ind.all,
     JPLLS.ind.all,TAILLN.ind.all,TAILLS.ind.all,
     USLLN.ind.all,USLLS.ind.all,VENLL.ind.all,
     file=paste0(out.dir,"/Output_res.RData"))

