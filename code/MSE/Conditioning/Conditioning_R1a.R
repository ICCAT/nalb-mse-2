# ============================================================
# Script: Conditioning_R1a.R
#
# Purpose:
#   Condition the North Atlantic albacore Operating Model and create
#   the FLBEIA input objects for the selected SS3 runs and OM
#   uncertainty scenarios. Without OEM.
#
# Inputs:
#   - SelectedRuns4OM.csv
#   - SS3 Report_<run>.sso files
#   - SS3 CompReport_<run>.sso files
#   - Projected OEM residuals for the CPUE indices
#
# Outputs:
#   - FLinput_run_<run>.RData
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Northern Albacore Operating Model Conditioning
#
# Objectives:
#
#   1. Read the selected SS3 runs for the four Operating Model
#      uncertainty scenarios:
#
#        - BaseCase
#        - AGE
#        - CPUE
#        - SIZE
#
#   2. Convert the SS3 outputs into the FLR and FLBEIA objects
#      required for the MSE:
#
#        - Biols
#        - Fleets
#        - Stock-recruitment relationships
#        - Advice
#        - Indices
#        - Control objects
#
#   3. Prepare the historical population and fishery data for
#      each selected SS3 realization.
#
#   4. Extend biological, fishery and index parameters over
#      the projection period.
#
#   5. Add projected observation error to the CPUE indices.
#
#   6. Configure vulnerable abundance and vulnerable biomass
#      observation models for the different indices.
#
#   7. Save one complete FLBEIA input file for each selected
#      SS3 run and Operating Model scenario.
#
# Notes:
#
#   - One hundred selected SS3 runs are conditioned for each
#     of the four Operating Model scenarios.
#
#   - The projection period starts in 2022 and ends in 2057.
#
#   - The resulting files are used as inputs for subsequent
#     FLBEIA MSE simulations.
#
# ---------------------------------------------------------------------------


rm(list=ls())


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
library(here)

proj_dir = here::here()
setwd(proj_dir)


# Section 2-3: Directory and Load the data------------------- ####

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

irun<-1  #test

for(sc.i in 1:4){ #choose the scenario
  
    for(irun in  1:100){
      plot.dir <- "Output/Figures/Conditioning/"
      out.data <- "FLinput/R1a/"
      in.OEM.data <- "OEM/InputMSE_OEM/ProjRes/"
      
      nm <- c("BaseCase","AGE","CPUE","SIZE")
      sc <- paste0("OM/",nm)
    
      sel_runs <- read.csv(paste0(sc[sc.i],"/Results/SelectedRuns4OM.csv"))
      nrun <- sel_runs[irun,1]
      #for(sc.i in 1:4){  #Different uncertainty grid
        #for SR
      replist <- SS_output(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"),verbose=F,printstats=F)
      #SS_plots(replist,uncertainty=T,png=T,forecastplot=TRUE, fitrange = TRUE)
      
      ss3 <- readOutputss3(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"))
      
      stock <- readFLSss3(dir=sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"),name="ALB")
      
      bs <- buildFLBFss330(ss3)
    
      # Section 4: Simulation parameters related with time--------- ####
      
      first.yr          <- 1930
      proj.yr           <- 2021+1 
      last.yr           <- 2057  
      hist.yrs  <- as.character(first.yr:(proj.yr-1))
      yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr)
      
      
      # Section 5: Names, age, dimensions-----------------------####
      #ss3$fleetNames
      fls <-   c("BB","BBisl","TRGN","MWT",       
                 "JPLLN",     "JPLLS",     "TAILLN",    "TAILLS",   
                 "USLLN",     "USLLS",    "VENLL",     "MIXKRPA",
                 "OthLL",     "OthSurf",   "BBisls2") 
      
      stks <- c('ALB')
      
      
      BB.mets <- c('BB') 
      BB.BB.stks <- c('ALB') 
      
      BBisl.mets <- c('BBisl') 
      BBisl.BBisl.stks <- c('ALB')
      
      TRGN.mets <- c('TRGN') 
      TRGN.TRGN.stks <- c('ALB')
      
      MWT.mets <- c('MWT') 
      MWT.MWT.stks <- c('ALB')
      
      JPLLN.mets <- c('JPLLN') 
      JPLLN.JPLLN.stks <- c('ALB')
      
      JPLLS.mets <- c('JPLLS') 
      JPLLS.JPLLS.stks <- c('ALB')
      
      TAILLN.mets <- c('TAILLN') 
      TAILLN.TAILLN.stks <- c('ALB')
      
      TAILLS.mets <- c('TAILLS') 
      TAILLS.TAILLS.stks <- c('ALB')
      
      USLLN.mets <- c('USLLN') 
      USLLN.USLLN.stks <- c('ALB')
      
      USLLS.mets <- c('USLLS') 
      USLLS.USLLS.stks <- c('ALB')
      
      VENLL.mets <- c('VENLL') 
      VENLL.VENLL.stks <- c('ALB')
      
      MIXKRPA.mets <- c('MIXKRPA') 
      MIXKRPA.MIXKRPA.stks <- c('ALB')
      
      OthLL.mets <- c('OthLL') 
      OthLL.OthLL.stks <- c('ALB')
      
      OthSurf.mets <- c('OthSurf') 
      OthSurf.OthSurf.stks <- c('ALB')
      
      BBisls2.mets <- c('BBisls2') 
      BBisls2.BBisls2.stks <- c('ALB')
      
      
      # all stocks the same
      ni             <- 1
      ns             <- 1
      
      # stock stk1
      ALB.age.min    <- as.vector(stock@range["min"])
      ALB.age.max    <- as.vector(stock@range["max"]) #in this case the same as plusgroup
      ALB.unit       <- 1 #4 spawning seasons     
      
      
      
      # Section 6: Biols-------------------------------####
      #
      #  Historical data
      #  stk1_n.flq, m, spwn, fec, wt
      #
      
      #stock stk1
      #stock nmeg
      ALB.age.min    <- as.vector(stock@range["min"])
      ALB.age.max    <- as.vector(stock@range["max"]) #in this case the same as plusgroup
      ALB.unit       <- 1 #4 spawning seasons        
      
      ALB_n.flq     <- propagate(stock@stock.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1)),]
      ALB_m.flq     <- propagate(stock@m,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
      ALB_spwn.flq  <- propagate(stock@m.spwn,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
      
      #The modifications in mat due to an error in maturation from the function readFLSss3
      
      ALB_mat.flq   <- propagate(stock@mat,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
      ALB_fec.flq   <- propagate(stock@mat,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
      ALB_fec.flq[] <- 1  #eggs/kg #In ss3 Is assumed egg=wt*(a+b*wt); a=1 and b=0
      ALB_wt.flq    <- propagate(stock@stock.wt,ni,fill.iter=TRUE) [,as.character(first.yr:(proj.yr-1)),]
      
      ALB_range.min       <- ALB.age.min
      ALB_range.max       <- ALB.age.max
      ALB_range.plusgroup <- ALB.age.max
      ALB_range.minyear   <-  as.vector(stock@range["minyear"])
      ALB_range.minfbar   <- as.vector(stock@range["minfbar"])
      ALB_range.maxfbar   <- as.vector(stock@range["maxfbar"])
      
      
      
      
      #The modifications in mat due to an error in maturation from the function readFLSss3
      # ...................  Projection: ...................................
      #  we assume that the projection values of some variables are equal to 
      #  the average of some historical years:weight,fecundity,mortality and spawning    
      
      ALB_biol.proj.avg.yrs<- c((proj.yr-3):(proj.yr-1)) 
      #..................................................................
      #              FLBEIA input object: biols
      #..................................................................
      stks.data <- list(ALB=ls(pattern="^ALB")) 
      
      biols   <- create.biols.data(yrs,ns,ni,stks.data)
      
      M_0 <- as.numeric(biols$ALB@m[1,1])
      biolsMOD <- biols
      biolsMOD[[1]]@n[1,]<- biols[[1]]@n[1,]*exp(-M_0*5/12)  
      biolsMOD[[1]]@spwn[] <-0
      biolsMOD$ALB@m[1,] <- biols$ALB@m[1,]*7/12 # estimated from ss3
      
      # Section 7: Fleets -----------------------####
      
      # Data per fleet
      #    effort, crewshare, fcost, capacity
      # Data per fleet and metier
      #    effshare, vcost
      # Data per fleet, metier and stock
      #    landings.n, discards.n,landings.wt, discards.wt, landings, discards, landings.sel, discards.sel, price
      
      
      #........................................................
      #
      #     we need some extra functions
      #     to sum the catch.n and catch.wt across different fleets.
      #........................................................
      
      sum.catch.fleet <- function(bs,idx,var){
        aux <-0
        for(i in 1:length(idx)){
          aux <- aux + lapply(bs$fisheries, var)[[idx[i]]] #$A???
        }
        return(aux)}
      
      #........................................................
      
      flq   <- FLQuant(dimnames=list(age = 'all', year = hist.yrs,unit=1,season=1:ns, iter = 1:ni))
      flq1  <- FLQuant(1,dimnames=list(age = 'all', year = hist.yrs, unit=1,season=1:ns, iter = 1:ni))
      
      # BB 
      BB_effort.flq        <- flq1
      BB_capacity.flq      <- flq1*1e7
      BB.BB.ALB_landings.n.flq <- propagate(bs$fisheries[[1]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      BB.BB.ALB_landings.wt.flq <- propagate(bs$fisheries[[1]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_BB.ALB <- quantSums(BB.BB.ALB_landings.n.flq*BB.BB.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      
      #check total landings the same
      landings_BB.ALB/quantSums(bs$fisheries[[1]]$A@landings.n*bs$fisheries[[1]]$A@landings.wt)
      
      
      # BBisl 
      BBisl_effort.flq        <- flq1
      BBisl_capacity.flq      <- flq1*1e7
      BBisl.BBisl.ALB_landings.n.flq <- propagate(bs$fisheries[[2]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      BBisl.BBisl.ALB_landings.wt.flq <- propagate(bs$fisheries[[2]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_BBisl.ALB <- quantSums(BBisl.BBisl.ALB_landings.n.flq*BBisl.BBisl.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # TRGN 
      TRGN_effort.flq        <- flq1
      TRGN_capacity.flq      <- flq1*1e7
      TRGN.TRGN.ALB_landings.n.flq <- propagate(bs$fisheries[[3]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      TRGN.TRGN.ALB_landings.wt.flq <- propagate(bs$fisheries[[3]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_TRGN.ALB <- quantSums(TRGN.TRGN.ALB_landings.n.flq*TRGN.TRGN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # MWT 
      MWT_effort.flq        <- flq1
      MWT_capacity.flq      <- flq1*1e7
      MWT.MWT.ALB_landings.n.flq <- propagate(bs$fisheries[[4]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      MWT.MWT.ALB_landings.wt.flq <- propagate(bs$fisheries[[4]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_MWT.ALB <- quantSums(MWT.MWT.ALB_landings.n.flq*MWT.MWT.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # JPLLN 
      JPLLN_effort.flq        <- flq1
      JPLLN_capacity.flq      <- flq1*1e7
      JPLLN.JPLLN.ALB_landings.n.flq <- propagate(bs$fisheries[[5]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      JPLLN.JPLLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[5]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_JPLLN.ALB <- quantSums(JPLLN.JPLLN.ALB_landings.n.flq*JPLLN.JPLLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # JPLLs
      JPLLS_effort.flq        <- flq1
      JPLLS_capacity.flq      <- flq1*1e7
      JPLLS.JPLLS.ALB_landings.n.flq <- propagate(bs$fisheries[[6]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      JPLLS.JPLLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[6]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_JPLLS.ALB <- quantSums(JPLLS.JPLLS.ALB_landings.n.flq*JPLLS.JPLLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # TAILLN
      TAILLN_effort.flq        <- flq1
      TAILLN_capacity.flq      <- flq1*1e7
      TAILLN.TAILLN.ALB_landings.n.flq <- propagate(bs$fisheries[[7]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      TAILLN.TAILLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[7]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_TAILLN.ALB <- quantSums(TAILLN.TAILLN.ALB_landings.n.flq*TAILLN.TAILLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # TAILLS
      TAILLS_effort.flq        <- flq1
      TAILLS_capacity.flq      <- flq1*1e7
      TAILLS.TAILLS.ALB_landings.n.flq <- propagate(bs$fisheries[[8]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      TAILLS.TAILLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[8]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_TAILLS.ALB <- quantSums(TAILLS.TAILLS.ALB_landings.n.flq*TAILLS.TAILLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # USLLN
      USLLN_effort.flq        <- flq1
      USLLN_capacity.flq      <- flq1*1e7
      USLLN.USLLN.ALB_landings.n.flq <- propagate(bs$fisheries[[9]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      USLLN.USLLN.ALB_landings.wt.flq <- propagate(bs$fisheries[[9]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_USLLN.ALB <- quantSums(USLLN.USLLN.ALB_landings.n.flq*USLLN.USLLN.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # USLLS
      USLLS_effort.flq        <- flq1
      USLLS_capacity.flq      <- flq1*1e7
      USLLS.USLLS.ALB_landings.n.flq <- propagate(bs$fisheries[[10]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      USLLS.USLLS.ALB_landings.wt.flq <- propagate(bs$fisheries[[10]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_USLLS.ALB <- quantSums(USLLS.USLLS.ALB_landings.n.flq*USLLS.USLLS.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # VENLL
      VENLL_effort.flq        <- flq1
      VENLL_capacity.flq      <- flq1*1e7
      VENLL.VENLL.ALB_landings.n.flq <- propagate(bs$fisheries[[11]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      VENLL.VENLL.ALB_landings.wt.flq <- propagate(bs$fisheries[[11]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_VENLL.ALB <- quantSums(VENLL.VENLL.ALB_landings.n.flq*VENLL.VENLL.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # MIXKRPA
      MIXKRPA_effort.flq        <- flq1
      MIXKRPA_capacity.flq      <- flq1*1e7
      MIXKRPA.MIXKRPA.ALB_landings.n.flq <- propagate(bs$fisheries[[12]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      MIXKRPA.MIXKRPA.ALB_landings.wt.flq <- propagate(bs$fisheries[[12]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_MIXKRPA.ALB <- quantSums(MIXKRPA.MIXKRPA.ALB_landings.n.flq*MIXKRPA.MIXKRPA.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      
      # OthLL
      OthLL_effort.flq        <- flq1
      OthLL_capacity.flq      <- flq1*1e7
      OthLL.OthLL.ALB_landings.n.flq <- propagate(bs$fisheries[[13]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      OthLL.OthLL.ALB_landings.wt.flq <- propagate(bs$fisheries[[13]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_OthLL.ALB <- quantSums(OthLL.OthLL.ALB_landings.n.flq*OthLL.OthLL.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      
      # OthSurf
      OthSurf_effort.flq        <- flq1
      OthSurf_capacity.flq      <- flq1*1e7
      OthSurf.OthSurf.ALB_landings.n.flq <- propagate(bs$fisheries[[14]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      OthSurf.OthSurf.ALB_landings.wt.flq <- propagate(bs$fisheries[[14]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_OthSurf.ALB <- quantSums(OthSurf.OthSurf.ALB_landings.n.flq*OthSurf.OthSurf.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      # BBisls2
      BBisls2_effort.flq        <- flq1
      BBisls2_capacity.flq      <- flq1*1e7
      BBisls2.BBisls2.ALB_landings.n.flq <- propagate(bs$fisheries[[15]]$A@landings.n,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      BBisls2.BBisls2.ALB_landings.wt.flq <- propagate(bs$fisheries[[15]]$A@landings.wt,ni,fill.iter=TRUE)[,as.character(first.yr:(proj.yr-1))]
      landings_BBisls2.ALB <- quantSums(BBisls2.BBisls2.ALB_landings.n.flq*BBisls2.BBisls2.ALB_landings.wt.flq) [,as.character(first.yr:(proj.yr-1))]
      
      
      # Projection 
      #==============================================================================
      BB_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      BB.BB_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      BB.BB.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      BBisl_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      BBisl.BBisl_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      BBisl.BBisl.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      TRGN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      TRGN.TRGN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      TRGN.TRGN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      MWT_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      MWT.MWT_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      MWT.MWT.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      JPLLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      JPLLN.JPLLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      JPLLN.JPLLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      JPLLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      JPLLS.JPLLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      JPLLS.JPLLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      TAILLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      TAILLN.TAILLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      TAILLN.TAILLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      TAILLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      TAILLS.TAILLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      TAILLS.TAILLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      USLLN_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      USLLN.USLLN_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      USLLN.USLLN.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      USLLS_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      USLLS.USLLS_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      USLLS.USLLS.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      VENLL_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      VENLL.VENLL_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      VENLL.VENLL.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))  
      
      MIXKRPA_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      MIXKRPA.MIXKRPA_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      MIXKRPA.MIXKRPA.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))
      
      OthLL_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      OthLL.OthLL_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      OthLL.OthLL.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))
      
      
      OthSurf_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      OthSurf.OthSurf_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      OthSurf.OthSurf.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))
      
      
      BBisls2_proj.avg.yrs           <- ac((proj.yr-3):(proj.yr-1))
      BBisls2.BBisls2_proj.avg.yrs       <- ac((proj.yr-3):(proj.yr-1))   
      BBisls2.BBisls2.ALB_proj.avg.yrs   <- ac((proj.yr-3):(proj.yr-1))
      
      # ..................................................................
      # FLBEIA input object: fleets ####
      #..................................................................
      
      BB.BB_effshare.flq <- flq1
      BBisl.BBisl_effshare.flq <- flq1
      TRGN.TRGN_effshare.flq <- flq1
      MWT.MWT_effshare.flq <- flq1
      JPLLN.JPLLN_effshare.flq <- flq1
      JPLLS.JPLLS_effshare.flq <- flq1
      TAILLN.TAILLN_effshare.flq <- flq1
      TAILLS.TAILLS_effshare.flq <- flq1
      USLLN.USLLN_effshare.flq <- flq1
      USLLS.USLLS_effshare.flq <- flq1
      VENLL.VENLL_effshare.flq <- flq1
      MIXKRPA.MIXKRPA_effshare.flq <- flq1
      OthLL.OthLL_effshare.flq <- flq1
      OthSurf.OthSurf_effshare.flq <- flq1
      BBisls2.BBisls2_effshare.flq <- flq1
      
      fls.data <- list(BB=ls(pattern="^BB"), BBisl=ls(pattern="^BBisl"), TRGN=ls(pattern="^TRGN"), 
                       MWT=ls(pattern="^MWT"),JPLLN=ls(pattern="^JPLLN"),JPLLS=ls(pattern="^JPLLS"),
                       TAILLN=ls(pattern="^TAILLN"),TAILLS=ls(pattern="^TAILLS"),USLLN=ls(pattern="^USLLN"),
                       USLLS=ls(pattern="^USLLS"),VENLL=ls(pattern="^VENLL"),MIXKRPA=ls(pattern = "^MIXKRPA"),
                       OthLL=ls(pattern="^OthLL"),OthSurf=ls(pattern="^OthSurf"),BBisls2=ls(pattern = "^BBisls2")) 
      
      stks.data <- list(ALB=ls(pattern="^ALB")) 
      
      fleets<- create.fleets.data(yrs,ns,ni,fls.data,stks.data)
      
      
      #  Section 8: ALB SRs -----------------####
      
      ALB_sr.model        <- 'bevholt'
      ALB_params.n        <- 2
      ALB_params.name     <- c('a','b') 
      
      
      R0 <- exp(replist$parameters$Value[replist$parameters$Label=="SR_LN(R0)"])
      sigma <- (replist$parameters$Value[replist$parameters$Label=="SR_sigmaR"])
      
      h <- replist$parameters$Value[replist$parameters$Label=="SR_BH_steep"] # 09/23 AGUR
      B0 <- c(replist$derived_quants$Value[replist$derived_quants$Label=="SSB_Virgin"])
      
      alfa <- (B0/R0)*(1-h)/(4*h)   #Mangel et al 2010 "Reproductive ecology..."
      beta <- (5*h-1)/(4*h*R0)
      M_0 <- as.numeric(biolsMOD$ALB@m[1,1])
      #a1=(1/exp(-M_0/(1/(5/12))))*1/beta
      a1 <- 1/beta
      b1=alfa/beta
      ALB_params.array    <- array(as.vector(c(a1,b1)),
                                   dim = c(ALB_params.n,length(first.yr:last.yr),ns,ni), 
                                   dimnames = list(param=ALB_params.name,
                                                   year = ac(first.yr:last.yr),
                                                   season=ac(1:ns), iter = 1:ni))   
      rec1<- stock@stock.n[1,,] # numbers at age 1 in each unit and season   #UNIT RECRUITMENT 1000s
      
      ssb1 <- unitSums(ssb(stock)) # sum over all the units,all contribute to rec
      
      ALB_rec.flq         <- rec1[,as.character(first.yr:(proj.yr-1)),]           
      ALB_ssb.flq         <- ssb1[,as.character(first.yr:(proj.yr-1)),]
      
      sd1 <-replist$parameters$Value[replist$parameters$Label=="SR_sigmaR"]
      
      ALB_uncertainty.flq <- FLQuant(exp(rnorm(length(first.yr:last.yr)*ni*ns,mean=0,sd1)),
                                     dimnames=list(age='all',year=first.yr:last.yr,season=1:ns,iter=1:ni)) 
      
      prop1 <- replist$recruitment_dist$recruit_dist$recr_dist_F  #proportion of recruitment per season 
      
      ALB_proportion.flq <-FLQuant(rep(0,length(first.yr:last.yr)*ni),
                                   dimnames=list(age='all',year=first.yr:last.yr,season=1:ns,iter=1:ni)) 
      ALB_proportion.flq[,,,1][] <- prop1[1]
      age.rec1 <- 0 #the age at recruitment
      ss.rec1<- 1 
      ALB_timelag.matrix <- matrix(c(age.rec1,ss.rec1),2,ns,byrow=TRUE, dimnames = list(c('year', 'season'),season=1:ns)) 
      
      
      #..............................................................................
      #              FLBEIA input object: SRs
      #..............................................................................
      stks.data <- list(ALB=ls(pattern="^ALB")) 
      
      SRs      <- create.SRs.data(yrs,ns,ni,stks.data)
      
      
      SRsUnc <- SRs
      SRs[["ALB"]]@uncertainty[] <- 1
      
      #................................................
      #PLOTS SRS
      #............................................
      # 
      # h <- replist$parameters$Value[replist$parameters$Label=="SR_BH_steep"] # 09/23 AGUR
      # B0 <- c(replist$derived_quants$Value[replist$derived_quants$Label=="SSB_Virgin"])
      # 
      # alfa <- (B0/R0)*(1-h)/(4*h)   #Mangel et al 2010 "Reproductive ecology..."
      # beta <- (5*h-1)/(4*h*R0)
      # M_0 <- as.numeric(biols$ALB@m[1,1])
      # a1=1/beta
      # b1=alfa/beta
      # 
      # plot(c(ssb(stock)),c(rec(stock))*exp(-M_0/(1/(5/12))),xlim=c(0,B0))
      # lines(seq(0,B0,length=100),a1 * seq(0,B0,length=100)/(b1 + seq(0,B0,length=100)))
      
      #  Section 10: Advice:TAC/TAE/quota.share ------------ ####
      
      
      ALB_advice.TAC.flq<-FLQuant(NA,dimnames=list(year=first.yr:last.yr), iter = ni)
      ALB_advice.avg.yrs<- c((proj.yr-3):(proj.yr-1))
      ALB_advice.TAC.flq[,ac(first.yr:(proj.yr-1))] <- seasonSums(unitSums(quantSums(catchWStock(fleets,"ALB"))[,ac(first.yr:(proj.yr-1))]))
      
      
      #CORRECT AGUR 09/23
      
      ALB_advice.TAC.1.flq <- propagate(ALB_advice.TAC.flq, iter=1, fill.iter= TRUE)
      
      
      #..............................................................................
      #              FLBEIA input object: advice
      #..............................................................................
      rm(ALB_advice.TAC.flq)
      stks.data <- list(ALB=ls(pattern="^ALB")) 
      #CORRECT AGUR 09/23
      advice1 <- create.advice.data(yrs,ns,ni,stks.data,fleets)
      advice1$TAC['ALB',ac(proj.yr:last.yr)] <- 31153
      
      advice <- advice1
      #  Section 11: main.ctrl  ------------------------- ####
      
      
      main.ctrl           <- list()
      main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)
      
      
      #  Section 12: biols.ctrl ------------------------####
      
      growth.model     <- rep('ASPG',1)
      biols.ctrl       <- create.biols.ctrl (stksnames=stks,growth.model= growth.model)
      
      
      #  Section 13: fleets.ctrl ----------------------- ####
      
      
      n.fls.stks      <- rep(1,15)
      fls.stksnames   <- rep('ALB',15)
      
      effort.models    <- rep('SMFB',15)
      
      
      effort.restr.BB<- 'ALB'
      restriction.BB  <- 'catch'
      effort.restr.BBisl <-'ALB'
      restriction.BBisl <-'catch'
      effort.restr.TRGN <-'ALB'
      restriction.TRGN<-'catch'
      effort.restr.MWT<-'ALB'
      restriction.MWT <-'catch'
      effort.restr.JPLLN <-'ALB'
      restriction.JPLLN <-'catch'
      effort.restr.JPLLS <-'ALB'
      restriction.JPLLS <- 'catch'
      effort.restr.TAILLN <-'ALB'
      restriction.TAILLN <-'catch'
      effort.restr.TAILLS <-  'ALB'
      restriction.TAILLS <-'catch'
      effort.restr.USLLN <- 'ALB'
      restriction.USLLN <-'catch'
      effort.restr.USLLS<-'ALB'
      restriction.USLLS <-'catch'
      effort.restr.VENLL <- 'ALB'
      restriction.VENLL <-'catch'
      effort.restr.MIXKRPA<-'ALB'
      restriction.MIXKRPA<-'catch'
      effort.restr.MIXKRPA<-'ALB'
      restriction.MIXKRPA<-'catch'
      effort.restr.OthLL<-'ALB'
      restriction.OthLL<-'catch'
      effort.restr.OthSurf<-'ALB'
      restriction.OthSurf<-'catch'
      effort.restr.BBisls2<-'ALB'
      restriction.BBisls2<-'catch'
      
      catch.models     <- rep("CobbDouglasAge",15) #later change to Baranov
      capital.models   <- rep('fixedCapital',15)
      price.models     <- NULL    
      
      flq     <- FLQuant(dimnames=list(age = 'all', year = first.yr:last.yr,season=1:4, iter = 1:ni))
      
      
      fleets.ctrl.SMFB     <- create.fleets.ctrl(fls=fls,n.fls.stks=n.fls.stks,fls.stksnames=fls.stksnames,
                                                 effort.models= effort.models, catch.models=catch.models,
                                                 capital.models=capital.models, price.models=price.models,flq=flq)
      
      #............................ different options of fleets.ctrl.SMFB.................
      
      fleets.ctrl.SMFB.ALB    <- fleets.ctrl.SMFB
      
      
      fleets.ctrl.SMFB.ALB$BB$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$BBisl$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$TRGN$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$MWT$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$JPLLN$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$JPLLS$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$TAILLN$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$TAILLS$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$USLLN$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$USLLS$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$VENLL$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$MIXKRPA$effort.restr <- 'ALB'
      
      fleets.ctrl.SMFB.ALB$OthLL$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$OthSurf$effort.restr <- 'ALB'
      fleets.ctrl.SMFB.ALB$BBisls2$effort.restr <- 'ALB'
      
      
      for(fl in names(fleets)) fleets.ctrl.SMFB[[fl]]$effort.restr[] <- 'ALB'
      fleets.ctrl.SMFB$seasonal.share$ALB <- fleets.ctrl.SMFB$seasonal.share$ALB[,,1,1,1,1]
      fleets.ctrl.SMFB$seasonal.share$ALB[] <- 1
      
      
      
      #  Section 14: advice.ctrl ------------------- ####
      
      advice.ctrl   <- create.advice.ctrl(stksnames = stks, HCR.models = rep('fixedAdvice', length(stks)))
      
      
      
      #  Section 15: assess.ctrl ------------------ ####
      
      
      assess.models    <- rep('NoAssessment',length(stks))
      
      assess.ctrl      <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)
      
      
      #  Section 16: obs.ctrl --------------------------####
      
      stkObs.models <- c(rep('perfectObs',length(stks)))
      
      obs.ctrl        <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models)
      
      
      #  Section 17: covars ---------------------------####
      BDs <- NULL
      
      covars <- NULL
      #  Section 18: covars.ctrl -------------------------------####
      
      
      covars.ctrl <- NULL
      
      #........................................
      
      
      #OEM - INDICES
      
      #................................................................
      # 
      
      
      indices_ALB <- readFLIBss3(sc[sc.i],repfile=paste0("Report_",nrun,".sso"),compfile = paste0("CompReport_",nrun,".sso"))
      class(indices_ALB)
      names(indices_ALB) <- c("BB", "JPLLN","JPLLS","TAILLN", "TAILLS", "USLLN","USLLS", "VENLL" )
      indices_hist <- list("ALB")
      indices_hist$ALB <- list(BB=indices_ALB$BB,JPLLN=indices_ALB$JPLLN,JPLLS=indices_ALB$JPLLS,
                               TAILLN=indices_ALB$TAILLN,TAILLS=indices_ALB$TAILLS,USLLN=indices_ALB$USLLN,
                               USLLS=indices_ALB$USLLS,VENLL=indices_ALB$VENLL)
      
      indices <- indices_hist
      fl_num <- c(1,5:11)
      for(i in 1:length(names(indices_ALB))){
        indices$ALB[[i]]<- window(indices$ALB[[i]],start=indices$ALB[[i]]@range["minyear"],end=2057,extend=TRUE)
        
        #selection pattern is scaled by the maximum value per year but we want the raw values
        ageselex <- ss3[["ageselex"]]
        yr.fl <- dimnames(indices_hist$ALB[[i]]@sel.pattern)$year
        for(j in yr.fl ){
          ss3.sel.yr <- ageselex[ageselex$Factor=="Asel2" & ageselex$Fleet==fl_num[i] & ageselex$Yr==as.numeric(j),-c(1:7)]
          indices$ALB[[i]]@sel.pattern[,j] <- as.numeric(ss3.sel.yr)
          
        }
        indices$ALB[[i]]@index.q[,as.character(2022:2057)] <-  indices$ALB[[i]]@index.q[,as.character(tail(yr.fl,n=1)[1])] 
        indices$ALB[[i]]@sel.pattern[,as.character(2022:2057)] <-  yearMeans( indices$ALB[[i]]@sel.pattern[,as.character(tail(yr.fl,n=3))])
        indices$ALB[[i]]@catch.wt[] <-  ss3$endgrowth$Wt_Mid[1:16]
        
        if( !("2021" %in% yr.fl)){
          indices$ALB[[i]]@index.q[,as.character(2021)] <-  indices$ALB[[i]]@index.q[,as.character(tail(yr.fl,n=1)[1])] 
          indices$ALB[[i]]@sel.pattern[,as.character(2021)] <-  yearMeans( indices$ALB[[i]]@sel.pattern[,as.character(tail(yr.fl,n=3))])}
      }
      
      #### Adding the error to the Index in the projection ####
      
      #BB
      
      
      
      Nmid <- (biolsMOD[[1]]@n[,as.character(first.yr:(proj.yr-1))]*exp(-biolsMOD[[1]]@m[,as.character(first.yr:(proj.yr-1))]/2))-
        landStock(fleets,"ALB")[,as.character(first.yr:(proj.yr-1))]/2
      
      rm(randRes)
      load(paste0( in.OEM.data,"Rand_And_ResidualsAR_Fl1","_",nm[sc.i],".RData"))
      
      hy <- dimnames(indices_ALB[[1]])$year
      ny <- length(proj.yr:last.yr)
      
      indices$ALB[[1]]@index.q[,as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[1]]@index.q[,as.character(proj.yr:last.yr)]) +randRes[length(hy)+(1:ny)+(nrun-1)*(length(hy)+ny)])
       
      #ind_fl <- c(1,5:11)
      #indices in numbers
      
      ind_num <- c(2,3,6,7,8) #JPLL N no data 2021 and VENLL neither
      fl_num <- c(5,6,9:11) #ss3 fleet number
      
      for(i in 1:length(ind_num)){  #BB out different format the errors, errors has no autocorrelation for BB
        rm(randRes)
        load(paste0( in.OEM.data,"/Rand_And_ResidualsAR_Fl",fl_num[i],"_",nm[sc.i],".RData"))
        indices$ALB[[ind_num[i]]]@index.q[,as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[,as.character(proj.yr:last.yr)])+resProj[nrun,(1:ny)])}
     
      fl_num <- c(7,8)  # fleet index in ss3
      ind_num <- c(4,5)  #TAILLN AND TAILLS the index in FLIndex object
      
      for(i in 1:length(ind_num)){
        
        rm(randRes)
        load(paste0( in.OEM.data,"/Rand_And_ResidualsAR_Fl",fl_num[i],"_",nm[sc.i],".RData"))
        indices$ALB[[ind_num[i]]]@index.q[,as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[,c(as.character(proj.yr:last.yr))])+
                                                                                   resProj[nrun,(1:ny)])}
       
      #### 2021 index = observed value ####
      
      # Nmid <- (biolsMOD[[1]]@n[,as.character(first.yr:(proj.yr-1))]*exp(-biolsMOD[[1]]@m[,as.character(first.yr:(proj.yr-1))]/2))-
      #   landStock(fleets,"ALB")[,as.character(first.yr:(proj.yr-1))]/2
      # 
      ind_num <- c(1,3,6,7) #JPLL N no data 2021 and VENLL neither
      for(i in ind_num){
        VN2021 <- quantSums(Nmid[,as.character(proj.yr-1)]* indices$ALB[[i]]@sel.pattern[,as.character(proj.yr-1)])
        indices$ALB[[i]]@index.q[,as.character(proj.yr-1)] <- indices$ALB[[i]]@index[,as.character(proj.yr-1)]/VN2021
        } 
      
      ind_num <- c(4,5)  #TAILLN AND TAILLS
      for(i in ind_num){
        VB2021 <- quantSums(Nmid[,as.character(proj.yr-1)]* indices$ALB[[i]]@sel.pattern[,as.character(proj.yr-1)]*indices$ALB[[i]]@catch.wt[,as.character(proj.yr-1)])
        indices$ALB[[i]]@index.q[,as.character(proj.yr-1)] <- indices$ALB[[i]]@index[,as.character(proj.yr-1)]/VB2021 
        }
      
      
      
      #........................................
      
      
      #  OBS CONTROL
      
      #................................................................
      # 
      
      
      flq.ALB <- FLQuant(dimnames = dimnames(biolsMOD$ALB@n)) 
      
      #source("../MSE/code/ageMatInd.R")
      obs.ctrl <- create.obs.ctrl( stksnames = "ALB", n.stks.inds = 8, 
                                   stks.indsnames = names(indices$ALB),
                                   stkObs.models = "age2ageDat", 
                                   indObs.models = c("ageInd","ageInd","ageInd",
                                                     "bioInd","bioInd","ageInd","ageInd","ageInd"), #"ssbIND"
                                   flq.ALB = flq.ALB)
      
      obs.ctrl$ALB$indObs$BB$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$JPLLN$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$JPLLS$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$USLLN$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$USLLS$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$VENLL$indObs.model <- "VPNInd"
      obs.ctrl$ALB$indObs$TAILLN$indObs.model <- "VPBInd"
      obs.ctrl$ALB$indObs$TAILLS$indObs.model <- "VPBInd"
      obs.ctrl$ALB$indObs$TAILLN$sInd <- 1
      obs.ctrl$ALB$indObs$TAILLS$sInd <- 1
      
      FL_run <- ifelse(sc.i==1,0,
                       ifelse(sc.i==2,100,
                              ifelse(sc.i==3,200,300)))
      
      save(biols, biolsMOD,SRs, SRsUnc,BDs, fleets,indices, covars, advice, main.ctrl,
           biols.ctrl, fleets.ctrl.SMFB, fleets.ctrl.SMFB.ALB, 
           covars.ctrl, obs.ctrl, assess.ctrl, advice.ctrl,
           file = paste0(out.data,"FLinput_run_",irun+FL_run,".RData"))
      rm(list=ls())
    }
}
