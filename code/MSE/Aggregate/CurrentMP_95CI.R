
# ============================================================
# Script: CurrentMP_95CI.R
#
# Purpose:
# Load the aggregated FLBEIA output for the scenario with the 
# current MP,compute relative values of SSB/SSBmsy and F/Fmsy 
# and generate quantile summaries (90% CI and 95% CI) for the biological, 
# fleet, metier and advice components.
#
# Inputs:
# - <sc_run>_AggregatedOutput_ALB.RData : FLBEIA aggregated
# output per run (dir: FLoutput/Summary/ModelBased_25var/)
# - RefPts_FLBRP_format.csv : operating model
# reference points (dir: Output/Tables/)
# - AuxiliaryFunctions.R : auxiliary functions
# - sharepoint_path.R : Sharepoint path config
#
# Outputs:
# - <sc_run>_Aggregated_bio_FLBRP_RefPts.RData : biological
# output with reference points applied
# - <sc_run>_Aggregated_bio_FLBRP_RefPts_Q.RData : quantile
# summaries (bioQ, bioQ90, bioQ95, fltQ, fltStkQ,
# mtQ, mtStkQ, advQ)
#
# Author: AZTI
# ============================================================





library(FLBEIA)
library(FLBEIAshiny)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source(file.path('code','Others','AuxiliaryFunctions.R'))
source('sharepoint_path.R')
setwd(shrpoint_path)

#  path:
dir_in <-  file.path("FLoutput","Summary","ModelBased_25var")
dir_out <- dir_in

#  scenarios:
sc_nm_all <- c("Ftg0.8_25%")

sc_run <-  c("2S31")

#reference points OM
ref.pts <- read.csv(file.path("Output","Tables","RefPts_FLBRP_format.csv"))

#input file name
file_nm <- c(paste0(sc_run, "_AggregatedOutput_ALB.RData"))

refPtsMod <- TRUE

#### Aggregate all the runs by scenario####

j<-1
  
  load(paste0(dir_in,"/",file_nm[j]))
  sc_nm <-sc_nm_all[j]
  
  bio_sc$scenario <- sc_nm
  iter <- unique(bio_sc$iter)
  #estimate relative values SSB/SSBmsy and F/Fmsy
  if(!(sc_run[j] %in% c("2S31_R0dw","2S31_R0up","2S33_R0dw","2S33_R0up"))){
  alpha <- 1
  }else{
   if(sc_run[j] %in% c("2S31_R0dw","2S33_R0dw")) alpha<-0.8   #Ro down scenario Bmsy should decrease 0.8 R0dw and 1.2 R0up
   if(sc_run[j] %in% c("2S31_R0up","2S33_R0up")) alpha<-1.2
  }
  bio_refpts <- transfBioSRmod(bio_sc,iter,ref.pts,alpha)
  
  flt_sc$scenario <- sc_nm
  fltStk_sc$scenario <- sc_nm
  mt_sc$scenario <- sc_nm
  mtStk_sc$scenario <- sc_nm
  adv_sc$scenario <- sc_nm

  save(bio_refpts,file=paste0(dir_out,"/",sc_run[j], "_Aggregated_bio_FLBRP_RefPts.RData"))
  
  bioQ95 <- bioSumQ(bio_refpts,prob=c(0.975,0.5,0.025))
  bioQ90 <- bioSumQ(bio_refpts)
  bioQ <- bioSumQ(bio_refpts)
  fltQ <- fltSumQ(flt_sc)
  fltStkQ <- fltStkSumQ(fltStk_sc)
  mtQ <- mtSumQ(mt_sc)
  mtStkQ <- mtStkSumQ(mtStk_sc)
  advQ <- advSumQ(adv_sc)
  
  save(bioQ90,bioQ,bioQ95,
       fltQ,
       fltStkQ,
       mtQ,
       mtStkQ,
       advQ,
       file=paste0(dir_out,"/",sc_run[j], "_Aggregated_bio_FLBRP_RefPts_Q.RData"))



