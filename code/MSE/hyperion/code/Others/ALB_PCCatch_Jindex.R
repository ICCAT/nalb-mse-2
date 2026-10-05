# ============================================================
# Script: ALB_PCCatch_Jindex.R
#
# Purpose:
#   Calculate TAC advice using a pseudo-constant catch harvest
#   control rule based on an aggregate biomass index.
#
# Inputs:
#   - FLIndices object
#   - Advice object
#   - Advice control settings
#
# Outputs:
#   - Updated TAC advice
#   - Aggregate index (Ind_J)
#   - Jratio values
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# ALB Pseudo-Constant Catch HCR
#
# HCR structure:
#
#   Inow = mean(Iy-2, Iy-1, Iy)
#
#   Jratio = Inow / Jref
#
#                     | maxTAC                  if Jratio >= 1
#   TAC[y+1] = TAC =  |
#                     | (maxTAC - Cmin) * Jratio + Cmin
#                                              if Jratio < 1
#
# Annual TAC change constraints:
#
#                         | prevTAC * (1 + maxRange)
#   TACconstraint = min   |
#                         | prevTAC * (1 - minRange)
#
#
# Index options:
#   type = 1  Arithmetic mean
#   type = 2  Weighted mean
#
# Notes:
#   - A single aggregate index is used per stock.
#   - The aggregate index can be calculated as either a simple
#     average or a weighted average across indices.
#   - Historical and projected values are stored in
#     advice$covars.
#
# ---------------------------------------------------------------------------


ALB_PCCatch_Jindex_HCR <- function(indices, advice, advice.ctrl, year, stknm,...){
  
  Idnms <- advice.ctrl[[stknm]][['index']]  # either the name or the position of the index in FLIndices object.
  minRange <- advice.ctrl[[stknm]][['ref.pts']]['minRange',] #[it]
  maxRange  <- advice.ctrl[[stknm]][['ref.pts']]['maxRange',] #[it]
  JRef  <- advice.ctrl[[stknm]][['ref.pts']]['Jref',]
  maxTAC  <- advice.ctrl[[stknm]][['ref.pts']]['maxTAC',]
  Cmin  <- advice.ctrl[[stknm]][['ref.pts']]['Cmin',]
  type <-  advice.ctrl[[stknm]][['type']]
 
  year_idx <- year
  nass <- advice.ctrl[[stknm]][['nass']]<-3
  
   #dimensions
  Ids_Jrat <- NULL
  minYr <- min(sapply(indices[[stknm]], function(x) x@range["minyear"]),na.rm = TRUE)
  imin <- which.min(sapply(indices[[stknm]], function(x) x@range["minyear"]))
  years <- advice.ctrl[[stknm]]$adv.year
  yrnm    <- dimnames(advice$TAC)[[2]][year_idx]
  last.yr <- as.numeric(yrnm)
  yrnms_id <- as.character(minYr:(last.yr-1))
  
  flq <- window(indices[[stknm]][[imin]]@index, minYr, last.yr-1)
  flq[] <- NA
  
  #covars 
  
  slot_names <- c(paste0("Ind_",Idnms),"Ind_J","Jrat")
  
  if (!(any(slot_names %in% names(advice$covars[[stknm]])))) {
   #create object for the first time
    advice$covars <- list()
    advice$covars[[stknm]] <- list()
    for (j in 1:(length(slot_names)-2)) {
     Idnm <- Idnms[j]
      advice$covars[[stknm]][[j]] <- window(indices[[stknm]][[Idnm]]@index,minYr,last.yr-1)
    }
    advice$covars[[stknm]][["Ind_J"]] <- flq
    advice$covars[[stknm]][["Jrat"]] <- flq
    names(advice$covars[[stknm]]) <- slot_names
 
  }else {
  #extend object
    for (j in 1:length(slot_names)) {
      advice$covars[[stknm]][[j]] <- window(advice$covars[[stknm]][[j]], minYr, 
                                  last.yr)
    }
   }
  
  
  Jmat <- matrix(NA, nrow=length(Idnms),ncol=length(yrnms_id))
  
  for(i in 1:length(Idnms)){
    Idnm <- Idnms[i]
    Id <- window(indices[[stknm]][[Idnm]]@index,minYr,last.yr-1) 
    Jmat[i,] <- Id 

  }
  
  if(type==1){  #not weighted mean
    advice$covars[[stknm]][["Ind_J"]][,yrnms_id] <- apply(Jmat, 2, mean, na.rm = TRUE)}
  
  if(type==2){  #not weighted mean
   sd_Ind <- advice.ctrl[[stknm]][['sd_Ind']]
   AC_Ind <- advice.ctrl[[stknm]][['AC_Ind']]

   sigma <- sd_Ind/(1-AC_Ind)
   w <- round(1/sqrt(sigma),2)

  advice$covars[[stknm]][["Ind_J"]][,yrnms_id] <- apply(Jmat, 2, weighted_mean_na, w = w)}
  
  flq_J <- advice$covars[[stknm]][["Ind_J"]][,yrnms_id]
  flq_ref <- flq[]
  flq_ref[] <- JRef
  flq_now <- flq[]
  for(i in 3:(length(yrnms_id))) {
         
   flq_now[, as.character(yrnms_id[i])] <- 
      (flq_J[, as.character(yrnms_id[i-1])] +
       flq_J[, as.character(yrnms_id[i-2])]+
       flq_J[, as.character(yrnms_id[i])]) / 3
  }  

  
  #Jrat     
  flq_Jrat <- round(flq_now/flq_ref,3)
  advice$covars[[stknm]][["Jrat"]][,yrnms_id] <- flq_Jrat
  Jrat <- advice$covars[[stknm]][["Jrat"]][,as.character(last.yr -1)] 
 
 
  b=Cmin
  a=( maxTAC-Cmin)
  
  TAC <- ifelse(Jrat < 1, a*Jrat+b,  maxTAC) 
  
  #cONSTRAINT

  prev_yrTAC <-   advice$TAC[stknm,year_idx,]
  year_ass <-  min(year_idx+nass,length(dimnames(advice$TAC[stknm,,])$year))
  

  Const_TAC <- ifelse(TAC>prev_yrTAC, min(TAC, prev_yrTAC*(maxRange+1)),
                   ifelse(TAC<prev_yrTAC, max(TAC, prev_yrTAC*(1-minRange)), TAC))

   
   
  advice$TAC[stknm,(year_idx+1):year_ass,] <- Const_TAC
  
  #fill covars
  yrnm.ind <- as.character(as.numeric(yrnm)-1)
  for(i in 1:(length(slot_names)-2)){
  advice$covars[[stknm]][[i]][, yrnms_id] <- Jmat[i,]}

  return(advice=advice)
}


#Auxiliar function

weighted_mean_na <- function(x, w) {
  Ind_notNA <- !is.na(x)          # NOT NA VALUES
  weighted.mean(x[Ind_notNA],w[Ind_notNA])}    