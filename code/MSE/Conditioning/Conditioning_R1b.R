# ============================================================
# Script: Conditioning_R1b.R
#
# Purpose:
#   Add observation error to historical and projected CPUE
#   indices in the conditioned FLBEIA input objects for the
#   Northern albacore Operating Model.
#
# Inputs:
#   - FLinput_run_<run>.RData files from FLinput/R1a
#   - SelectedRuns4OM.csv for each OM scenario
#   - SS3 Report_<run>.sso files
#   - SS3 CompReport_<run>.sso files
#   - Historical and projected OEM residuals
#
# Outputs:
#   - FLinput_run_<run>.RData files in FLinput/R1b
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Add Observation Error to FLBEIA Indices
#
# Objectives:
#
#   1. Read the previously conditioned FLBEIA input files
#      generated for the four Operating Model scenarios:
#
#        - BaseCase
#        - AGE
#        - CPUE
#        - SIZE
#
#   2. Recover the corresponding selected SS3 realization for
#      each FLBEIA run.
#
#   3. Rebuild the historical FLIndex objects using the SS3
#      index information, raw selectivity-at-age patterns and
#      catch weights-at-age.
#
#   4. Extend index catchability and selectivity over the
#      projection period from 2022 to 2057.
#
#   5. Calculate vulnerable abundance or vulnerable biomass
#      for each CPUE index, depending on the index type.
#
#   6. Correct index catchability using the relationship
#      between expected SS3 CPUE and the calculated vulnerable
#      abundance or biomass.
#
#   7. Add observation error to index catchability during both
#      the historical and projection periods:
#
#        - BB index using corrected random residuals
#        - Number-based indices using projected residuals
#        - Biomass-based indices using projected residuals
#
#   8. Recalculate the historical index values after applying
#      the catchability correction and observation error.
#
#   9. Save the updated FLBEIA input objects for subsequent
#      Management Strategy Evaluation simulations.
#
# Notes:
#
#   - The script processes 400 FLBEIA runs:
#
#        Runs   1-100  BaseCase
#        Runs 101-200  AGE
#        Runs 201-300  CPUE
#        Runs 301-400  SIZE
#
#   - Number-based indices are calculated from vulnerable
#     abundance and selectivity-at-age.
#
#   - TAILLN and TAILLS are biomass-based indices and include
#     catch weight-at-age in the vulnerable biomass calculation.
#
#   - The resulting R1b input files differ from the R1a files
#     through the incorporation of observation error in the
#     historical and projected CPUE indices.
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

nrun<-1  #test


for(nrun in  1:400){
  
   in.data <- "FLinput/R1a"
   in.OEM.data <- "OEM/InputMSE_OEM/ProjRes/"
   out.data <- "FLinput/R1b/"
   

  load(paste0(in.data,"/FLinput_run_",nrun,".RData"))

  nm <- c("BaseCase","AGE","CPUE","SIZE")
  sc <- paste0("OM/",nm)
  
    #for(i in 1:4){
  sc.i <- ifelse(nrun<=100,1,
         ifelse(nrun<=200,2,
                ifelse(nrun<=300,3,4)))
  
  sel_runs <- read.csv(paste0(sc[sc.i],"/Results/SelectedRuns4OM.csv"))
  
  FL_run <- ifelse(sc.i==1,0,
                   ifelse(sc.i==2,100,
                          ifelse(sc.i==3,200,300)))
  irun <- sel_runs[nrun-FL_run,1]
  
  indices_ALB <- readFLIBss3(sc[sc.i],repfile=paste0("Report_",irun,".sso"),compfile = paste0("CompReport_",irun,".sso"))
  class(indices_ALB)
  names(indices_ALB) <- c("BB", "JPLLN","JPLLS","TAILLN", "TAILLS", "USLLN","USLLS", "VENLL" )
  stks <- c('ALB')

  run.nm <- "TAC"
  
  first.yr          <- 1930
  proj.yr           <- 2022
  last.yr           <-2057
  
  ny <- length(proj.yr:last.yr)

#for(nrun in 1:35){

  #Run name
  
 
  ss3 <- readOutputss3(sc[sc.i],repfile=paste0("Report_",irun,".sso"),compfile = paste0("CompReport_",irun,".sso"))
  
  #EXAMPLE EFFORT 0
  
  main.ctrl$sim.years["final"] <- 2057
  main.ctrl$sim.years["initial"] <- 2022

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
    yr.fl <- dimnames(indices_hist$ALB[[i]]@index[!is.na(indices_hist$ALB[[i]]@index),])$year
    for(j in yr.fl ){
      ss3.sel.yr <- ageselex[ageselex$Factor=="Asel2" & ageselex$Fleet==fl_num[i] & ageselex$Yr==as.numeric(j),-c(1:7)]
      indices$ALB[[i]]@sel.pattern[,j] <- as.numeric(ss3.sel.yr)
    }
    indices$ALB[[i]]@index.q[,as.character(2022:2057)] <-  yearMeans( indices$ALB[[i]]@index.q[,as.character(tail(yr.fl,n=3))]) 
    indices$ALB[[i]]@sel.pattern[,as.character(2022:2057)] <-  yearMeans( indices$ALB[[i]]@sel.pattern[,as.character(tail(yr.fl,n=3))])
    indices$ALB[[i]]@catch.wt[] <-  ss3$endgrowth$Wt_Mid[1:16]
    
    if( !("2021" %in% yr.fl)){
      indices$ALB[[i]]@index.q[,as.character(2021)] <-  yearMeans(indices$ALB[[i]]@index.q[,as.character(tail(yr.fl,n=3))] )
      indices$ALB[[i]]@sel.pattern[,as.character(2021)] <-  yearMeans( indices$ALB[[i]]@sel.pattern[,as.character(tail(yr.fl,n=3))])}
  }
  
  #### Adding the error to the Index in the history and projection ####
  #......................................................................
  
  #BB different dimension
  
  
  Nmid <- (biolsMOD[[1]]@n[,as.character(first.yr:(proj.yr-1))]*exp(-biolsMOD[[1]]@m[,as.character(first.yr:(proj.yr-1))]/2))-
    landStock(fleets,"ALB")[,as.character(first.yr:(proj.yr-1))]/2
  
  rm(randRes)
  load(paste0( in.OEM.data,"Rand_And_ResidualsAR_Fl1","_",nm[sc.i],".RData"))
  hy <- dimnames(indices_ALB[[1]])$year
  ny <- length(proj.yr:last.yr)

  indexN <- quantSums(Nmid[,hy]* indices$ALB[[1]]@sel.pattern[,hy])*
          indices$ALB[[1]]@index.q[,hy]
  
  indices$ALB[[1]]@index.var[] <- mean(ss3$cpue$Exp[ss3$cpue$Fleet==1],na.rm=T)/mean(as.data.frame(indexN)$data,na.rm=T) 

  #q coIndexB#q corrected with var
  indices$ALB[[1]]@index.q[, hy] <- exp(log(indices$ALB[[1]]@index.q[, hy] *indices$ALB[[1]]@index.var[,hy])+ 
                                                   randRes_corrected[(1:length(hy))+(nrun-1)*(length(hy)+ny)])
  indices$ALB[[1]]@index.q[, as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[1]]@index.q[, as.character(proj.yr:last.yr)]*
                                                                                  indices$ALB[[1]]@index.var[,as.character(proj.yr:last.yr)])+
                                                                     +randRes_corrected[length(hy)+(1:ny)+(nrun-1)*(length(hy)+ny)])
  IndexWithError<- quantSums(Nmid[, hy] * indices$ALB[[1]]@sel.pattern[, hy])* 
    indices$ALB[[1]]@index.q[, hy]
  
  indices$ALB[[1]]@index[,hy] <- IndexWithError
  
  #indices in numbers
  
  ind_num <- c(2,3,6,7,8) #JPLL N no data 2021 and VENLL neither
  fl_num <- c(5,6,9:11) #ss3 fleet number
  
  for(i in 1:length(ind_num)){
    rm(randRes)
    hy <- dimnames(indices_ALB[[ind_num[i]]])$year
    load(paste0( in.OEM.data,"/Rand_And_ResidualsAR_Fl",fl_num[i],"_",nm[sc.i],".RData"))
    indexN <- quantSums(Nmid[, hy] * indices$ALB[[ind_num[i]]]@sel.pattern[, hy] ) *
      indices$ALB[[ind_num[i]]]@index.q[, hy]
    indices$ALB[[ind_num[i]]]@index.var[] <- mean(ss3$cpue$Exp[ss3$cpue$Fleet==fl_num[i]],na.rm=T)/mean(as.data.frame(indexN[, hy])$data,na.rm=T) 
    
        #q coIndexB#q corrected with var
    indices$ALB[[ind_num[i]]]@index.q[, hy] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[, hy] *indices$ALB[[ind_num[i]]]@index.var[,hy])+ 
                                                     +resProj[nrun-(sc.i-1)*100,1:length(hy)])
    indices$ALB[[ind_num[i]]]@index.q[, as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[, as.character(proj.yr:last.yr)]*
                                                                                    indices$ALB[[ind_num[i]]]@index.var[,as.character(proj.yr:last.yr)])+
                                                                                resProj[nrun-(sc.i-1)*100,(length(hy)+1):(ny+length(hy))])
    
    IndexWithError<- quantSums(Nmid[, hy] * indices$ALB[[ind_num[i]]]@sel.pattern[, hy])* 
      indices$ALB[[ind_num[i]]]@index.q[, hy]
    
     indices$ALB[[ind_num[i]]]@index[,hy] <- IndexWithError
    
    
     }     
  
  #indices in biomass
  fl_num <- c(7,8)  # fleet index in ss3
  ind_num <- c(4,5)  #TAILLN AND TAILLS the index in FLIndex object
  
  for(i in 1:length(ind_num)){
    rm(randRes)
    hy <- dimnames(indices_ALB[[ind_num[i]]])$year
    
    load(paste0( in.OEM.data,"Rand_And_ResidualsAR_Fl",fl_num[i],"_",nm[sc.i],".RData"))
    
    Index<- quantSums(Nmid[, hy] * indices$ALB[[ind_num[i]]]@sel.pattern[, hy] *
                                 indices$ALB[[ind_num[i]]]@catch.wt[, hy]) * indices$ALB[[ind_num[i]]]@index.q[, hy]
    
    
    indices$ALB[[ind_num[i]]]@index.var[] <- mean(ss3$cpue$Exp[ss3$cpue$Fleet==fl_num[i]],na.rm=T)/mean(as.data.frame(Index)$data,na.rm=T) 
    #q corrected with var
    indices$ALB[[ind_num[i]]]@index.q[, hy] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[, hy] *indices$ALB[[ind_num[i]]]@index.var[,hy])+ 
                                                     resProj[nrun-(sc.i-1)*100,1:length(hy)])
    indices$ALB[[ind_num[i]]]@index.q[, as.character(proj.yr:last.yr)] <- exp(log(indices$ALB[[ind_num[i]]]@index.q[, as.character(proj.yr:last.yr)]*
                                                                                 indices$ALB[[ind_num[i]]]@index.var[,as.character(proj.yr:last.yr)])+
                                                                                resProj[nrun-(sc.i-1)*100,(length(hy)+1):(ny+length(hy))])
   
    IndexWithError<- quantSums(Nmid[, hy] * indices$ALB[[ind_num[i]]]@sel.pattern[, hy]* indices$ALB[[ind_num[i]]]@catch.wt[, hy]) *
      indices$ALB[[ind_num[i]]]@index.q[, hy]
    
    indices$ALB[[ind_num[i]]]@index[,hy] <- IndexWithError
    
    }
  
  save(biols, biolsMOD,SRs, SRsUnc,BDs, fleets,indices, covars, advice, main.ctrl,
       biols.ctrl, fleets.ctrl.SMFB, fleets.ctrl.SMFB.ALB, 
       covars.ctrl, obs.ctrl, assess.ctrl, advice.ctrl,
       file = paste0(out.data,"FLinput_run_",nrun,".RData"))
}
