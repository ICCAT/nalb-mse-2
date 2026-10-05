# ============================================================
# Script: AlbHCR.R
#
# Purpose:
#   Calculate TAC advice using a biomass-based harvest control
#   rule derived from SPiCT reference points.
#
# Inputs:
#   - FLStock object
#   - Advice object
#   - Advice control settings
#   - SPiCT reference points stored in covars
#
# Outputs:
#   - Updated TAC advice
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# ALB Biomass-Based HCR
#
# HCR structure:
#
#   Ftarget   = Ftar × FMSY
#   Fminimum  = Fmin × FMSY
#
#   Btrigger  = Btrigger × BMSY
#   Blimit    = Blim × BMSY
#
#                   | B × Fminimum
#           TAC =   | (a + b × B/BMSY) × FMSY × B
#                   | B × Ftarget
#
# Annual TAC change constraints:
#
#   Increase limited by maxRange
#   Decrease limited by minRange
#
# Notes:
#   - Biomass is calculated as SSB.
#   - FMSY and BMSY are obtained from SPiCT outputs.
#   - TAC advice is constrained by:
#         * maximum annual increase
#         * maximum annual decrease
#         * maximum TAC
#
# ---------------------------------------------------------------------------


AlbHCR <- function (stocks, advice, advice.ctrl, year, stknm,covars, ...) {
  nyears <- ifelse(is.null(advice.ctrl[[stknm]][["nyears"]]), 
                   3, advice.ctrl[[stknm]][["nyears"]])
  stk <- stocks[[stknm]]
  stk@harvest[stk@harvest < 1e-12 | is.na(stk@harvest)] <- 1e-12
  stk@catch.n[is.na(stk@catch.n)] <- 1e-06
  stk@landings.n[is.na(stk@landings.n)] <- 0
  stk@discards.n[is.na(stk@discards.n)] <- 1e-06
  stk@catch.n[stk@catch.n == 0] <- 1e-06
  stk@landings.n[stk@landings.n == 0] <- 1e-06
  stk@discards.n[stk@discards.n == 0] <- 0
  stk@catch <- computeCatch(stk)
  stk@catch[is.na(stk@catch)] <- 0
  stk@landings <- computeLandings(stk)
  stk@discards <- computeDiscards(stk)
  ageStruct <- ifelse(dim(stk@m)[1] > 1, TRUE, FALSE)
  
  ref.pts <- advice.ctrl[[stknm]]$ref.pts
  Cadv <- ifelse(advice.ctrl[[stknm]][["AdvCatch"]][year + 
                                                      1] == TRUE, "catch", "landings")

  iter <- dim(stk@m)[6]
  yrsnames <- dimnames(stk@m)[[2]]
  yrsnumbs <- as.numeric(yrsnames)
  assyrname <- yrsnames[year]
  assyrnumb <- yrsnumbs[year]
  int.yr <- advice.ctrl[[stknm]]$intermediate.year
  for (i in 1:iter) {
    stki <- iter(stk, i)
    int.yr <- ifelse(is.null(int.yr), "Fsq", int.yr)

    
      # b.datyr <- (stk.aux@stock.n * stk.aux@stock.wt)[, year, drop = TRUE]
      b.datyr <- ssb(stki)[, year-1, drop = TRUE]

      Fmsy <-covars$ALB$spict_Fmsy[,year-1]
      Bmsy <- covars$ALB$spict_Bmsy[,year-1]
      
      Ftar <-  ref.pts["Ftar", i] *Fmsy
      Fmin<-  ref.pts["Fmin", i] *Fmsy
      Btrigger <- ref.pts["Btrigger", i] *Bmsy
      Blim <-  ref.pts["Blim", i] *Bmsy
      maxRange <-  ref.pts["maxRange", i] 
      minRange <-  ref.pts["minRange", i]    
      maxTAC <- ref.pts["maxTAC", i] 
      
      b.pos <- findInterval(b.datyr, c(Blim,Btrigger))
      
      a <- (Ftar/Fmsy)-(((Ftar-Fmin)/Fmsy)/((Btrigger-Blim)/Bmsy))*Btrigger/Bmsy
      b <-((Ftar-Fmin)/Fmsy)/((Btrigger-Blim)/Bmsy)
      TAC_i <- ifelse(b.pos == 0, b.datyr*Fmin,
                    ifelse(b.pos == 1, (a+b*b.datyr/Bmsy)*Fmsy*b.datyr, b.datyr*Ftar))
      print(TAC_i)
      if (is.na(TAC_i) | TAC_i == 0) {
        advice[["TAC"]][stknm, year + 1:nyears, , , , i] <- 0
        next
      }
    
   # slot(stki, Cadv)[, year+1] <- TAC_i
    
    #Restrictions
    delta_yy<- (TAC_i-advice[["TAC"]][stknm, year, , , , i])/advice[["TAC"]][stknm, year, , , , i]
    
    if(delta_yy>maxRange) TAC_i= advice[["TAC"]][stknm,year, , , , i]*(1+maxRange)
    if(delta_yy< -minRange) TAC_i= advice[["TAC"]][stknm, year, , , , i]*(1-minRange)
    
    nmaxyear <- dim(advice[["TAC"]][stknm,])[2]
    #nmaxyear <- (dims(biols[[1]])$year)
    if((year+nyears) > nmaxyear){
    advice[["TAC"]][stknm, (year + 1):nmaxyear, , , , i] <- min(TAC_i,maxTAC)
    }else{
      advice[["TAC"]][stknm, year + 1:nyears, , , , i] <- min(TAC_i,maxTAC)}
  }
  return(advice)
}