# ============================================================
# Script: VPBInd.R
#
# Purpose:
# Simulate an index based on vulnerable biomass.
#
# Inputs:
# - FLBiol object
# - FLIndex object
# - Observation control settings
# - Fleet information
#
# Outputs:
# - Updated FLIndex object
#
# Author: AZTI
# ============================================================



VPBInd <- function (biol, index, obs.ctrl, year, stknm,fleets, ...) 
{
  it <- dim(biol@n)[6]
  ns <- dim(biol@n)[4]
  obs.yrs <- obs.ctrl$yrs
  for(i in 1:obs.yrs){
    yrnm.1 <- dimnames(biol@n)[[2]][year - i]
    if(is.na(index@index[, yrnm.1])){
  sInd <- 1
  VPB <- (biol@n[, yrnm.1, , sInd, ] * exp(-biol@m[, yrnm.1, , sInd, ]/2) - 
            landStock(fleets, name(biol))[, yrnm.1,  , sInd, ]/2) * index@sel.pattern[, yrnm.1, , sInd, ]*index@catch.wt[, yrnm.1, , sInd, ] 
  B <- quantSums(  VPB[, yrnm.1, , sInd, ] )
  index@index[, yrnm.1] <- B *index@index.q[, yrnm.1]
    }}
  return(index)
    
}