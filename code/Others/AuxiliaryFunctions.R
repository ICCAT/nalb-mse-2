# ============================================================
# Script: AuxiliaryFunctions.R
#
# Purpose:
# Collection of auxiliary plotting and transformation
# functions used by FLBEIA visualization scripts.
#
# Inputs:
# - FLBEIA output objects
# - Reference points
#
# Outputs:
# - Standardized ggplot themes
# - Saved figures
# - Derived biomass reference indicators
#
# Author: AZTI
# ============================================================

# -------------------------------------------------------------------------
# SAVEPLOT

SavePlot<-function(plotname,width=8,height=4){
  file <- file.path(dir_plot,paste0(plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300)
}



# -------------------------------------------------------------------------
# PLOT SETTINGS

theme_fun <-function(){
  theme_bw()+
    theme(axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size=14, angle=0),
          axis.text.y = element_text(size=14, angle=0),
          title=element_text(size=14,angle=0),
          legend.text = element_text(size=8))
}

# -------------------------------------------------------------------------
# transform ssb2Btarget and f2Ftarget

# -------------------------------------------------------------------------
# PLOT SETTINGS
transfBio <- function(bio_1,iter,ref.pts){
  for(i in iter){
    print(i)
  bio_1$value[bio_1$indicator=="ssb2Btarget" & bio_1$iter==i] <- 
    bio_1$value[bio_1$indicator=="ssb" & bio_1$iter==i]/ref.pts$SSB_MSY[ref.pts$iter==i]
  bio_1$value[bio_1$indicator=="f2Ftarget" & bio_1$iter==i] <- 
    bio_1$value[bio_1$indicator=="f" & bio_1$iter==i]/ref.pts$F_MSY[ref.pts$iter==i]
    }
  return(bio_1)
  }


transfBioSRmod <- function(bio_1,iter,ref.pts,alpha){
  for(i in iter){
    print(i)
    bio_1$value[bio_1$indicator=="ssb2Btarget" & bio_1$iter==i] <- 
      bio_1$value[bio_1$indicator=="ssb" & bio_1$iter==i]/(ref.pts$SSB_MSY[ref.pts$iter==i]*alpha)
    bio_1$value[bio_1$indicator=="f2Ftarget" & bio_1$iter==i] <- 
      bio_1$value[bio_1$indicator=="f" & bio_1$iter==i]/ref.pts$F_MSY[ref.pts$iter==i]
  }
  return(bio_1)
}
