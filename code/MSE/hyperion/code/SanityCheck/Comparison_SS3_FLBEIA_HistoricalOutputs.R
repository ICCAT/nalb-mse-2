# ============================================================
# Script: Comparison_SS3_FLBEIA_HistoricalOutputs.R
#
# Purpose:
#   Compare historical trajectories from SS3 and FLBEIA
#   to validate the Operating Model conditioning process.
#
# Inputs:
#   - SS3 assessment outputs
#   - FLBEIA simulation outputs
#
# Outputs:
#   - Historical comparison figures
#   - Fleet-specific catch comparison figures
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# SS3 versus FLBEIA Historical Comparison
#
# Objectives:
#
#   1. Compare historical stock trajectories between
#      SS3 and FLBEIA:
#
#        - Spawning stock biomass (SSB)
#        - Catch
#        - Recruitment
#        - Fishing mortality
#
#   2. Verify that the FLBEIA Operating Model reproduces
#      the historical dynamics estimated by SS3.
#
#   3. Identify discrepancies introduced during the
#      conditioning process.
#
#   4. Compare fleet-specific catches between:
#
#        - SS3 observed catches
#        - FLBEIA reconstructed catches
#
#   5. Generate diagnostic figures to validate the
#      historical reconstruction used in the Operating Model.
#
# Notes:
#
#   - Comparisons are restricted to the historical period.
#
#   - F values are not directly comparable because SS3
#     reports F/FMSY whereas FLBEIA stores fishing mortality.
#
#   - Fleet-level comparisons are intended to verify the
#     consistency of reconstructed catches between models.
#
# ---------------------------------------------------------------------------



library(FLXSA)
library(FLAssess)
library(FLash)
library(FLCore)     
library(FLFleet)
library(FLBEIA)
library(ss3om)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)
#.......LOAD DATA .......................
nrun <- 1

in.data <- "FLinput/HistError_AR_CPUE_CatchL3y"
in.data <- "FLoutput/R1b/"
in.data <- "FLoutput/Rinit/"
plot.dir <- "Output/Figures/Conditioning/"

nm <- c("BaseCase","AGE","CPUE","SIZE")
sc <- paste0("OM/",nm)


#for(sc.i in 1:4){  #Different uncertainty grid
sc.i <- 1
#for SR

sel_runs <- read.csv(paste0(sc[sc.i],"/Results/SelectedRuns4OM.csv"))
srun <- sel_runs[nrun-(sc.i-1)*100,1]
#for(sc.i in 1:4){  #Different uncertainty grid
#for SR
replist <- SS_output(sc[sc.i],repfile=paste0("Report_",srun,".sso"),compfile = paste0("CompReport_",srun,".sso"),verbose=F,printstats=F)

load(paste0(in.data,"Output_run_",nrun,".RData"))

#................................................

stock.nm <- "ALB"
ss3.stock <- replist


biosub <- bio[bio$year <=2021 ,]
#df.EF0sub <- df.Ef0
summary(biosub)
#SSB

timeseries <- ss3.stock$timeseries[ss3.stock$timeseries$Seas==1,] 

p1 <- ggplot()+geom_line(data=timeseries[timeseries$Yr<=2021,],aes(x=Yr,y=SpawnBio,col="ss3 "),size=1.5)+
  geom_line(data=bio,aes(x=year,y=ssb,col=paste0("FLBEIA run ", nrun))) + 
  labs(x = "Year", y = "SSB", color = "Legend") +
  scale_color_manual(stock.nm, values=c("red", "blue")) +
  scale_x_continuous(limits = c(1940, 2018)) 

p1
#CATCH

timeseries <- aggregate(Obs~Yr, data=ss3.stock$catch,sum)

p2 <- ggplot()+geom_line(data=timeseries[timeseries$Yr<=2021,],aes(x=Yr,y=Obs,col="ss3"),size=1.5)+
  geom_line(data=biosub,aes(x=year,y=catch,col="FLBEIA")) + 
  labs(x = "Year",y = "Catch",color = "Legend") +
  scale_color_manual(stock.nm, values=c("red", "blue")) +
  coord_cartesian(xlim = c(1940, 2018)) 

p2

#REC

timeseries <- aggregate(Recruit_0~Yr, data=ss3.stock$timeseries[ss3.stock$timeseries$Yr> 1930,],sum)

p3 <- ggplot()+geom_line(data=timeseries[timeseries$Yr<=2021,],aes(x=Yr,y= Recruit_0,col="ss3"),size=1.5)+
  geom_line(data=biosub,aes(x=year,y=rec,col="FLBEIA")) + 
  labs(x = "Year", y = "Recruitment",color = "Legend") +
  scale_color_manual(stock.nm, values=c("red", "blue")) +
  coord_cartesian(xlim = c(1940, 2018)) 
p3
#F VALUES ARE NOT COMPARABLE

Fmsy <- ss3.stock$derived_quants$Value[ss3.stock$derived_quants$Label=="annF_MSY"]
Kobe <- ss3.stock$Kobe
Kobe$F_yr <- Kobe$F.Fmsy

timeseries <- Kobe
p4 <- ggplot()+geom_line(data=Kobe[Kobe$Yr<=2021,],aes(x=Yr,y= F_yr,col="ss3"),size=1.5)+
  geom_line(data=biosub,aes(x=year,y=f,col="FLBEIA")) + 
  labs(x = "Year", y = "F",color = "Legend") +
  scale_color_manual(stock.nm, values=c("red", "blue")) +
  coord_cartesian(xlim = c(1940, 2018)) 
p4
library(ggpubr)
pAll <- ggarrange(p1, p2, p3,p4,
                  #  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2,common.legend=TRUE)
pAll
plot.dir <- "Output/Figures/RefPts/Comparison_ss3_FLBEIA"
ggsave(paste0(plot.dir,"Comparison_SS3_FLBEIA_",stock.nm,"_Run", nrun,".jpg"),pAll, width = 3000, height = 3000, units = "px", dpi = 400)


#...............  catch by fleet..................


#CATCH
fl <- 15
timeseries <- aggregate(Obs~Yr, data=ss3.stock$catch[ss3.stock$catch$Fleet==fl,],sum)

df <- as.data.frame(Rinit$fleets[[fl]]@metiers[[1]]@catches[[1]]@landings)
p2 <- ggplot()+geom_line(data=timeseries[timeseries$Yr<=2021,],aes(x=Yr,y=Obs,col="ss3"),size=1.5)+
  geom_line(data=df,aes(x=year,y=data,col="FLBEIA")) + 
  labs(x = "Year",y = "Catch",color = "Legend") +
  scale_color_manual(stock.nm, values=c("red", "blue")) +
  coord_cartesian(xlim = c(1940, 2018)) 

p2
ggsave(paste0(plot.dir,"CompCatch_SS3_FLBEIA_",stock.nm,"_", ss3.stock$FleetNames[fl],".jpg"),p2, width = 3000, height = 3000, units = "px", dpi = 400)

