# ============================================================
# Script: Estimate_CPUERef4JointIndexWeighted.R
#
# Purpose:
#   Calculate and explore weighted aggregate CPUE indices
#   for North Atlantic albacore and compare them with historical
#   stock status indicators (SSB/SSBMSY and F/FMSY).
#
# Inputs:
#   - SS3 assessment output
#   - Historical CPUE indices
#
# Outputs:
#   - Weighted aggregate index (Jindex)
#   - Diagnostic plots
#   - NormalizedCPUE2021_Jindex.csv
#   - Indices_Jindex_Weighted.csv
#
# Author: AZTI
# ============================================================

# ---------------------------------------------------------------------------
# Weighted Aggregate Index Analysis
#
# Objectives:
#
#   1. Extract historical CPUE series from the 2023
#      Atlantic albacore assessment.
#
#   2. Calculate stock status indicators:
#
#         SSB / SSBMSY
#         F / FMSY
#
#   3. Build a weighted aggregate CPUE index (Jindex)
#      using fleet-specific weights based on:
#
#         sigma = SD / (1 - AC)
#
#         weight = 1 / sqrt(sigma)
#
#   4. Compare the aggregate CPUE index with
#      historical stock status trajectories.
#
#   5. Generate figures to support the definition
#      of CPUE-based HCR reference points.
#
# Notes:
#
#   - CPUE_5_JPLL_N and CPUE_11_VENLL are excluded
#     from the aggregate index calculation.
#
#   - The weighted aggregate index is calculated
#     from 1999 onwards.
#
#   - Jindex is intended for the evaluation of
#     index-based management procedures.
#
# ---------------------------------------------------------------------------

library(ggpubr)
library(ggplot2)
library(r4ss)
library(ss3om)
library(dplyr)
library(tidyr)

proj_dir = here::here()
setwd(proj_dir)

source(file.path("code","Others","AuxiliaryFunctions.R"))

# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)



# Section 1: create directories ------------------- ####


dir_out <- file.path("HCR")
dir_plot <- file.path("HCR")


#### Section 2-3: Directory and Load the data------------------- ####


ss3.out.wd.1 <- "D:\\AZTI\\ALB - General\\Assessment\\Assessment_2023\\ALB_SS3_FinalVersionRecDev2018\\v28_forecast3_relf_v5_Fmsy08_2018_v3"
ss3.1 <- readOutputss3(ss3.out.wd.1)
out.stock.1 <- readFLSss3(ss3.out.wd.1, name="ALB")
bs.1 <- buildFLBFss330(ss3.1)



#................................................
#             stock
#
#...............................................

#
stock.nm <- "ALB"
ss3.stock <- ss3.1

ssbmsy <- ss3.stock$derived_quants[ss3.stock$derived_quants$Label=="SSB_MSY",]$Value
ssby <- ss3.stock$timeseries[ss3.stock$timeseries$Seas==1 & ss3.stock$timeseries$Yr>=1950 & ss3.stock$timeseries$Yr<=2021,]$SpawnBio
year <- ss3.stock$timeseries[ss3.stock$timeseries$Seas==1 & ss3.stock$timeseries$Yr>=1950 & ss3.stock$timeseries$Yr<=2021,]$Yr
Fmsy <- ss3.stock$derived_quants[ss3.stock$derived_quants$Label=="annF_MSY",]$Value
Fy <- ss3.stock$Kobe$F.Fmsy[ss3.stock$Kobe$Yr<=2021 & ss3.stock$Kobe$Yr>=1950]

#CPUE

unique(ss3.stock$Fleet)
ss3.stock$Fleet
ss3.stock.agg <- aggregate(Obs~Yr+Fleet,mean,data=ss3.stock$cpue)

cpue1 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==1]
cpue1.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==1]

cpue2 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==5]
cpue2.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==5]


cpue3 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==6]
cpue3.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==6]

cpue4 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==7]
cpue4.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==7]

cpue5 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==8]
cpue5.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==8]

cpue6 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==9]
cpue6.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==9]

cpue7 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==10]
cpue7.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==10]

cpue8 <- ss3.stock.agg$Obs[ss3.stock.agg$Fleet==11]
cpue8.yr <- ss3.stock.agg$Yr[ss3.stock.agg$Fleet==11]

#data.frame
df <- data.frame(year=c(rep(year,2),cpue1.yr,cpue2.yr,cpue3.yr,cpue4.yr,cpue5.yr,cpue6.yr,cpue7.yr,cpue8.yr),
                 indicator=c(rep("SSB/SSBmsy",length(ssby)),rep("F/Fmsy",length(Fy)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[1]),length(cpue1)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[5]),length(cpue2)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[6]),length(cpue3)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[7]),length(cpue4)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[8]),length(cpue5)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[9]),length(cpue6)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[10]),length(cpue7)),
                             rep(paste0("CPUE_",ss3.stock$FleetNames[11]),length(cpue8))),
                 value=c(ssby/ssbmsy,Fy,cpue1,cpue2,cpue3,cpue4,cpue5,cpue6,cpue7,cpue8))
summary(df)
df$indicator<- as.factor(as.character(df$indicator))


#.................................................
#     MEAN CPUE REF
#....................................................

weighted_mean_na <- function(x, w) {
  Ind_notNA <- !is.na(x)          # NOT NA VALUES
  weighted.mean(x[Ind_notNA],w[Ind_notNA])}

#mean 2010 because it's the year where the SSB is overe BMSY

sub_df <- df[!(df$indicator %in% c("CPUE_5_JPLL_N","CPUE_11_VENLL")),]

# weights
sd_Ind <- c(0.38, 0.36, 0.29, 0.33, 0.39, 0.37)
AC_Ind <- c(0.11, 0.39, 0.16, 0.56, 0.66, 0.59)
sigma  <- sd_Ind / (1 - AC_Ind)
w      <- round(1 / sqrt(sigma), 2)

names(w) <- c("CPUE_1_BB",
              "CPUE_6_JPLL_S",
              "CPUE_7_TAILL_N",
              "CPUE_8_TAILL_S",
              "CPUE_9_USLL_N",
              "CPUE_10_USLL_S")

# Función (la que ya tienes)
weighted_mean_na <- function(x, w) {
  Ind_notNA <- !is.na(x)
  weighted.mean(x[Ind_notNA], w[Ind_notNA])
}

# select the indices
sub_df_cpue <- sub_df[sub_df$indicator %in% names(w), ]


years  <- sort(unique(sub_df_cpue$year[sub_df_cpue$year>=1999]))
result <- data.frame(
  year           = years,
  weighted_index = rep(NA_real_, length(years))
)

for (i in seq_along(years)) {
  
  yr_data <- sub_df_cpue[sub_df_cpue$year == years[i], ]
  
  values  <- yr_data$value
  weights <- w[as.character(yr_data$indicator)]   
  
  result$value[i] <- weighted_mean_na(values, weights)
}

print(result)

Jindex <- result
Jindex$indicator <- "Jindex"


df_wref <- bind_rows(df, Jindex) 
summary(df_wref)



df_wide <- df_wref |>
  pivot_wider(
    names_from  = indicator,   # los valores de esta columna se convierten en nombres de columna
    values_from = value        # los valores que rellenan esas nuevas columnas
  )

as.data.frame(df_wide[df_wide$year=="2010",])
write.csv(df_wide,file="Data/NormalizedCPUE2021_Jindex.csv")
#.................................................
#     PLOTS
#....................................................

#SSB, F, CPUE


pAll <- ggplot(df,aes(x=year,y=value,col=indicator))+geom_line()+geom_point()+
  labs(x = "Year", y = "Value", fill = "Legend",title=stock.nm) +
  scale_x_continuous(limits = c(1950, 2019))+theme_minimal()+
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")


print(pAll)
SavePlot(file.path(paste0("TS_cpue_",stock.nm)),7,5)



p2 <- ggplot(df,aes(x=year,y=value,col=indicator))+geom_line()+geom_point()+
  labs(x = "Year", y = "Value", fill = "Legend",title=stock.nm) +
  # scale_fill_manual(values=c("red", "blue","green")) +
  scale_x_continuous(limits = c(2000, 2019))+theme_minimal()+
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")

print(p2)
SavePlot(file.path(paste0("TS_cpue_after2000_",stock.nm)),7,5)


#GM 
pAll_ref <- ggplot(df_wref,aes(x=year,y=value,col=indicator))+geom_line()+
  geom_point(size = 1.7) +
  geom_line(
    data = subset(df_wref, indicator %in% c("Jindex")),
    size = 1.2,color="red") +
  geom_point(
    data = subset(df_wref, indicator %in% c("Jindex")),
    size = 1.9,color="red") +
  geom_line(
    data = subset(df_wref, indicator %in% c("SSB/SSBmsy")),
    size = 1.2,color="black") +
  labs(x = "Year", y = "Value", fill = "Legend",title=stock.nm) +
  # scale_fill_manual(values=c("red", "blue","green")) +
  scale_x_continuous(limits = c(2000, 2019))+theme_minimal()+
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")




df[df$year==2010 & df$value>1,] #outptu fleet 7,8,9,10
sub_df <- df[df$indicator %in% c("F/Fmsy","SSB/SSBmsy","CPUE_7_TAILL_N","CPUE_8_TAILL_S",
                                 "CPUE_9_USLL_N","CPUE_10_USLL_S"),]

p3 <- ggplot(sub_df,aes(x=year,y=value,col=indicator))+geom_line()+geom_point()+
  labs(x = "Year", y = "Value", fill = "Legend",title=stock.nm) +
  # scale_fill_manual(values=c("red", "blue","green")) +
  scale_x_continuous(limits = c(2000, 2019))+theme_minimal()+
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")


print(p3)
SavePlot(file.path(paste0("Jindex_TS_sub_2010_cpue_after2000_",stock.nm)),7,5)
write.csv(df_wref,file=paste0(dir_out,"/Indices_Jindex_Weighted.csv"))



#.........EXTRA PLOTS

library(ggplot2)
library(scales)

special_colors <- c(
  "Jindex" = "red",
  "SSB/SSBmsy" = "black"
)

others <- setdiff(unique(df_wref$indicator), names(special_colors))

other_colors <- setNames(hue_pal()(length(others)), others)

all_colors <- c(special_colors, other_colors)

linew_vals <- c(
  setNames(rep(0.4, length(others)), others),  
  "Jindex" = 1.2,                              
  "SSB/SSBmsy" = 1.2                           
)


pAll_ref <- ggplot(df_wref, aes(x = year, y = value,
                                color = indicator,
                                linewidth = indicator)) +
  geom_line() +
  geom_point(size = 1.7) +
  scale_color_manual(values = all_colors, guide = "legend") +
  scale_linewidth_manual(values = linew_vals, guide = "legend") +
  labs(
    x = "Year",
    y = "Value",
    color = "Indicator",
    linewidth = "Indicator",
    title = stock.nm
  ) +
  scale_x_continuous(limits = c(1980, 2019)) + # clave para que aparezcan los CPUE
  theme_minimal() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")

print(pAll_ref)
#SavePlot(file.path(paste0("TS_JindexWeighted_",stock.nm)),7,5)

#after 2000
# Paso 1: Add the transparency in the data frame
alpha_val <- ifelse(df_wref$indicator %in% c("Jindex", "SSB/SSBmsy"),
1,    # opaco
0.3   # transparente
)

pAll_ref <- ggplot(df_wref, aes(x = year, y = value,
                                color = indicator,
                                linewidth = indicator,
                                alpha = alpha_val)) +  # <- usa la columna nueva
  geom_line() +
  geom_point(size = 1.7) +
  scale_color_manual(values = all_colors, guide = "legend") +
  scale_linewidth_manual(values = linew_vals, guide = "legend") +
  scale_alpha_identity() +  # <- usa los valores directamente, sin transformación
  labs(
    x = "Year",
    y = "Value",
    color = "Indicator",
    linewidth = "Indicator"
  ) +
  scale_x_continuous(limits = c(2000, 2021)) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
    axis.ticks = element_line(color = "black", linewidth = 0.5)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")

print(pAll_ref)
SavePlot(file.path(paste0("TS_JindexWeighted_after1999_",stock.nm)),7,5)


