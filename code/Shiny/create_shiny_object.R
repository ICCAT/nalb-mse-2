# IMPORTANT:
# Once the .flbeia object is created, move it to the shinyFLBEIA repository ('data' folder)

rm(list = ls())
library(dplyr)
library(tibble)
library(tidyr)
library(colorspace)
source('sharepoint_path.R')
source('code/Shiny/translate_text.R')
# Load standard functions to prepare input data:
source("C:/Use/GitHub/shinyFLBEIA/prepare_input/prepare_input_functions.R")

# -------------------------------------------------------------------------
thr_ssb = 0.1 # minimum value SSB allowed
thr_catch = 0.1 # minimum catch value allowed
thr_f_min = 1e-06 # minimum F allowed
thr_f_max = 5 # maximum F allowed (F in FLBEIA is not harvest rate)
nyears_tac = 3 # number of years TAC Period
sim_yr_str = 2027 # first year for PM calculation (projection period)
nsim = 100 # number of iterations
all_sim_yr = 1930:2057 # historial and projection years

# Title MSE
title_en = "North Atlantic Albacore"
title_es = "Atún Blanco del Atlántico Norte"
title_fr = "Thon Blanc de l'Atlantique Nord"

# Summary MSE info:
summary_en = "

Results of the North Atlantic Albacore (N-ALB, *Thunnus alalunga*) management strategy evaluation (MSE). 
The historical period corresponds to 1930 to 2026. The projection period corresponds to 2027 to 2057.
Results for 4 sets of Operating Models (OMs, 1 Reference and 3 Robustness), 7 management procedures (MPs), and 400 OM iterations are included in this Shiny app.
Find out more information about this MSE [on this site](https://iccat.github.io/nalb-mse-2/).

"
summary_es = "

Resultados de la evaluación de la estrategia de gestión (MSE) del atún blanco del Atlántico Norte (N-ALB, *Thunnus alalunga*). 
El período histórico abarca de 1930 a 2026. El período de proyección abarca de 2027 a 2057.
En esta aplicación Shiny se incluyen los resultados de 4 conjuntos de modelos operativos (OMs, 1 Referencia y 3 Robustez), 7 procedimientos de gestión (MP), y 400 iteraciones de OM.
Para obtener más información sobre esta MSE, consulte [esta página web](https://iccat.github.io/nalb-mse-2/).

"
summary_fr = "

Résultats de l'évaluation de la stratégie de gestion (MSE) du thon blanc de l'Atlantique Nord (N-ALB, *Thunnus alalunga*). 
La période historique s'étend de 1930 à 2026. La période de projection s'étend de 2027 à 2057.
Cette application Shiny présente les résultats de 4 ensembles de modèles opérationnels (OM : 1 de référence et 3 de robustesse), de 7 procédures de gestion (MP) et 400 itérations de OM.
Pour en savoir plus sur cette MSE, consultez [ce site](https://iccat.github.io/nalb-mse-2/).

"

# -------------------------------------------------------------------------
# Read Ref Points

# Read OM/iter category:
om_iter_p = read.csv(file.path(shrpoint_path, 'RefPts/Analysis/RefPtsOM/RefPts.csv')) %>% 
  select(iter, scenario) %>% 
  mutate(scenario = gsub(pattern = "OM/", replacement = "", x = scenario)) 
# Make iter from 1 to nsim:
om_iter_p = om_iter_p %>% mutate(iter_gr = iter %% nsim) %>%
                mutate(iter_gr = if_else(iter_gr == 0, nsim, iter_gr))

# Read reference points OM:
ref_points = read.csv(file.path(shrpoint_path, 'RefPts/RefPts_FLBRP.csv')) %>% 
  rename(iter = OM) %>%
  select(SSB_MSY, F_MSY, iter)


# -------------------------------------------------------------------------
# Define MP information matrix and OM factor vector:
MP_info = data.frame(Code = c('25%_F0.8_B1', '10%_F0.8_B1', 
                                '25%_F1_B1', '10%_F1_B1',
                                '15%_PCCatch',
                                '10%_Emp_W', '25%_Emp_W'
                        ),
                        Type = c(rep('Model-Based', times = 4),
                                 rep('Empirical', times = 3)),
                        Description = c('TAC from model-based HCR: F_tgt=0.8F_msy, B_thr=B_msy. Maximum TAC increase of 25% and decrease of 20% between consecutive management periods. TAC cannot exceed 50,000 t. This is equivalent to the current MP.',
                                        'TAC from model-based HCR: F_tgt=0.8F_msy, B_thr=B_msy. Maximum TAC change of 10% between consecutive management periods. TAC cannot exceed 50,000 t.',
                                        'TAC from model-based HCR: F_tgt=F_msy, B_thr=B_msy. Maximum TAC increase of 25% and decrease of 20% between consecutive management periods. TAC cannot exceed 50,000 t.',
                                        'TAC from model-based HCR: F_tgt=F_msy, B_thr=B_msy. Maximum TAC change of 10% between consecutive management periods. TAC cannot exceed 50,000 t.',
                                        'TAC is constant (42,000 t) if combined index above reference value. Maximum TAC change of 15% between consecutive management periods.',
                                        'TAC from empirical HCR. Weighting to derive combined index. Maximum TAC change of 10% between consecutive management periods. TAC cannot exceed 50,000 t.',
                                        'TAC from empirical HCR. Weighting to derive combined index. Maximum TAC increase of 25% and decrease of 20% between consecutive management periods. TAC cannot exceed 50,000 t.'
                        )
)
OM_Factor_info <- data.frame(Factor = c("Reference", "R_dec", "R_inc", "R_var_inc"),
                              Description=c("Reference set", 
                                            "Robustness set: 20% decrease in unfished recruitment level in projection period", 
                                            "Robustness set: 20% increase in unfished recruitment level in projection period",
                                            "Robustness set: 20% increase in recruitment variability in projection period") )
OM_Level_info <- data.frame(Level = c("Base", "CPUE", "Size", "Age"),
                            Description=c("No Upweight", 
                                          "Upweight CPUE", 
                                          "Upweight marginal size compositions",
                                          "Upweight conditional age-at-length data") )


# -------------------------------------------------------------------------
# Read FLBEIA outputs
# Define paths for FLBEIA outputs:

# MP/OM path vector:
mp_path_vec = c(# Model based:
            paste0("ModelBased_25var/", c("2S31", "2S31_R0dw", "2S31_R0up", "2S31_sigma"), "_AggregatedOutput_ALB"),
            paste0("ModelBased_10var/", c("2S31", "2S31_R0dw", "2S31_R0up", "2S31_sigma"), "_AggregatedOutput_ALB"),
            paste0("ModelBased_25var/", c("2S33", "2S33_R0dw", "2S33_R0up", "2S33_sigma"), "_AggregatedOutput_ALB"),
            paste0("ModelBased_10var/", c("2S33", "2S33_R0dw", "2S33_R0up", "2S33_sigma"), "_AggregatedOutput_ALB"),
            # PCCatch
            paste0("PCC/", c("PCC2", "PCC2_R0dw", "PCC2_R0up", "PCC2_sigma"), "_AggregatedOutput_ALB"),
            # Empirical
            paste0("EMP/", c("EMPW8", "EMPW8_R0dw", "EMPW8_R0up", "EMPW8_sigma"), "_AggregatedOutput_ALB"), # 10%
            paste0("EMP/", c("EMPW3", "EMPW3_R0dw", "EMPW3_R0up", "EMPW3_sigma"), "_AggregatedOutput_ALB") # 25%
)

# Matrix: n_MPs x n_OMs
mp_path = matrix(mp_path_vec, nrow = nrow(MP_info), ncol = nrow(OM_Factor_info), byrow = TRUE)

# IMPORTANT: for some robustness tests, ref points are rescaled below

# Read outputs:
mp_list = list()
tac_list = list()
catch_list = list()
i_list = 1
for(j in 1:ncol(mp_path)) { # OM loop
  for(k in 1:nrow(mp_path)) { # MP loop
  
  this_path = mp_path[k,j]  
    
  if(!is.na(this_path)) {
  
    load(file.path(shrpoint_path, "FLoutput/Summary", paste0(this_path, '.RData')))
    
    # BIOLOGY:
    tmp_dat = bio_sc %>% select(stock, year, iter, indicator, value) %>%
      filter(indicator %in% c('catch', 'f', 'ssb', 'rec')) %>%
      pivot_wider(names_from = "indicator", values_from = "value")
  
    # Add ref points:
    tmp_dat = tmp_dat %>% left_join(ref_points, by = 'iter')
  
    # Cleaning: make sure you do not get weird values:
    tmp_dat = tmp_dat %>% mutate(ssb = if_else(ssb <= 0, thr_ssb, ssb),
                                 f = if_else(f > thr_f_max | is.na(f), thr_f_max, f),
                                 f = if_else(f < thr_f_min, thr_f_min, f),
                                 catch = if_else(catch <= thr_catch, thr_catch, catch))
    
    # IMPORTANT!!! 
    # Only rescale ref points for robustness tests if needed:
    if(j == 2) tmp_dat = tmp_dat %>% mutate(SSB_MSY = if_else(year >= sim_yr_str, SSB_MSY*0.8, SSB_MSY))
    if(j == 3) tmp_dat = tmp_dat %>% mutate(SSB_MSY = if_else(year >= sim_yr_str, SSB_MSY*1.2, SSB_MSY))
    
    # Calculate bbmsy and ffmsy:
    tmp_dat = tmp_dat %>% mutate(bbmsy = ssb/SSB_MSY, ffmsy = f/F_MSY)
    
    # Add TAC information:
    tmp_dat = tmp_dat %>% left_join(adv_sc %>% ungroup() %>% 
                              filter(indicator == 'tac') %>%
                              select(stock, year, iter, value) %>%
                              rename(tac = value), by = c("stock", "year", "iter"))
    
    # Sort TAC data by management period:
    tac_dat = tmp_dat %>% filter(year >= sim_yr_str) %>% mutate(fore_yr = year - sim_yr_str + 1,
                                                                tac_period = ceiling(fore_yr/nyears_tac)) 
    tac_dat = tac_dat %>% group_by(stock, iter, tac_period) %>% summarise(tac = mean(tac), .groups = 'drop')
    
    mp_list[[i_list]] = tmp_dat %>% mutate(MP = MP_info$Code[k], OM = OM_Factor_info$Factor[j])
    tac_list[[i_list]] = tac_dat %>% mutate(MP = MP_info$Code[k], OM = OM_Factor_info$Factor[j])
    
    # CATCH:
    cth_dat = fltStk_sc %>% select(stock, year, fleet, iter, indicator, value) %>%
      filter(indicator %in% c('catch', 'quotaUpt')) %>%
      pivot_wider(names_from = "indicator", values_from = "value")
    # Check if NaN in quota uptake due to catch = 0
    cth_dat$quotaUpt[is.nan(cth_dat$quotaUpt)] = 0
    catch_list[[i_list]] = cth_dat %>% mutate(MP = MP_info$Code[k], OM = OM_Factor_info$Factor[j])
    
    # Remove objects:
    rm(bio_sc, flt_sc, fltStk_sc, adv, adv_sc, mt_sc, mtStk_sc)
    
    # List indicator
    i_list = i_list + 1
  
  }
  
  cat("OM", j, "- MP", k, "completed.", "\n")
  
  }
}

# Now merge all MP data frames:
mp_merged = bind_rows(mp_list)
tac_merged = bind_rows(tac_list)
catch_merged = bind_rows(catch_list)

# Remove objects:
rm(mp_list, tac_list, catch_list)

# -------------------------------------------------------------------------
# Create object:
myoutput = list()
myoutput$n_sim = nsim # number of iterations

# -------------------------------------------------------------------------
# Set base information:
myoutput$title = title_en
myoutput$date = Sys.Date()
myoutput$summary <- summary_en

# Stock identifier:
myoutput$stocks = c("ALB") # should match the name in FLBEIA outputs
myoutput$n_stocks = length(myoutput$stocks)

# -------------------------------------------------------------------------
# Set Management Procedures information:
myoutput$mp = list()
myoutput$mp$metadata = MP_info

# Prepare MP info:
myoutput = MP_process(myoutput)

# -------------------------------------------------------------------------
# Set Operating Models:
myoutput$om = list()
# OM Factor:
myoutput$om$metadata$factor = OM_Factor_info
myoutput$om$metadata$level = OM_Level_info

# Set levels in OM df:
om_iter = om_iter_p %>% mutate(scenario = factor(scenario, 
                                               levels = c("BaseCase", "CPUE", "SIZE", "AGE"),
                                               labels = OM_Level_info$Level))

# -------------------------------------------------------------------------
# Add time series information:
myoutput$timeseries = list()
myoutput$timeseries$metadata <- data.frame(
  Code=c('SB/SB_MSY', 'F/F_MSY', 'Catch'),
  Label=c('SB/SB_MSY', 'F/F_MSY', 'Catch'),
  Description=c('Spawning biomass relative to spawning biomass at MSY',
                'Fishing mortality relative to fishing mortality at MSY',
                'Catch (tonnes)')
)

# select column names of chosen variables:
sel_var = c('bbmsy', 'ffmsy', 'catch')

# Add some info:
myoutput$timeseries$time <- all_sim_yr
myoutput$timeseries$timenow <- sim_yr_str - 1 # last historical time step
myoutput$timeseries$timelab <- 'Year'

# Prepare TS info:
myoutput = TS_process(myoutput)

# -------------------------------------------------------------------------
# Kobe plot
myoutput$kobe = list()
myoutput$kobe$metadata <- data.frame(Code=c('SB/SBMSY', 'F/FMSY'),
                                     Label=c('SB/SBMSY', 'F/FMSY'),
                                     Description = c('Spawning biomass relative to SB_MSY',
                                                     'Fishing mortality relative to F_MSY')
)

# select column names of chosen variables:
sel_var = c('bbmsy', 'ffmsy')

# Projection period:
myoutput$kobe$time <- sim_yr_str:max(all_sim_yr)
# Ref line for Kobe Time:
myoutput$kobe$kobe_target = 0.6 # percentage green area (as fraction)

# Prepare KOBE info:
myoutput = KOBE_process(myoutput)

# -------------------------------------------------------------------------
# Performance Indicators information:
# MUST HAVE AT LEAST TWO PIs

myoutput$pi = list()
myoutput$pi$metadata <- data.frame(Code=c('minB', 'meanB', 'meanF',
                                          'PGK', 'PRK', 'PBlim', 'PBmsy', 
                                          'Tstr', 'Tmed', 'Tlon',  
                                          'Tsd', 'Tc', 'PTcx', 'Tcmax'),
                                   Type = c(rep('Status', times = 5),
                                            rep('Safety', times = 2),
                                            rep('Yield', times = 3),
                                            rep('Stability', times = 4)),
                                   Description=c('Minimum spawner biomass (SB) relative to SB at maximum sustainable yield (SB_MSY)',
                                                 'Mean SB relative to SB_MSY',
                                                 'Mean fishing mortality (F) relative to F at MSY',
                                                 'Prob. Green Kobe Quadrant',
                                                 'Prob. Red Kobe Quadrant',
                                                 'Prob. SB > 40%SB_MSY',
                                                 'Prob. SB_MSY > SB > 40%SB_MSY',
                                                 'Mean TAC (Short Term, 1-3 Years)',
                                                 'Mean TAC (Medium Term, 5-10 Years)',
                                                 'Mean TAC (Long Term, 15-30 Years)',
                                                 'Standard Deviation in TAC',
                                                 'Mean Absolute Change in TAC',
                                                 'Prob. TAC Change (%) above 10%',
                                                 'Max. TAC change (%) Between Periods')
)

# Prepare PI info:
myoutput = PI_process(myoutput)

# -------------------------------------------------------------------------
# PREPARE CATCH INFORMATION by fleet:
myoutput$fleet = list()
myoutput$fleet$metadata <- data.frame(Code=c("BB", "BBisl", "TRGN", "MWT", "JPLLN", "JPLLS", 
                                             "TAILLN", "TAILLS", "USLLN", "USLLS", "VENLL", "MIXKRPA", 
                                             "OthLL", "OthSurf", "BBisls2"),
                                      Type = c('BB', 'BB', 'Others', 'Others', 'LL', 'LL', 
                                               'LL', 'LL', 'LL', 'LL', 'LL', 'Others',
                                               'LL', 'Others', 'BB'),
                                      Description=c('Baitboat (Spain, France)',
                                                    'Baitboat islands (Portugal Madeira/Azores, Spain Canary) for quarters 1, 3, and 4',
                                                    'Troll (Spain, France) and Gillnets (France, Ireland)',
                                                    'Mid-water trawl (France, Ireland)',
                                                    'Japan longline north 30',
                                                    'Japan longline south 30',
                                                    'Taiwan longline north 30',
                                                    'Taiwan longline south 30',
                                                    'US and Canada longline north 30',
                                                    'US longline south 30',
                                                    'Venezuela longline',
                                                    'Mixed flags longline (KR, PA, CHN)',
                                                    'Other longline',
                                                    'Other surface gears',
                                                    'Baitboat islands (Portugal Madeira/Azores, Spain Canary) for quarter 2')
)

# Variable information:
myoutput$fleet$variables <- data.frame(
  Code=c('Long-Term Catch'),
  Description=c('Average Catch in Projection Years 15-30')
)

# select column names of chosen variables:
sel_var = c('catch')

# Prepare FLEET info:
myoutput = FLEET_process(myoutput)

# -------------------------------------------------------------------------
# Translate text:
# Update function if description text changes
myoutput = translate_info(myoutput)

# -------------------------------------------------------------------------
# Save Object:
saveRDS(myoutput, file.path("docs/data", 'NALB.flbeia'), compress = "xz") # save it here to download
# Copy object to Shiny folder:
file.copy(from = file.path("docs/data", 'NALB.flbeia'),
          to = file.path("C:/Use/GitHub/shinyFLBEIA/data", 'NALB.flbeia'), overwrite = TRUE)
