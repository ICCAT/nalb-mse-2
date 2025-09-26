rm(list = ls())
require(r4ss)
require(dplyr)
require(stringr)
source('sharepoint_path.R')

mod_vec = c('BaseCase', 'CPUE', 'Size', 'Age')
mod_path = file.path(shrpoint_path, 'OM')
save_par = list()
save_dq = list()
save_ts = list()
save_kobe = list()
i_count = 1
for(j in 1:length(mod_vec)) {
  load(file.path(mod_path, mod_vec[j], "Results/Output.RData"))
  sel_mods = read.csv(file.path(mod_path, mod_vec[j], 'Results/SelectedRuns4OM.csv'))
  for(k in 1:nrow(sel_mods)) {
    this_report = SumReport[[sel_mods$Run[k]]]
    # Parameters:
    par_est = this_report$parameters
    par_df = data.frame(M = par_est['NatM_Lorenzen_Fem_GP_1', 'Value'],
                        LAmin = par_est['L_at_Amin_Fem_GP_1', 'Value'],
                        LAmax = par_est['L_at_Amax_Fem_GP_1', 'Value'],
                        K = par_est['VonBert_K_Fem_GP_1', 'Value'],
                        SD1 = par_est['SD_young_Fem_GP_1', 'Value'],
                        SDA = par_est['SD_old_Fem_GP_1', 'Value'],
                        R0 = par_est['SR_LN(R0)', 'Value'],
                        h = par_est['SR_BH_steep', 'Value'],
                        sigmaR = par_est['SR_sigmaR', 'Value'])
    par_df$mod_type = mod_vec[j]
    par_df$iter = sel_mods$Run[k]
    save_par[[i_count]] = par_df
    # Derived quantities:
    quant_est = this_report$derived_quants
    quant_df = quant_est %>% filter(Label %in% c('SSB_Virgin', "SSB_MSY", "annF_MSY", "Dead_Catch_MSY",
                                      paste0("SSB_", this_report$endyr),
                                      paste0("F_", this_report$endyr))) %>%
                select(Label, Value)
    quant_df$mod_type = mod_vec[j]
    quant_df$iter = sel_mods$Run[k]
    save_dq[[i_count]] = quant_df
    # Time series:
    ts_df = quant_est %>% filter(Label %in% c(paste0("SSB_", this_report$startyr:this_report$endyr),
                                      paste0("F_", this_report$startyr:this_report$endyr))) %>%
      select(Label, Value) %>% mutate(year = as.numeric(str_split_i(Label, "_", 2)),
                                      var = str_split_i(Label, "_", 1))
    ts_df$mod_type = mod_vec[j]
    ts_df$iter = sel_mods$Run[k]
    save_ts[[i_count]] = ts_df
    # Kobe:
    kobe_df = ts_df
    kobe_df$Value[kobe_df$var == 'SSB'] = kobe_df$Value[kobe_df$var == 'SSB'] / quant_est$Value[quant_est$Label == 'SSB_MSY']
    kobe_df$Value[kobe_df$var == 'F'] = kobe_df$Value[kobe_df$var == 'F'] / quant_est$Value[quant_est$Label == 'annF_MSY']
    kobe_df$mod_type = mod_vec[j]
    kobe_df$iter = sel_mods$Run[k]
    save_kobe[[i_count]] = kobe_df
    # i count:
    i_count = i_count + 1
  }
  rm(SumReport)
  cat(paste0("Model set ", j, " of ", length(mod_vec), " completed.\n"))
}

# Merge:
save_par = bind_rows(save_par)
save_dq = bind_rows(save_dq)
save_ts = bind_rows(save_ts)
save_kobe = bind_rows(save_kobe)

# Set factor levels:
save_par = save_par %>% mutate(mod_type = factor(mod_type, levels = mod_vec))
save_dq = save_dq %>% mutate(mod_type = factor(mod_type, levels = mod_vec))
save_ts = save_ts %>% mutate(mod_type = factor(mod_type, levels = mod_vec))
save_kobe = save_kobe %>% mutate(mod_type = factor(mod_type, levels = mod_vec))

# Save:
saveRDS(save_par, file = file.path('docs/TechSpec/data', 'summ_par.rds'))
saveRDS(save_dq, file = file.path('docs/TechSpec/data', 'summ_dq.rds'))
saveRDS(save_ts, file = file.path('docs/TechSpec/data', 'summ_ts.rds'))
saveRDS(save_kobe, file = file.path('docs/TechSpec/data', 'summ_kobe.rds'))
