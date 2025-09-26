require(r4ss)
require(dplyr)
source('sharepoint_path.R')

mod_vec = c('BaseCase', 'CPUE', 'Size', 'Age')
save_mod = list()
save_vals = list()
for(j in 1:length(mod_vec)) {
  mod_type = mod_vec[j]
  mod_path = file.path(shrpoint_path, "OM")
  sel_mods = read.csv(file.path(mod_path, mod_type, 'Results/SelectedRuns4OM.csv'))
  dat_file = SS_readdat(file.path(mod_path, mod_type, 'data.ss'), verbose = FALSE)
  ctl_file = SS_readctl(file.path(mod_path, mod_type, 'control.ss'), datlist = dat_file)
  save_par = list()
  for(k in 1:nrow(sel_mods)) {
    par_file = SS_readpar_3.30(parfile = file.path(mod_path, mod_type, paste0('ss', sel_mods$Runs[k], '.par')), datsource = dat_file, ctlsource = ctl_file, verbose = FALSE)
    save_par[[k]] = data.frame(iter = sel_mods$Runs[k], 
                               mod_type = mod_type,
                               M = par_file$MG_parms['NatM_p_1_Fem_GP_1', 'ESTIM'],
                               sigmaR = par_file$SR_parms['SR_sigmaR', 'ESTIM'])
  }
  save_par = bind_rows(save_par)
  save_vals[[j]] = save_par
  iter_M = save_par %>% arrange(M) %>% slice(c(1, floor(n()/2), n()))
  iter_sigmaR = save_par %>% arrange(sigmaR) %>% slice(c(1, floor(n()/2), n()))
  save_mod[[j]] = rbind(iter_M, iter_sigmaR)
}
save_mod = bind_rows(save_mod)
save_vals = bind_rows(save_vals)
write.csv(save_mod, 'docs/TechSpec/tables/selected_OM.csv', row.names = FALSE)
saveRDS(save_vals, file = 'docs/TechSpec/data/M-sigmaR_values.rds')
