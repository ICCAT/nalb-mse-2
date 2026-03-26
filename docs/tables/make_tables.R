rm(list = ls())
require(dplyr)
source('sharepoint_path.R')
tbl_dir = 'docs/TechSpec/tables'

# Read SS3 model
mymod = SS_output(file.path(shrpoint_path, "Assessment/Assessment_2023/ALB_SS3_FinalVersionRecDev2018/v28_forecast3_relf_v5_Fmsy08_2018_v3"))

# -------------------------------------------------------------------------
# Fleet table
mytab = data.frame(Fleet = mymod$FleetNames)
mytab$CPUE = 'No'
mytab$CPUE[sort(unique(mymod$cpue$Fleet))] = 'Yes'
mytab = mytab %>% mutate(Description = c("Baitboat (Spain, France)",
                                         "Baitboat islands (Portugal Madeira/Azores, Spain Canary) for quarters 1, 3, and 4",
                                         "Troll (Spain, France) and Gillnets (France, Ireland)",
                                         "Mid-water trawl (France, Ireland)",
                                         "Japan longline north 30",
                                         "Japan longline south 30",
                                         "Taiwan longline north 30",
                                         "Taiwan longline south 30",
                                         "US and Canada longline north 30",
                                         "US longline south 30",
                                         "Venezuela longline",
                                         "Mixed flags longline (KR, PA, CHN)",
                                         "Other longline",
                                         "Other surface gears",
                                         "Baitboat islands (Portugal Madeira/Azores, Spain Canary) for quarter 2"
                                         ), .before = 'CPUE')
saveRDS(mytab, file.path(tbl_dir, 'FleetInfo.rds'))

# -------------------------------------------------------------------------
# Table OEM rho residuals:

load(file.path(shrpoint_path, 'OEM/InputMSE_OEM/ProjRes/Rand_And_ResidualsAR_Fl1_BaseCase.RData'))
these_cpue = sort(unique(ss3$cpue$Fleet))
save_df = list()
for(i in seq_along(these_cpue)) {
  err_df = ss3$cpue %>% filter(Fleet == these_cpue[i])
  err_vec = err_df %>% pull(Dev)
  fleet_name = unique(err_df$Fleet_name)
  ac_check = acf(err_vec, plot = FALSE)
  significance_level = qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(err_vec)))
  tmp_df = data.frame(Fleet = fleet_name, rho = ac_check$acf[,,1][2], 
                      sigma = sd(err_vec), lag = 1,
                      sig_level = significance_level)
  tmp_df = tmp_df %>% mutate(sig_acf = if_else(abs(rho) > sig_level, 'Yes', 'No'))
  save_df[[i]] = tmp_df
}
save_df = bind_rows(save_df)
save_df$rho = round(save_df$rho, 2)
save_df$sigma = round(save_df$sigma, 2)
saveRDS(save_df, file.path(tbl_dir, 'OEM_residuals.rds'))
