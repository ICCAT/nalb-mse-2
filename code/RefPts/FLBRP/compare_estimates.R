# ============================================================
# Script: Compare_RefPts_FLR_SS3.R
#
# Purpose:
#   Compare MSY biological reference points (Fmsy, SSBmsy,
#   MSY) estimated with FLR (FLBRP and a custom R function)
#   against those obtained directly from SS3, across all
#   Monte Carlo runs. Produces diagnostic violin plots to
#   assess consistency between estimation methods.
#
# Inputs:
#   - RefPtsOM/<ss_files>  : SS3 reference points per scenario
#                            (BaseCase, AGE, CPUE, SIZE)
#   - estimates/est_*.csv  : FLBRP and Rfun reference points
#                            per run, produced by
#                            4_calculate_refpts.R
#   - RefPtsOM_FLR_biolsM_updateFLBRP_stock2020.csv :
#                            FL2020 FLR estimates for
#                            historical comparison
#
# Outputs:
#   - RefPts_FLBRP.csv     : merged FLBRP reference points
#                            across all runs
#   - figures/compare_1.png: SS3 vs FLBRP vs Rfun (violin)
#   - figures/compare_2.png: FLBRP minus Rfun differences
#   - figures/compare_3.png: FLBRP minus SS3 differences
#   - figures/compare_4.png: FL2020 minus SS3 differences
#
#
# Author: AZTI
# ============================================================
require(dplyr)
require(tidyr)
require(ggplot2)

#path

project_directory <- here::here()
setwd(project_directory)

source("sharepoint_path.R")

if (!exists("shrpoint_path")) {
  stop(
    "Object 'shrpoint_path' was not created by sharepoint_path.R."
  )
}

if (!dir.exists(shrpoint_path)) {
  stop(
    "SharePoint directory not found: ",
    shrpoint_path
  )
}

setwd(shrpoint_path)


# Save figures:
fig_path = "RefPts/Analysis/Figures"
dir.create(fig_path)

ss_path = "RefPts"
# Make sure you get the right order:
ss_files = c("BaseCase_ss3.csv", "AGE_ss3.csv", "CPUE_ss3.csv", "SIZE_ss3.csv") 

# Read SS3 ref points:
save_df = list()
for(i in seq_along(ss_files)) {
  tmp = read.csv(file.path(ss_path, "RefPtsOM", ss_files[i])) %>% mutate(OM = 100*(i-1) + iter)
  save_df[[i]] = tmp %>% select(SSB_MSY, F_MSY, MSY, OM) %>% mutate(type = "SS3")
}
# Merge:
ss_refpts = bind_rows(save_df)

# Read FLBRP ref points:
folder_path <- "estimates"
csv_files <- list.files(path = folder_path, full.names = TRUE)
# Read and merge all CSV files
fl_refpts <- csv_files %>% lapply(read.csv) %>% bind_rows()
# Save ref pts:
write.csv(fl_refpts %>% filter(type == "FLBRP") %>% select(-type) %>% arrange(OM),
          file = "RefPts_FLBRP.csv", row.names = FALSE)

# Merge SS3 and FLBRP datasets:
# NA for Fmsy in SS3 due to units are different
merged_data = rbind(ss_refpts %>% mutate(F_MSY = NA), fl_refpts)

# Plot 1:
plot_data = merged_data %>% pivot_longer(cols = c("SSB_MSY", "F_MSY", "MSY"),
                                       names_to = "variable")

p1 = ggplot(data = plot_data, aes(x = type, y = value)) +
  geom_violin() +
  geom_jitter(width = 0.15,size = 2,alpha = 0.7) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free_y")
ggsave(filename = "compare_1.png", path = fig_path, plot = p1, 
       width = 170, height = 100, units = "mm", dpi = 300)

# Plot 2: compare FLBRP and Rfun estimates:
plot_data = merged_data %>% filter(type %in% c("FLBRP", "Rfun"))
plot_data = plot_data %>% pivot_wider(names_from = "type", values_from = c("SSB_MSY", "F_MSY", "MSY"))
plot_data = plot_data %>% mutate(SSB_diff = SSB_MSY_FLBRP - SSB_MSY_Rfun,
                                 F_diff = F_MSY_FLBRP - F_MSY_Rfun,
                                 MSY_diff = MSY_FLBRP - MSY_Rfun)
plot_data = plot_data %>% pivot_longer(cols = c("SSB_diff", "F_diff", "MSY_diff"),
                                       names_to = "variable")

p2 = ggplot(data = plot_data, aes(x = 1, y = value)) +
  geom_violin() +
  geom_jitter(width = 0.15,size = 2,alpha = 0.7) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free_y")
ggsave(filename = "compare_2.png", path = fig_path, plot = p2, 
       width = 170, height = 100, units = "mm", dpi = 300)

# Plot 3: compare FLBRP and SS3 estimates:
plot_data = merged_data %>% filter(type %in% c("FLBRP", "SS3"))
plot_data = plot_data %>% pivot_wider(names_from = "type", values_from = c("SSB_MSY", "F_MSY", "MSY"))
plot_data = plot_data %>% mutate(SSB_diff = SSB_MSY_FLBRP - SSB_MSY_SS3,
                                 F_diff = F_MSY_FLBRP - F_MSY_SS3,
                                 MSY_diff = MSY_FLBRP - MSY_SS3)
plot_data = plot_data %>% pivot_longer(cols = c("SSB_diff", "F_diff", "MSY_diff"),
                                       names_to = "variable")

p3 = ggplot(data = plot_data, aes(x = 1, y = value)) +
  geom_violin() +
  geom_jitter(width = 0.15,size = 2,alpha = 0.7) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free_y")
ggsave(filename = "compare_3.png", path = fig_path, plot = p3, 
       width = 170, height = 100, units = "mm", dpi = 300)

# Plot 4: compare FLAgur and SS3 estimates:
flagur = read.csv(file.path(ss_path, "RefPtsOM_FLR_biolsM_updateFLBRP_stock2020.csv"))
flagur = flagur %>% select(-c(X, scenario)) %>% rename(OM = iter) %>% mutate(type = "FL2020")

plot_data = rbind(merged_data %>% filter(type %in% c("SS3")),
                  flagur)
plot_data = plot_data %>% pivot_wider(names_from = "type", values_from = c("SSB_MSY", "F_MSY", "MSY"))
plot_data = plot_data %>% mutate(SSB_diff = SSB_MSY_FL2020 - SSB_MSY_SS3,
                                 F_diff = F_MSY_FL2020 - F_MSY_SS3,
                                 MSY_diff = MSY_FL2020 - MSY_SS3)
plot_data = plot_data %>% pivot_longer(cols = c("SSB_diff", "F_diff", "MSY_diff"),
                                       names_to = "variable")

p4 = ggplot(data = plot_data, aes(x = 1, y = value)) +
  geom_violin() +
  geom_jitter(width = 0.15,size = 2,alpha = 0.7) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free_y")
ggsave(filename = "compare_4.png", path = fig_path, plot = p4, 
       width = 170, height = 100, units = "mm", dpi = 300)
