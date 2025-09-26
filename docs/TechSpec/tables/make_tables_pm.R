rm(list = ls())
require(dplyr)
require(tidyr)
source('../../sharepoint_path.R') # leave it this way because it will be updated when running the tech doc
tbl_dir = 'tables'

# Read PM outputs
pm_df = readRDS(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics.rds'))

# Make table:
tbl_data = pm_df %>% group_by(Ftgt, Btgt) %>% summarise_at(vars(minB:maxTc), median)
saveRDS(tbl_data, file.path(tbl_dir, 'PM_summary.rds'))


# -------------------------------------------------------------------------
# For Robustness runs:

# Check if file exists
file_present = file.exists(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics_Robust.rds'))

if(file_present) {
  
  # Read PM outputs
  pm_df = readRDS(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics_Robust.rds'))
  
  # Make table:
  tbl_data = pm_df %>% group_by(Scen, Ftgt, Btgt) %>% summarise_at(vars(minB:maxTc), median)
  saveRDS(tbl_data, file.path(tbl_dir, 'PM_summary_robust.rds'))
  
}