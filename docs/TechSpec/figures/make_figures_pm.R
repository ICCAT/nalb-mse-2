rm(list = ls())
require(r4ss)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(fmsb)
require(scales)
source('../../sharepoint_path.R') # leave it this way because it will be updated when running the tech doc
fig_dir = 'figures'

# Read PM outputs
pm_df = readRDS(file.path(shrpoint_path, 'FLoutput/PerformanceMetrics/PerfMetrics.rds'))


# -------------------------------------------------------------------------
# Radar plot

# Select metrics to be plotted:
plot_data = pm_df %>% group_by(Ftgt, Btgt) %>% summarise_at(vars(minB:maxTc), median)
# Create expression vector B/Bmsy:
leg_labels1 = paste0(1:nrow(plot_data), ": ", plot_data$Btgt, "B")
leg_labels2 = paste0(plot_data$Ftgt, "F")
leg_labels = as.expression( mapply( function(x, y) bquote(.(x)["msy"]*"-"*.(y)["msy"]), leg_labels1, leg_labels2 ) )

plot_data = plot_data %>% ungroup %>% 
              # mutate(hcr = paste0(Btgt, 'Bmsy/', Ftgt, 'Fmsy'), .after = 'Btgt') %>%
              mutate(hcr = paste0(1:n()), .after = 'Btgt') %>%
              select(-c(Ftgt, Btgt))
metric_vec = colnames(plot_data)[-1] # remove hcr column

# -------------------------------------------------------------------------
# Plot radar plots:
png(filename = file.path(fig_dir, 'pm_radar.png'), 
    width = 150, height = 170, res = 300, units = 'mm')
layout(matrix(c(1:16, 17, 17, 17, 17), ncol=4, byrow=TRUE), heights=c(rep(1, times = 4), 0.6))
for(j in seq_along(metric_vec)) {
  this_var = metric_vec[j]
  subdat = matrix(plot_data %>% pull({{this_var}}), nrow = 1) 
  if(this_var %in% c("Clon", "Cmed", "Csht", "Csd")) {
    min_val = round(min(subdat)*0.95, digits = 0)
    max_val = round(max(subdat)*1.05, digits = 0)
    c_labels = round(seq(min_val, max_val, length.out = 3))
  }
  if(this_var %in% c("pGreen", "pRed", "pBlim", "pBmsy", "pShw", "pTX")) {
    subdat = subdat * 100 # in %
    min_val = 0
    max_val = 100
    c_labels = round(seq(min_val, max_val, length.out = 3))
  }
  if(this_var %in% c("Cc", "maxTc")) {
    subdat = subdat * 100 # in %
    min_val = round(min(subdat)*0.95, digits = 0)
    max_val = round(max(subdat)*1.05, digits = 0)
    c_labels = round(seq(min_val, max_val, length.out = 3))
  }
  if(this_var %in% c("minB", "meanB", "meanF")) {
    min_val = floor(min(subdat)*10)/10
    max_val = ceiling(max(subdat)*10)/10
    c_labels = seq(min_val, max_val, length.out = 3)
  }
  
  subdat = rbind(matrix(rep(max_val, times = ncol(subdat)), nrow = 1), # max
                 matrix(rep(min_val, times = ncol(subdat)), nrow = 1), # min
                 subdat)
  subdat = as.data.frame(subdat) 
  colnames(subdat) = plot_data$hcr
  
  # Plot:
  par(mar = c(0.5, 0, 1, 0))
  # par(mai=c(0, 0, 0.5, 0))
  radarchart( subdat, maxmin = TRUE, cglcol = "gray", seg = 2, axistype = 4,
              caxislabels = c_labels, 
              axislabcol="gray", cglwd=0.8,
              pfcol = alpha("gray", alpha = 0.5 ),
              vlcex = 0.9, title =  this_var)

}
# Empty:
par(mai=c(0,0,0,0))
plot.new()
# Add legend
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", ncol=5, legend=leg_labels, bty = "n")
dev.off()
