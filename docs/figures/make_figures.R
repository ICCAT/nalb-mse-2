require(r4ss)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(caret)
require(tidyr)
source('sharepoint_path.R')
fig_dir = 'docs/TechSpec/figures'

# Read SS3 model
mymod = SS_output(file.path(shrpoint_path, "Assessment/Assessment_2023/ALB_SS3_FinalVersionRecDev2018/v28_forecast3_relf_v5_Fmsy08_2018_v3"))

# Polygon data for kobe plot
datapoly = data.frame(id = rep(1:4, each = 5), 
                      x = c(1,1,20,20,1,-1,-1,1,1,-1,-1,-1,1,1,-1,1,1,20,20,1),
                      y = c(1,-1,-1,1,1,1,-1,-1,1,1,20,1,1,20,20,20,1,1,20,20))

# Data in SS3 -------------------------------------------------------------
SSplotData(replist = mymod, plotdir = fig_dir, print = TRUE, subplots = 2, cex = 0.5, 
           punits = 'mm', pwidth = 150, pheight = 180)

png(filename = file.path(fig_dir, 'biology.png'), width = 170, height = 60, res = 300, units = "mm")
par(mfrow = c(1,3))
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = mymod, subplots = c(1), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = mymod, subplots = c(6), mainTitle = FALSE)
par(mar = c(4.1,4,0.5,0.5))
SSplotBiology(replist = mymod, subplots = c(21), mainTitle = FALSE)
dev.off()


# -------------------------------------------------------------------------
# Plot indices:
plot_data = mymod$cpue
fleet_order = plot_data %>% group_by(Fleet) %>% summarise(Fleet_name = unique(Fleet_name)) %>% arrange(Fleet)
plot_data$Fleet_name = factor(plot_data$Fleet_name, levels = fleet_order$Fleet_name)

p1 = ggplot(data = plot_data, aes(x = Yr, y = Obs)) +
  geom_line() + theme_bw() + ylab("CPUE") + xlab(NULL) +
  theme(axis.text = element_text(size = 7.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~ Fleet_name)
ggsave(filename = file.path(fig_dir, 'cpue_obs.png'), plot = p1, 
       width = 150, height = 110, units = "mm", dpi = 300)

# -------------------------------------------------------------------------
# Figure M and sigmaR values per OM:

plot_data = readRDS('docs/TechSpec/data/M-sigmaR_values.rds')
plot_data = pivot_longer(plot_data, cols = c('M', 'sigmaR'), names_to = 'parameter', values_to = 'value')
  
p1 = ggplot(plot_data, aes(x=value))+
  geom_histogram(color="black", fill="gray80") +
  theme_classic() +
  xlab('Parameter value') + ylab('Frequency') +
  facet_wrap(~ parameter, scales = 'free_x')
ggsave(filename = file.path(fig_dir, 'par_sample.png'), plot = p1, 
       width = 150, height = 75, units = "mm", dpi = 300)

# -------------------------------------------------------------------------
# Figure HCR example:
p1 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_segment(x = 0, y = 0.1, xend = 0.4, yend = 0.1, lwd = 1, color = 'black') +
  geom_segment(x = 0.4, y = 0.1, xend = 0.8, yend = 0.8, lwd = 1, color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 3, yend = 0.8, lwd = 1, color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 0.8, yend = 0, linetype = 'dashed', color = 'black') +
  geom_segment(x = 0.8, y = 0.8, xend = 0, yend = 0.8, linetype = 'dashed', color = 'black') +
  theme_bw() +
  ylab(NULL) + xlab(NULL) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0.4, 0.8, 1), labels = c(expression(B[lim]), expression(B[thr]), expression(B[msy]))) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0.1, 0.8, 1), labels = c(expression(F[min]), expression(F[tgt]), expression(F[msy]))) +
  theme(legend.direction="horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900'))
ggsave(filename = file.path(fig_dir, 'hcr_example.png'), plot = p1, 
       width = 80, height = 70, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# Figure HCR by case:
mydat = as.data.frame(expand.grid(ffmsy = c(0.8, 0.9, 1, 1.1, 1.2), bbmsy = c(0.8, 0.9, 1, 1.1, 1.2)))
mydat = mydat %>% mutate(bbmsy_t = factor(bbmsy, levels = c(0.8, 0.9, 1, 1.1, 1.2), 
                                        labels = c(expression("0.8"*B[msy]),
                                                   expression("0.9"*B[msy]),
                                                   expression(B[msy]),
                                                   expression("1.1"*B[msy]),
                                                   expression("1.2"*B[msy]))),
                         ffmsy_t = factor(ffmsy, levels = c(0.8, 0.9, 1, 1.1, 1.2), 
                                        labels = c(expression("0.8"*F[msy]),
                                                   expression("0.9"*F[msy]),
                                                   expression(F[msy]),
                                                   expression("1.1"*F[msy]),
                                                   expression("1.2"*F[msy]))))

p2 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_segment(x = 0, y = 0.1, xend = 0.4, yend = 0.1, color = 'black') +
  geom_segment(data = mydat, aes(x = 0.4, y = 0.1, xend = bbmsy, yend = ffmsy), color = 'black') +
  geom_segment(data = mydat, aes(x = bbmsy, y = ffmsy, xend = 3, yend = ffmsy), color = 'black') +
  theme_bw() +
  ylab(NULL) + xlab(NULL) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0.4, 1), labels = c(expression(B[lim]), expression(B[msy]))) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0.1, 1), labels = c(expression(F[min]), expression(F[msy]))) +
  theme(legend.direction="horizontal",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  facet_grid(ffmsy_t ~ bbmsy_t, labeller = label_parsed )
ggsave(filename = file.path(fig_dir, 'hcr_combs.png'), plot = p2, 
       width = 170, height = 170, units = "mm", dpi = 300)


# -------------------------------------------------------------------------
# Figure for performance metrics example:

# Simulate time series:
n_ts = 30 # number of years to simulate
x = seq(0, 10, length.out = 100)
y = 2 * sin(3 * pi * 0.5 * x + pi / 4) + 1.5 * sin(2 * pi * 0.3 * x + pi / 6) # For SSB
plot(x[1:n_ts], y[1:n_ts], type = 'b')
y1 = -2 * sin(3 * pi * 0.5 * x + pi / 4) + 1.5 * sin(2 * pi * 0.5 * x + pi / 6) # For SSB
# plot(x[1:n_ts], y1[1:n_ts], type = 'b')
# plot(y[1:n_ts], y1[1:n_ts], type = 'b')
# abline(h = 1, v = -1)

# Define ref points:
Blim = 0.4
Bmsy = 1
Fmsy = 1
# Create a data frame:
exvec = data.frame(Bio_all = y[1:n_ts], Fval = y1[1:n_ts])
# Scale between min max:
df = preProcess(exvec, method = 'range', rangeBounds = c(0.25, 1.75))
exvec = predict(df, as.data.frame(exvec))
# Create relevant columns:
exvec = exvec %>% mutate(Yr_sim = 1:n_ts, 
                         red = if_else(Bio_all < Bmsy & Fval > Fmsy, '0', '1'),
                         green = if_else(Bio_all >= Bmsy & Fval <= Fmsy, '0', '1'))
exvec = exvec %>% mutate(pblim = if_else(Bio_all < Blim, '0', '1'))
exvec = exvec %>% mutate(pbmsy = if_else(Bio_all < Bmsy & Bio_all > Blim, '0', '1'))
# Define x axis label:
x_lab = 'Simulation years'

# Status
p1 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point() +
  geom_point(data = exvec %>% filter(min(Bio_all) == Bio_all), 
             aes(x = Yr_sim, y = Bio_all), color = 'blue', size = 2) +
  theme_bw() + ylab(expression(B/B[msy])) + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "minB", 
           size = 3, color = 'blue')

p2 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = mean(exvec$Bio_all), color = 'blue') +
  theme_bw() + ylab(expression(B/B[msy])) + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "meanB", 
           size = 3, color = 'blue')

p3 = ggplot(data = exvec, aes(x = Yr_sim, y = Fval)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = mean(exvec$Fval), color = 'blue') +
  theme_bw() + ylab(expression(F/F[msy])) + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "meanF", 
           size = 3, color = 'blue')

p4 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_path(data = exvec, aes(x = Bio_all, y = Fval), color = 'gray60') +
  geom_point(data = exvec, aes(x = Bio_all, y = Fval, color = green)) +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() +
  ylab(expression("F/"*F[msy])) + xlab(expression("B/"*B[msy])) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) +
  theme(legend.position="none",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  annotate("text", x = 1, y = 1.9, label = "pGreen",  size = 3, color = 'blue')

p5 = ggplot(data = datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = factor(id), group = factor(id)), alpha = 0.45) +
  geom_path(data = exvec, aes(x = Bio_all, y = Fval), color = 'gray60') +
  geom_point(data = exvec, aes(x = Bio_all, y = Fval, color = red)) +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() +
  ylab(expression("F/"*F[msy])) + xlab(expression("B/"*B[msy])) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) +
  theme(legend.position="none",
        legend.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        strip.background = element_blank()) +
  guides(fill = 'none') +
  scale_fill_manual(values = c('#8cff66', '#ffff00', '#ff3300', '#ff9900')) +
  annotate("text", x = 1, y = 1.9, label = "pRed",  size = 3, color = 'blue')

# Merge status plots:
merged_plot = grid.arrange(p1, p2, p3, p4, p5, ncol = 3)
ggsave(filename = file.path(fig_dir, 'status_ex.png'), plot = merged_plot, 
       width = 170, height = 120, units = "mm", dpi = 300)

# Safety:
p6 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point(aes(color = pblim)) +
  geom_hline(yintercept = Blim, linetype = 'dashed', color = 'gray60') +
  scale_color_manual(values = c('0' = 'gray40', '1' = 'blue')) +
  theme_bw() + ylab('Spawning biomass (t)') + xlab(x_lab) +
  scale_y_continuous(breaks = Blim, labels = expression(B[lim])) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "pBlim", 
           size = 3, color = 'blue')

p7 = ggplot(data = exvec, aes(x = Yr_sim, y = Bio_all)) +
  geom_line() + geom_point(aes(color = pbmsy)) +
  geom_hline(yintercept = Blim, linetype = 'dashed', color = 'gray60') +
  geom_hline(yintercept = Bmsy, linetype = 'dashed', color = 'gray60') +
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() + ylab('Spawning biomass (t)') + xlab(x_lab) +
  scale_y_continuous(breaks = c(Blim, Bmsy), labels = c(expression(B[lim]), expression(B[msy]))) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 1.8, label = "pBmsy", 
           size = 3, color = 'blue')

# Merge safety plots:
merged_plot = grid.arrange(p6, p7, ncol = 2)
ggsave(filename = file.path(fig_dir, 'safety_ex.png'), plot = merged_plot, 
       width = 140, height = 60, units = "mm", dpi = 300)


# Yield:
# add noise to catch:
tac_period = 3
set.seed(123)
exvec = exvec %>% mutate(catch = rep(rnorm(10, mean = 2), each = tac_period))
p8 = ggplot(data = exvec, aes(x = Yr_sim, y = catch)) +
  geom_line() + geom_point() +
  geom_segment(x = 1, xend = 3, y = mean(exvec$catch[1:3]), yend = mean(exvec$catch[1:3]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "Csht", 
           size = 3, color = 'blue', parse = TRUE)

p9 = ggplot(data = exvec, aes(x = Yr_sim, y = catch)) +
  geom_line() + geom_point() +
  geom_segment(x = 5, xend = 10, y = mean(exvec$catch[5:10]), yend = mean(exvec$catch[5:10]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "Cmed", 
           size = 3, color = 'blue', parse = TRUE)

p10 = ggplot(data = exvec, aes(x = Yr_sim, y = catch)) +
  geom_line() + geom_point() +
  geom_segment(x = 15, xend = 25, y = mean(exvec$catch[15:25]), yend = mean(exvec$catch[15:25]), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "Clon", 
           size = 3, color = 'blue', parse = TRUE)

# Merge yield plots:
merged_plot = grid.arrange(p8, p9, p10, ncol = 3)
ggsave(filename = file.path(fig_dir, 'yield_ex.png'), plot = merged_plot, 
       width = 170, height = 60, units = "mm", dpi = 300)


# Stability
plot_df = exvec %>% select(Yr_sim, catch)
plot_df$Yr_sim_end = NA
plot_df$catch_end = NA
plot_df$Yr_sim_end[1:(nrow(plot_df)-1)] = plot_df$Yr_sim[2:nrow(plot_df)]
plot_df$catch_end[1:(nrow(plot_df)-1)] = plot_df$catch[2:nrow(plot_df)]
plot_df2 = plot_df %>% na.omit

p11 = ggplot(data = plot_df, aes(x = Yr_sim, y = catch)) +
  geom_point() +
  geom_segment(data = plot_df2, aes(x = Yr_sim, xend = Yr_sim_end, y = catch, yend = catch_end), 
               arrow = arrow(length = unit(0.15, "cm")), color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "Cc", 
           size = 3, color = 'blue', parse = T)

p12 = ggplot(data = plot_df, aes(x = Yr_sim, y = catch)) +
  geom_point() + geom_line() +
  geom_segment(data = plot_df, aes(x = Yr_sim, xend = Yr_sim, y = mean(catch), yend = catch), 
               color = 'blue') +
  theme_bw() + ylab("Catch (t)") + xlab(x_lab) +
  geom_hline(yintercept = mean(plot_df$catch), color = 'black') +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "Csd", 
           size = 3, color = 'blue', parse = TRUE)

plot_df3 = plot_df
plot_df3$catch[c(22:24)] = 0
plot_df3 = plot_df3 %>% mutate(tac = if_else(catch == 0, '0', '1'))
p13 = ggplot(data = plot_df3, aes(x = Yr_sim, y = catch)) +
  geom_point(aes(color = tac)) + 
  scale_color_manual(values = c('0' = 'blue', '1' = 'gray40')) +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_y_continuous(breaks = 0) +
  theme(legend.position = 'none',
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "pShw", 
           size = 3, color = 'blue')

plot_df4 = exvec %>% select(Yr_sim, catch)
plot_df4 = plot_df4[seq(from = 2, by = tac_period, length.out = nrow(plot_df4)/tac_period), ]
plot_df4$Yr_sim_end = NA
plot_df4$catch_end = NA
plot_df4$Yr_sim_end[1:(nrow(plot_df4)-1)] = plot_df4$Yr_sim[2:nrow(plot_df4)]
plot_df4$catch_end[1:(nrow(plot_df4)-1)] = plot_df4$catch[2:nrow(plot_df4)]
plot_df4 = plot_df4 %>% na.omit
plot_df4$diff = abs(plot_df4$catch_end - plot_df4$catch)
plot_df4 = plot_df4 %>% mutate(diff_type = if_else(diff > 1.3, '1', '0'))
p14 = ggplot(data = exvec, aes(x = Yr_sim, y = catch)) +
  geom_point() + 
  geom_segment(data = plot_df4, 
               aes(x = Yr_sim, xend = Yr_sim_end, 
                   y = catch, yend = catch_end,
                   color = diff_type), 
               arrow = arrow(length = unit(0.15, "cm"))) +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_color_manual(values = c('0' = 'gray40', '1' = 'blue')) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position = "none") +
  annotate("text", x = 15, y = 4, label = "pTX", 
           size = 3, color = 'blue')

p15 = ggplot(data = exvec, aes(x = Yr_sim, y = catch)) +
  geom_point() + 
  geom_segment(data = plot_df4 %>% filter(which.max(diff) == row_number()), 
               aes(x = Yr_sim, xend = Yr_sim_end, y = catch, yend = catch_end), 
               arrow = arrow(length = unit(0.15, "cm")), color = 'blue') +
  theme_bw() + ylab("TAC (t)") + xlab(x_lab) +
  scale_x_continuous(breaks = seq(10, 30, 10)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  annotate("text", x = 15, y = 4, label = "maxTc", 
           size = 3, color = 'blue', parse = T)

# Merge stability plots:
merged_plot = grid.arrange(p11, p12, p13, p14, p15, ncol = 3)
ggsave(filename = file.path(fig_dir, 'stability_ex.png'), plot = merged_plot, 
       width = 170, height = 120, units = "mm", dpi = 300)
