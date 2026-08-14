# 5_plot_survival_retractions.R
# plot the results from the survival models
# August 2026
library(survival) # for proportional hazards test
library(xtable)
library(dplyr)
library(stringr)
library(survminer) # for Kaplan-Meier
library(ggplot2)
library(ggplotify) # to convert base plot to ggplot
library(patchwork) # to combine plots
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
source('R/dark_theme.R') # for dark slide theme for AIMOS presentation
source('R/plot_retractions.R') # code to plot Kaplan-Meier
cbPalette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#999999"
) # colours
# folder for conference figures:
loc = 'C:/Users/barnetta/OneDrive - Queensland University of Technology/talks/AIMOS5/figures/'

# get data and results
load('data/3_plus_experience.RData') # need the original data
source('4_data_prepare_retractions.R') # prepare the data
load('results/4_retraction_models.RData') # results from HPC, 4_survival_retractions.R

## plot inverse survival from Kaplan-Meier
# i) retractions and expressions of concern
ret_plot = plot_retractions(
  inmodel = smodel,
  indata = data,
  xlab = "Years to retraction or\nexpression of concern"
)
# ii) retractions only
ret_only_plot = plot_retractions(
  inmodel = smodel_ret_only,
  indata = data,
  xlab = "Years to retraction"
)
# iii) concerns only
eoc_only_plot = plot_retractions(
  inmodel = smodel_eoc_only,
  indata = data,
  vjust = c(3, -2), # numbers appearing on top of one another
  xlab = "Years to expression of concern"
)
# export plot
ggsave(
  'figures/5_survival_retraction.jpg',
  ret_plot,
  width = 4.5,
  height = 4.2,
  units = 'in',
  dpi = 500
)

## smaller combined plot of retractions/expressions of concern
ret_only_plot <- ret_only_plot +
  ggtitle('Retractions')
eoc_only_plot <- eoc_only_plot +
  ggtitle('Expressions of concern') +
  theme(legend.position = 'none') +
  ylab(NULL)
combined_plot = ret_only_plot + eoc_only_plot
# export combined plot
ggsave(
  'figures/5_survival_retraction_combined.jpg',
  combined_plot,
  width = 8,
  height = 4.2,
  units = 'in',
  dpi = 500
)


# check VIF from full model #
#vif = car::vif(cmodel_adjusted) # has not worked as there's no intercept

# check proportional hazards assumption, failed!
test.ph <- cox.zph(cmodel)
test.ph
plot(test.ph) # plot Schoenfeld residuals; shows slightly increasing effect over time
# export plot
jpeg(
  filename = "figures/5_Schoenfeld_residuals.jpg",
  width = 4,
  height = 4,
  units = 'in',
  res = 300
)
par(mar = c(4, 4, 0.1, 0.1)) #bottom, left, top, right
plot(test.ph)
dev.off()

## plot time-varying cox model for retractions only
# text labels
text1 = data.frame(
  xtime = 0.1,
  mean = 1.06,
  lower = NA,
  upper = NA,
  label = 'Open review increases hazard'
)
text2 = data.frame(
  xtime = 0.1,
  mean = 0.94,
  lower = NA,
  upper = NA,
  label = 'Open review decreases hazard'
)

#
col = 'yellow4'
iplot = ggplot(
  data = pred_time,
  aes(x = xtime, y = exp(mean), ymin = exp(lower), ymax = exp(upper))
) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_line(colour = col, lwd = 1.05) +
  geom_ribbon(alpha = 0.5, fill = col) +
  geom_text(data = text1, aes(x = xtime, y = mean, label = label), adj = 0) +
  geom_text(data = text2, aes(x = xtime, y = mean, label = label), adj = 0) +
  g.theme +
  scale_y_log10(breaks = c(0.3, 0.5, 1, 1.3)) +
  scale_x_continuous(expand = 0.01) +
  xlab('Years to retraction') +
  ylab('Hazard ratio for retration')
iplot
# export
ggsave(
  'figures/5_time_dependent_hazard.jpg',
  iplot,
  width = 4.5,
  height = 4.2,
  units = 'in',
  dpi = 500
)

## combined Schoenfeld and time-dependent
# scale to fit in box
p_base_converted <- as.ggplot(
  ~ plot(test.ph, ylab = 'Log hazard ratio', xlab = 'Years to retraction'),
  scale = 0.95,
  vjust = 0.1
)
p_base_converted <- p_base_converted + ggtitle('Schoenfeld residuals')
iplot <- iplot + ggtitle('Time-dependent effect')
# export plot
jpeg(
  filename = "figures/5_Schoenfeld_residuals_plus.jpg",
  width = 8,
  height = 4,
  units = 'in',
  res = 400
)
p_base_converted + iplot
dev.off()
