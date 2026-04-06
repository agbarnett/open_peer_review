# 5_plot_stability_continuous.R
# plot the continuous estimates from the stability selection
# January 2026
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
g.theme = theme_bw()+ theme(panel.grid.minor = element_blank())
# folder for figures:
loc = 'C:/Users/barnetta/OneDrive - Queensland University of Technology/talks/AIMOS5/figures/'

# load the results
load('results/4_ests_stability.RData') # from 4_stability_selection.R

# need to run for mean and SD used for standardisation
load('data/3_plus_experience.RData')
source('4_data_prepare.R')

## plot continuous variables 
# a) date published
source('5_plot_date.R')
# export
ggsave('figures/5_date_effect.jpg', rplot, width = 5.2, height=4.2, units='in', dpi=500)
# b) ORCID proportion
source('5_plot_orcid.R')
# export
ggsave('figures/5_orcid_effect.jpg', oplot, width = 5.2, height=4.2, units='in', dpi=500)

## combined continuous plot; add panel labels
rplot = rplot + ggtitle('(A) Date published')
oplot = oplot + ggtitle('(B) Proportion of authors with an ORCID') + ylab(NULL)
jpeg('figures/5_continuous.jpg', oplot, width = 8.2, height=4.2, units='in', res=500)
grid.arrange(rplot, oplot, ncol=2)
dev.off()
