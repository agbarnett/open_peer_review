# 2_stability_selection.R
# use stability selection to find variables associated with open peer review
# September 2025
library(dplyr)
library(stabs) # for stability selection
library(broom)
library(stringr)
TeachingDemos::char2seed("blackpool")

# get data, from 1_process.R
load("data/1_processed.RData")

# prepare the data
source('2_data_prepare.R')

# bootstrap combined with lasso (takes a while)
stab.lasso <- stabsel(x = x, 
                     y = y,
                     fitfun = lars.lasso, 
                     cutoff = 0.75, # probability cut-off for selecting variables
                     PFER = 1) # per-family error rate [0,1]; expected number of falsely selected variables

# refit model with selected variables
x_selected = x[,as.numeric(stab.lasso$selected)]
small_model = glm(y ~ x_selected)
ests = tidy(small_model, conf.int = TRUE)

# save
save(ests, stab.lasso, small_model, file = 'results/2_ests_stability.RData')

