# 3_stability_selection.R
# use stability selection to find variables associated with open peer review
# November 2025
library(dplyr)
library(lars)
library(stabs) # for stability selection
library(broom)
library(stringr)
TeachingDemos::char2seed("blackpool")

# get the data from 2_add_author_experience.R
load('data/2_plus_experience.RData')

# prepare the data
source('3_data_prepare.R')

# bootstrap combined with lasso (takes a while)
stab.lasso <- stabsel(x = x, 
                     y = y, # binary outcome
                     fitfun = lars.lasso, 
                     cutoff = 0.75, # probability cut-off for selecting variables
                     PFER = 1) # per-family error rate [0,1]; expected number of falsely selected variables

# refit model with selected variables (no shrinkage)
x_selected = x[,as.numeric(stab.lasso$selected)]
small_model = glm(y ~ x_selected)
ests = tidy(small_model, conf.int = TRUE)

# selected names
x_selected_names = str_remove(ests$term, '^x_selected')
x_selected_names = x_selected_names[x_selected_names!='(Intercept)']

# add numbers for categorical variables (used in plot)
categorical = x_selected_names[x_selected_names != 'published']
ests = mutate(ests, n = nrow(x)) # start with entire sample (works for continuous variables)
for (this_name in categorical){
  which_col = which(str_detect(colnames(x), pattern = paste(this_name,'$',sep=''))) # because of Health_care_...
  if(length(which_col)!=1)(cat('Error finding column for', this_name, '\n', sep=''))
  this_n = sum(x[,which_col]) # sum entire column
  e_index = which(str_detect(ests$term, pattern = this_name)) # find location in data frame
  ests$n[e_index] = this_n
}

# save
save(ests, stab.lasso, small_model, x_selected_names, file = 'results/3_ests_stability.RData')

