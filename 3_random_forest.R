# 3_random_forest.R
# look at random forests for classifying open peer review in selected countries
# September 2025
library(dplyr)
library(randomForest)
library(stringr)
TeachingDemos::char2seed("wycombe")

# get data, from 1_process.R
load("data/1_processed.RData")

# prepare the data
source('2_data_prepare.R')

# select one country
this_country = 'Ethiopia'
column_index = which(colnames(x) == paste('country_', this_country, sep=''))
row_index = x[,column_index] == 1
x_selected = x[row_index, -row_index] # drop country column as no longer meaningful (can keep co-author countries)
y_selected = y[row_index]
# run trees
trees = randomForest(x = x_selected, y = y_selected)
imp = data.frame(importance(trees)) %>%
  tibble::rownames_to_column() %>%
  arrange(desc(IncNodePurity)) %>%
  head()

# try tree instead?
library(rpart)
library(rpart.plot)
my.control = rpart.control()
my.control$cp = 0.001
tree = rpart(y_selected ~ x_selected, method='class', control = my.control)
plotcp(tree)
pruned = prune(tree, cp = 0.011)
rpart.plot(pruned, type=1, extra=101)
