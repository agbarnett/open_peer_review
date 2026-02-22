# 99_check_funders.R
# check that funder data and countries match (e.g., biggest funder of research in China is from China)
# Feb 2026
library(dplyr)

# analysis data from from 3_add_author_experience.R
load('data/3_plus_experience.RData')

# to get X matrix and Y
source('4_data_prepare.R')

# bind country to funder
countries_to_check = c('Australia','Belgium','China','France','India','United States of America','United Kingdom')
finds = NULL
for (c in countries_to_check){
  search_string = paste('country_', c, sep='')
  index  = which(country_mat_names == search_string)
  index_row = country_mat[,index] == 1
  # get funders that mention this country and calculate frequencies
  funder_mention = funder_mat[index_row,] %>%
    colSums()
  find_max = which(funder_mention == max(funder_mention))
  frame = data.frame(country = c, funder = funder_mat_names[find_max])
  finds = bind_rows(finds, frame)
}

# add names
load('data/1_funder_info.RData')
findsx = mutate(finds, funder = str_remove_all(funder, 'funder_')) %>%
  rename('funder_number' = 'funder') %>%
  left_join(funder_text, by='funder_number')
