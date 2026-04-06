# 99_correlations.R
# correlation of subjects
library(dplyr)
library(stringr)

# get the data from 3_add_author_experience.R
load('data/3_plus_experience.RData')

# prepare the data
source('4_data_prepare.R')

# country correlations
bigr = cor(country_mat) # takes a while
longr <- data.frame(
  expand.grid(lapply(dim(bigr), seq_len)),
  corr = as.vector(bigr)
) %>% 
  filter(Var1 < Var2) %>% # lower triangle
  arrange(desc(corr))
# country frame
cframe = data.frame(number = 1:length(countries_to_use), country = countries_to_use)
longr = left_join(longr, cframe, by=c('Var1'='number')) %>%
  left_join(cframe, by=c('Var2'='number')) %>%
  select('country.x','country.y','corr')

# subject correlations
scorr = function(inmat, vlabels){
  if(length(vlabels)!=ncol(inmat)){stop('Mismatch')}
  bigc = cor(inmat) # takes a while
  longc <- data.frame(
    expand.grid(lapply(dim(bigc), seq_len)),
    corr = as.vector(bigc)
  ) %>% 
    filter(Var1 < Var2) %>%
    arrange(desc(corr))
  # subject frame
  sframe = data.frame(number = 1:length(vlabels), subject = vlabels)
  longc = left_join(longc, sframe, by=c('Var1'='number')) %>%
    left_join(sframe, by=c('Var2'='number')) %>%
    select('subject.x','subject.y','corr')
  # show very high correlations
  very_high = filter(longc, corr >= 0.97)# threshold found by trial and error
  return(very_high)
}
very_high = scorr(subject_mat, vlabels = subjects_to_use)
text = c(very_high$subject.x, very_high$subject.y)
tab = table(text)
tab[order(tab)]
# get list of subjects to remove
source('99_subjects_to_remove.R')
#                          
# test numbers
index = subjects_to_use==to_remove[2]
sum(subject_mat[, index])
# redo correlation matrix to confirm no remaining very high correlations
index = subjects_to_use %in% to_remove
if(sum(index)!=length(to_remove)){cat('error, typo.\n')}
# remove variables above
subject_mat_dash = subject_mat[, !index]
subjects_to_use_dash = subjects_to_use[!subjects_to_use %in% to_remove]
remaining_high = scorr(subject_mat_dash, vlabels = subjects_to_use_dash)

## notes
# big enough gap between 'Nutrition' and 'Obesity' so kept both
# big enough gap between Geomorphology and Topography
# enough difference between Animal cells & Cellular types
# ~250 gap between children and families, so kept