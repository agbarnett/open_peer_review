# 2_add_author_experience.R
# add the experience of authors from OpenAlex
# use last author and their paper numbers at submission
# September 2025
library(openalexR)
library(stringr)
library(dplyr)
source('R/open_alex.R') # main function to get open alex data
loop_size = 100 # number of DOIs to loop through

# get data, from 1_process.R
load("data/1_processed.RData")
max_loops = floor(nrow(data)/loop_size)

# big loop to process results in batches
experience = NULL
for (k in 1:max_loops){
  # get start and end
  lstart = 1 + ((k-1)*loop_size)
  lend = k*loop_size
  lend = min(lend, nrow(data)) 
  # call main function
  res = get_alex(dois = data$doi[lstart:lend], dates = data$received[lstart:lend])
  if(nrow(res) != loop_size){cat('Error, missing results for loop ', k, '\n', sep='')}
  experience = bind_rows(experience, res)
  # progress
  cat('Up to loop ', k, '.\r', sep='')
}
experience = unique(experience) # due to restarts

# merge experience back with data
data = left_join(data, experience, by = 'doi')

# save
save(data, censor.date, file='data/2_plus_experience.RData')
#save(experience, k , file='temporary.RData') # safety net for crashes

## redo missing author numbers - hacky! ##
data_complete = data
missing = filter(data_complete, is.na(author_papers)) %>% pull(doi)
load("data/1_processed.RData")
data_missing = filter(data, doi %in% missing)
max_loops = floor(nrow(data_missing)/loop_size)
# big loop to process results in batches
experience_missing = NULL
for (k in 1:max_loops){
  # get start and end
  lstart = 1 + ((k-1)*loop_size)
  lend = k*loop_size
  lend = min(lend, nrow(data_missing)) 
  # call main function
  res = get_alex(dois = data_missing$doi[lstart:lend], dates = data_missing$received[lstart:lend])
  if(nrow(res) != loop_size){cat('Error, missing results for loop ', k, '\n', sep='')}
  experience_missing = bind_rows(experience_missing, res)
  # progress
  cat('Up to loop ', k, '.\r', sep='')
}

## now create version of the data with fewer missing publication counts ##
# merge experience back with data
data_e = left_join(experience_missing, data, by = 'doi')
missing = pull(data_e, doi) # DOIs 
# previously processed data
load('data/2_plus_experience.RData')
previous = filter(data, !doi %in% missing)
# now update
data = bind_rows(previous, data_e)
# remove any counts over 2,000 (assume they are an error); see https://www.quora.com/Who-has-published-the-most-number-of-scientific-research-papers-till-now
data = mutate(data, 
              author_papers = ifelse(author_papers>2000, NA, author_papers))
# re-save (over-write)
save(data, censor.date, file='data/2_plus_experience.RData')
