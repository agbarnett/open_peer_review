# 3_add_author_experience_filler.R

## run on HPC due to time, see folder open_review_plos ##

# add the experience of authors from OpenAlex
# use last author and their paper numbers at submission date
# filler for missing authors from first run
# Feb 2026
library(openalexR)
library(stringr)
library(dplyr)
source('R/open_alex_experience.R') # main function to get open alex data
source('0_my_openalex_get_do_not_share.R') # my key
loop_size = 500 # number of DOIs to loop through

# get data, from 2_process.R
load("data/2_processed.RData")
max_loops = floor(nrow(data)/loop_size)
# for running in batches
start = 1
max_loops = 1

# big loop to process results in batches
experience = NULL
for (k in start:max_loops){
  # get start and end
  lstart = 1 + ((k-1)*loop_size)
  lend = k*loop_size
  lend = min(lend, nrow(data)) 
  # call main function
  res = get_alex_experience(dois = data$doi[lstart:lend], dates = data$received[lstart:lend], oa_key = my_open_alex_key)
  if(nrow(res) != loop_size){cat('Error, missing results for loop ', k, '\n', sep='')}
  experience = bind_rows(experience, res)
  # progress
  cat('Up to loop ', k, '.\r', sep='')
}
experience = unique(experience) # due to restarts

## merge experience back with data
# use a smaller data set for merge
rstart = 1 + ((start-1)*loop_size)
rend = max_loops*loop_size
data = data[rstart:rend,]
N = nrow(data)
# check size
if(nrow(experience)!=N){cat('Error in row numbers, data = ', N, ', but author data = ', nrow(experience), '\n',sep='')}
# merge
data = left_join(data, experience, by = 'doi')

# save partial file
outfile = paste('data/3_plus_experience_', max_loops, '.RData', sep='')
save(data, censor.date, file = outfile)
