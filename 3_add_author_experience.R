# 3_add_author_experience.R
# run on HPC due to time
# add the experience of authors from OpenAlex; using paid API
# use last author and their paper numbers at submission date
# March 2026
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
max_loops = 234

# big loop to process results in batches
for (k in start:max_loops){

  # check if the file already exists (avoid repeating data collection)
  dir_data = dir('data')
  this_pattern = paste('experience_', k, '\\.RData', sep='')
  check = any(str_detect(dir_data, pattern = this_pattern))
  if(check==TRUE){next}

  # get start and end
  lstart = 1 + ((k-1)*loop_size)
  lend = k*loop_size
  lend = min(lend, nrow(data)) 
  # call main function
  experience = get_alex_experience(dois = data$doi[lstart:lend], dates = data$received[lstart:lend])
  if(nrow(experience) != loop_size){cat('Error, missing results for loop ', k, '\n', sep='')}
  
# save experience data, one file at a time
  outfile = paste('data/3_plus_experience_', k, '.RData', sep='')
  save(experience, file = outfile)
  experience = NULL

}
