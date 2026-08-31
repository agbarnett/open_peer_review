# 3_combine_experience_data.R
# combine the experience files created by the batch processes
# March 2026
library(dplyr)
library(stringr)

# find the files
d = dir('data', pattern='plus_experience_')
cat('There are ', length(d), ' files.\n', sep='')
# should be 235
# what numbers are missing
numbers = as.numeric(str_remove_all(d, '3_plus_experience_|\\.RData'))
all = 1:235
setdiff(all, numbers)

# loop through files
setwd('data')
edata = NULL
for (file in d){
  load(file)
  
  # check numbers
  tab = group_by(experience, atype) %>%
    tally() 
  check1 = filter(tab, atype == 'last') %>%
    filter(n < 450) # expect most to be last authors
  check2 = filter(tab, atype == 'No papers') %>%
    filter(n > 20) # should not be lots of missing papers
  if(nrow(check1)>0 | nrow(check2)>0){
    cat('Check author types for ', file,'\n', sep='')
    print(tab)
  }
  
  # check for missing
  n_missing = sum(is.na(experience$author_papers))
  if(n_missing > 15){cat('Check missing for ', file,'\n', sep='')}

    # check overall numbers
  if(nrow(experience)!=500){cat('Check total for ', file,'\n', sep='')}
  # concatenate
  edata = bind_rows(edata, experience)
}
setwd('..')

# overall table
group_by(edata, atype) %>%
  tally()
# check distribution of paper numbers
hist(log2(edata$author_papers+1))

# checked missing categories (e.g., no authors...), but no missing can be set to zero

# combine with prior data and save
load('data/2_processed.RData')
data = left_join(data, edata, by = 'doi') 
save(data, file = 'data/3_plus_experience.RData')
