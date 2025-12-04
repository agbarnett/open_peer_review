# 2_patch_author_experience.R
# patch in author experienced to newly created XML data
# November 2025
library(dplyr)

# get data with experience
load('data/2_plus_experience.RData')
experience = select(data, doi, author_papers)
cat(nrow(data), ' rows in experience\n', sep='')

# from 1_process.R
load('data/1_processed.RData')
cat(nrow(data), ' rows in data\n', sep='')
update = left_join(data, experience, by='doi')
cat(nrow(update), ' rows in update\n', sep='')
summary(update$author_papers)
summary(experience$author_papers)

# save (over-write)
data = update
save(data, censor.date, file = 'data/2_plus_experience.RData')

# check
dup_doi = group_by(data, doi) %>%
  summarise(n = n()) %>%
  filter(n>1) %>%
  pull(doi)
filter(data, doi %in% dup_doi)
