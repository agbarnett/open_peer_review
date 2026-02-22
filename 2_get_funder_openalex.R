# 2_get_funder_openalex.R
# use openalex to get funder and topics
# not working well for funders
# Feb 2026
library(openalexR)
library(dplyr)
library(stringr)

# get the data; from 1_process.R
load('data/1_processed.RData')
data = mutate(data, r = runif(n())) %>% # randomly order to expose errors better
  arrange(r)

# variables to extract from OpenAlex
vars_to_use = c('doi','funders','awards','topics') # see https://docs.openalex.org/api-entities/works/work-object

# loop
end = 1500
empty_funders = empty_topics = 0
funders = topics = NULL
for (k in 602:end){ # one paper at a time
  # query openalex
  this_doi = data$doi[k]
  res = oa_fetch(entity = 'works',
                 options = list(select = vars_to_use),
                 mailto = 'a.barnett@qut.edu.au', 
                 abstract = FALSE,
                 doi = this_doi)
  # pause to avoid rate limiters
  Sys.sleep(0.44)
  # extract topics
  if(is.null(res$topics) == TRUE | nrow(res$topics[[1]]) == 0){
    empty_topics = empty_topics + 1
  }
  if(is.null(res$topics) == FALSE & nrow(res$topics[[1]]) > 0){
    this_topics = filter(res$topics[[1]], type =='topic') %>%
      mutate(doi = this_doi,
             topic = str_remove(id, 'https...openalex.org.')) %>% # reduce to number for matrix
      select(-i, -score, -type, -id) # drop variables
    # concatenate
    topics = bind_rows(topics, this_topics)
  }
  # extract funders
  if(is.null(res$funders) == TRUE){
    empty_funders = empty_funders+1
    next
  }
  this_funder = mutate(res$funders, doi = this_doi)
  funders = bind_rows(funders, this_funder)
}

# make unique list of topics (linking numbers to names)
unique_topics = select(topics, topic, display_name) %>%
  unique()
topics = select(topics, -display_name) # now remove from topic frame to save room
