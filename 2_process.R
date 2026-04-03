# 2_process.R
# process the data;
# - add the authors' countries from their addresses
# - process email domain
# - add time between submission and acceptance
# - add retractions
# - add citations
# Feb 2026
library(readxl)
library(rvest) # for read_html
library(stringr)
library(dplyr)
library(openalexR) # for citations
library(tidyr) # for separate
library(janitor) # for clean_names
source('0_my_openalex_get_do_not_share.R') # my key
source('R/add_country.R') # for countries
source('R/add_us_state.R') # for countries
source('R/extract_email_domain.R') # for emails
source('R/countries_and_states.R') # data on countries and states

# from 0_read_xml_data.R
load('data/0_unprocessed.RData')
N = nrow(data)

## countries and email domain
# make variables for authors' countries (did not add editor because its only the institution)
# simplify email domain
data$country = data$edomain = ''
for (k in 1:N){ # big loop, takes a while
  data$country[k] = list(add_country(countries, states, text = data$aff[k][[1]])) # authors' countries
  data$edomain[k] = list(extract_email_domain(data$domain[k][[1]])) # email domain 
  
  # progress update
  if(k %% 1000 == 0){cat('Up to ', k, '.\r', sep='')}
  
}
table(unlist(data$country)) # check
prop.table(table(is.na(unlist(data$country)))) # check missing
table(unlist(data$edomain)) # check
prop.table(table(is.na(unlist(data$edomain)))) # check missing

# get citations and retractions from OpenAlex in batches
batch_size = 100 # number of DOIs to get from OpenAlex per request
max_batch = floor(nrow(data)/batch_size) + 1
open_alex = NULL
for (b in 1:max_batch){
  start = ((b-1)*batch_size) + 1
  stop = b*batch_size
  stop = min(stop, nrow(data)) # don't go beyond data
  from_oa = oa_fetch(doi = data$doi[start:stop],
                     entity = 'works',
                     abstract = FALSE,
                     options = list(select=c('doi','is_retracted', 'cited_by_count')),
                     api_key = my_open_alex_key,
                     mailto = 'a.barnett@qut.edu.au')
  open_alex = bind_rows(open_alex, from_oa)
  from_oa = NULL # safety net
}
# clean doi for merge
open_alex = mutate(open_alex, 
                   doi = str_remove(doi, 'https://doi.org/'),
                   doi = tolower(doi),
                   doi = str_squish(doi)) %>% 
  unique()
# 4 results with no OpenAlex data, e.g., 10.1371/journal.pclm.0000347
# 2 from open alex in twice,  10.1371/journal.pcsy.0000021 
# some with no records in Open Alex , e.g, 10.1371/journal.ppat.1008361
open_alex = group_by(open_alex, doi) %>%
  arrange(doi, desc(cited_by_count)) %>%
  slice(1) %>% # take paper with highest citations
  ungroup()
# add OpenAlex data to original data
data = left_join(data, open_alex, by='doi') %>%
  rename('citations' = 'cited_by_count',
         'oa_retracted' = 'is_retracted') %>%
  mutate(citations = as.numeric(citations))

## time between submission and acceptance (peer review time)
data = mutate(data, time_between = as.numeric(accepted - received),
              time_between = ifelse(time_between < 0, NA, time_between), # remove 55 negative numbers
              time_between = ifelse(time_between > 1461, NA, time_between) # remove ? over four years gap
)


## retractions
# get the latest retraction data from crossref
my.email = 'a.barnett@qut.edu.au'
url = paste("https://api.labs.crossref.org/data/retractionwatch?mailto=", my.email, sep='')
destfile = file.path(tempdir(), 'retractions.csv') # put in temporary folder
download.file(url, destfile = destfile, method='curl')
# now read into a csv
retractions = read.csv(destfile) %>%
  filter(RetractionNature == 'Retraction') %>%
  select(OriginalPaperDOI, Journal, RetractionDate) %>%
  rename('doi' = 'OriginalPaperDOI') %>%
  separate(RetractionDate, into=c('ret_date',NA), sep=' ')  %>%
  mutate(doi = str_squish(doi),
         doi = tolower(doi),
         ret_date = as.Date(ret_date, '%m/%d/%Y')) %>%
  unique()
# delete file to tidy up
unlink('retractions.csv')
# merge retractions with data
data = mutate(data, 
              doi = str_squish(doi), # standardise DOI for merge
              doi = tolower(doi))
censor.date = as.Date(Sys.Date()) # date RW database searched
data = left_join(data, retractions, by='doi') 
data = mutate(data, retracted = case_when( # binary retracted variable (cannot be part of previous merge)
  is.na(ret_date) == FALSE ~ TRUE,
  is.na(ret_date) == TRUE ~ FALSE
),
time_to_retraction = case_when( # calculate time to retraction
  is.na(ret_date) == FALSE ~ as.numeric(ret_date - published),
  is.na(ret_date) == TRUE ~ as.numeric(censor.date - published)
)) 
prop.table(table(data$retracted))*100 # check percentage retracted

# check for duplicates
table(table(data$doi)) # should all be 1
get_dupes(data)
# slim down data
data = select(data, -aff, -domain, -'Journal', -accepted, -ret_date) # no longer needed

# save
save(data, censor.date, file = 'data/2_processed.RData')
