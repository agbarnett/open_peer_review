# 99_author_overlap.R
# check author overlap in a random sample to examine the issue of assuming independence between articles
# September 2025
library(rvest) # for reading XML
library(stringr)
library(dplyr)

# take random sample of articles and authors
n_sample = 100 # number of papers to sample
TeachingDemos::char2seed('blackpool')
# get data for included DOIs
load('data/1_processed.RData')
dois = sample_n(data, replace = FALSE, size = n_sample) %>% pull(doi)
dois = str_remove(dois, '10.1371/')
dois = paste(dois, '.xml', sep='') # for text matching below

# XML files
here = getwd()
xml_location = "C:/Users/barnetta/Downloads/allofplos" # had to put on C drive because could not unzip on U drive
setwd(xml_location)

all_authors = NULL
for (k in 1:n_sample){
  article = read_html(dois[k])
  
  # DOI
  doi = article %>%
    html_nodes("article-meta") %>%
    html_nodes("article-id[pub-id-type='doi']") %>%
    html_text2()
  
  # author names
  authors = article %>% 
    html_nodes('article-meta') %>%
    html_nodes("contrib[contrib-type='author']") %>%
    html_nodes('name')
  surnames = authors %>% html_nodes('surname') %>% html_text()
  first = authors %>% html_nodes('given-names') %>% html_text()
  name = bind_cols(first, surnames)
  names(name) = c('first','surname') 
  name = mutate(name, doi = doi)
  
  #
  all_authors = bind_rows(all_authors, name)
}
setwd(here)
#

# find repeated authors
for_duplicates = select(all_authors, -doi)
janitor::get_dupes(for_duplicates)
filter(all_authors, first == 'Yuxuan')

#
head(arrange(all_authors, desc(surname), desc(first)))
head(arrange(all_authors, surname, first))
