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

## Bayes calculation for upper estimate of overlap
# beta priors and posterior
prior_a = 1
prior_b = 2 # vaguely informative prior
observed_a = 1 # overlap observed
observed_b = 99 # no overlap
posterior_a = prior_a + observed_a
posterior_b = prior_b + observed_b
#
x = seq(0,0.15,0.001) # range of errors to examine
prior = data.frame(x=x, d=dbeta(x, shape1 = prior_a, shape2=prior_b))
posterior = data.frame(x=x, d=dbeta(x, shape1 = posterior_a, shape2 = posterior_b))

# estimating 90% upper limit for author overlap
p_limit = 0.9
# prior
upper_prior = qbeta(p_limit, shape1 = prior_a, shape2 = prior_b)
upper_prior = round(upper_prior*1000)/1000
# overlap
upper_overlap = qbeta(p_limit, shape1 = posterior_a, shape2 = posterior_b)
upper_overlap = round(upper_overlap*1000)/1000
posterior_mode = (posterior_a - 1 )/ (posterior_a+posterior_b-2)
posterior_mode = round(posterior_mode*1000)/1000
