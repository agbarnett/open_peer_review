# 99_open_science_by_country.R
# examine open science practices over the same period (from March 2019) for the eight countries with a low/high probability
# - using pubmed to examine preprints by country
# - DORA signatories
# - Open Access papers
# December 2025
library(rentrez)
library(openalexR)
library(dplyr)
library(tidyr)
library(xtable)
library(readxl)
library(janitor)
library(rvest)

## part 0
# country codes for OpenAlex, see https://api.openalex.org/institutions?group_by=country_code
country_codes = read.table(header=TRUE, sep=',', text='
code,country
GB,UK
FR,France
PK,Pakistan
NL,Netherlands
ET,Ethiopia
KR,South Korea
CN,China
SA,Saudi Arabia
')

## part 1: DORA signatories by country, from a search of the DORA web site made on 12 december 2025
dora = read.table(sep=',', header=TRUE, text='
country,individual,organisations
UK,1945,293
France,1343,96
Ethiopia,20,0
South Korea,239,8
Netherlands,434,30
China,140,10
Pakistan,100,59
Saudi Arabia,46,4'
)

## denominators for part 1
# get number of universities per country as a denominator using data from Wikipedia
#source('99_wikipedia_universities.R')
#dora = left_join(dora, universities, by='country')
# do not use as denominators are clearly not working

# estimate number of current authors; with country code
author_numbers = NULL
for (k in 1:nrow(country_codes)){
  
  res = oa_fetch(entity = 'authors',
                 last_known_institutions.country_code = country_codes$code[k], # last known institution, will count if an author has multiple institutions
                 'summary_stats.2yr_mean_citedness' = '>1', # trying to get recent researchers
                 count_only = TRUE,
                 verbose = TRUE, # to get url
                 mailto = 'a.barnett@qut.edu.au')
  
  # add to data frame
  frame = data.frame(country = country_codes$country[k], number = res$count)
  author_numbers = bind_rows(author_numbers, frame)
  
}
# merge counts and denominators
dora = left_join(dora, author_numbers, by = 'country') %>%
  mutate(rate = round(individual / (number/100000))) %>% # rate per 100,000 authors
  rename('arate' = 'rate',
         'anumber' = 'number')

## part 2: preprints
countries = list(c('China'),
                 c('France'),
                 c('Ethiopia'),
                 c('Netherlands','Holland'),
                 c('UK','United Kingdom','Wales','England','Scotland','Northern Ireland'),
                 c('South Korea','Republic of Korea'),
                 c('Saudi Arabia'),
                 c('Pakistan'))
preprint_data = NULL
for (country_num in 1:length(countries)){
  this_country = countries[[country_num]]
  if(length(this_country) == 1){
    country_text = paste('"', this_country, '"[AFFL]', sep='')
  }
  if(length(this_country) > 1){
    country_text = paste('(', paste(this_country, sep='', collapse= ' OR '), ')[AFFL]', sep='')
  }
  # preprints
  query1 = paste(country_text,' AND preprint[ptyp] AND 2019:2025[pdat]', sep='')
  qres1 = entrez_search(db = 'pubmed', term = query1)
  # all articles
  query2 = paste(country_text,' AND 2019:2025[pdat]', sep='')
  qres2 = entrez_search(db = 'pubmed', term = query2)
  #
  this_data = data.frame(country = this_country[1], number = qres1$count, denominator = qres2$count)
  preprint_data = bind_rows(preprint_data, this_data)
}
# calculate preprint rate per 10,000 papers
preprint_data = mutate(preprint_data, rate = round(number / (denominator/10000)))

## part 3: open access
# variable list, see https://docs.openalex.org/api-entities/works/work-object
vars_to_select = c('doi','publication_date','open_access','authorships')
# loop through countries
open_data = NULL
for (k in 1:nrow(country_codes)){
  
  works_count_open <- oa_fetch(
    entity = "works",
    type = 'article',
    options = list(select = vars_to_select), # not needed as using counts, but useful for checking
    authorships.countries = country_codes$code[k],
    is_oa = TRUE,
    is_paratext = FALSE, # don't want front covers, etc
    output = 'list',
    mailto = 'a.barnett@qut.edu.au',
    from_publication_date = "2019-03-01", # from March 2019
    verbose = TRUE,
    count_only = TRUE
  )
  
  works_count_closed <- oa_fetch(
    entity = "works",
    type = 'article',
    options = list(select = vars_to_select), # not needed as using counts, but useful for checking
    authorships.countries = country_codes$code[k],
    is_oa = FALSE,
    is_paratext = FALSE, # don't want front covers, etc
    output = 'list', # not needed as using counts, but good for checking
    mailto = 'a.barnett@qut.edu.au',
    from_publication_date = "2019-03-01",
    verbose = TRUE,
    count_only = TRUE
  )
  
  # add to data frame
  frame1 = data.frame(country = country_codes$country[k], type = 'open', number = works_count_open$count)
  frame2 = data.frame(country = country_codes$country[k], type = 'closed', number = works_count_closed$count)
  open_data = bind_rows(open_data, frame1, frame2)
  
}
#
open_data_wide = pivot_wider(open_data, values_from = 'number', names_from = 'type') %>%
  mutate(total = open + closed,
         percent_oa = round(100*open/total)) # calculate percentage open access


## part 4: merge sources and output
table = full_join(dora, preprint_data, by='country') %>%
  full_join(open_data_wide, by='country') %>%
  arrange(country)
#
date.searched = as.Date(Sys.Date())
save(table, date.searched, file='results/99_open_science.RData')

# output to latex
for_latex = mutate(table,
                   empty = '', # empty column to start (probability of open review)
                   dora = paste(individual, ' (', arate, ')', sep='')) %>%
  select(empty, country, dora, rate, percent_oa) 
print(xtable(for_latex, digits=0), math.style.negative=FALSE, include.rownames=FALSE, hline.after=FALSE, file = "results/99_open_science.tex")


## part 5: check preprints against PLOS ONE paper
# data is preprints from inception to May 2021
preprint_check = read_excel(path = 'data/pone.0281659.s001.xlsx') %>%
  clean_names() %>%
  mutate(country = case_when(
    country == 'United Kingdom' ~ 'UK',
    TRUE ~ as.character(country)
  ))
cross_check = left_join(preprint_data, preprint_check, by = 'country')
with(cross_check, plot(number, number_of_preprints))
with(cross_check, cor(number, number_of_preprints, method ='spearman')) # looks good
