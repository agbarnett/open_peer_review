# countries_and_states.R
# get lists of countries and US states
# August 2025

## part 1: countries
# read in the country data
countries = read.csv('data/all_countries.csv') %>% # from https://github.com/agbarnett/ISO-3166-Countries-with-Regional-Codes/tree/master/all
  clean_names() %>%
  pull(name)

## part 2: states from Wikipedia
url_states = 'https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations'
# Get US states 
page = read_html(url_states)
table = page %>% 
  html_elements("table") %>%
  .[2] %>% # second table 
  html_table(header = FALSE, fill = TRUE) # ignore header
states = table[[1]] %>%
  filter(X2=='State') %>%
  select(X1, X7) %>%
  rename('state' = 'X1',
         'postal' = 'X7') %>%
  pull(state) # just use state

# tidy
remove(url_states, table, page)
