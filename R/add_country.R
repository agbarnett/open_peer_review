# add_country.R
# function to add country from affiliation
# just take unique countries as affiliation data is unique (so making a binary outcome of country yes/no)
# started from cited_reviewers
# July 2024

add_country = function(countries, states, text){
  # make changes to text to standardise
  text = str_replace_all(text, ', B.nin$', ', Benin') # accent
  text = str_replace_all(text, ', Brasil$', ', Brazil')
  text = str_replace_all(text, ', Korea$', ', Republic of Korea')
  text = str_replace_all(text, ', M..?xico$', ', Mexico') # accent
  text = str_replace_all(text, ', Russia$', ', Russian Federation')
  text = str_replace_all(text, ', S.n.gal$', ", Senegal") # accents
  text = str_replace_all(text, ', T.rkiye$', ', Turkey') # accent and stick with old name
  text = str_replace_all(text, ', Trinidad$', ", Trinidad and Tobago") 
  text = str_replace_all(text, ', United State ', ', United States')
  text = str_replace_all(text, ', United States$', ", United States of America")
  text = str_replace_all(text, ', UAE$', ", United Arab Emirates") 
  # add USA to every US state that is missing country
  text = add_us_state(states, text)
  #
  search = paste(paste('\\b', paste(countries, collapse='\\b|\\b'), sep=''), '\\b', sep='')
  country = stringi::stri_extract_last(str = text, regex = search, case_insensitive=TRUE) # take last country, e.g., 'Geisel School of Medicine at Dartmouth, Lebanon, NH, USA'; lower or upper case
  # consolidate some countries
  country = str_to_title(country) # remove all caps (title case)
  country = str_replace(country, ' Of ', ' of ')
  country = case_when(
    country == 'Brunei' ~ "Brunei Darussalam",
    country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
    country == "Cote d'ivoire" ~ "Côte d'Ivoire",
    country == "Cabo Verde" ~ "Cape Verde",
    country == "Czech Republic" ~ "Czechia",
    country == "UK" ~ "United Kingdom",
    country == "Uk" ~ "United Kingdom", # because of str_to_title
    country == "USA" ~ "United States of America",
    country == "Usa" ~ "United States of America", # because of str_to_title
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ as.character(country) # otherwise
  )
  return(country)
}