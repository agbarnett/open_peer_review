# 1_examine_funders.R
# looking for most common funders - extract all names associated with numbers
# Nov 2025
library(dplyr)
library(jsonlite) # for reading JSON files on funders
library(stringr)
url_start = 'http://dx.doi.org/10.13039/' # where funder data is; from crossref

# from 0_read_xml_data.R
load('data/0_unprocessed.RData')

# find most common funder numbers (first remove some extra text before the number)
numbers = unlist(data$funder_number) %>%
  str_remove('https://data.crossref.org:443/fundingdata/funder|http://data.crossref.org/fundingdata/funder/10.13039/|https?://(dx.)?doi.org/10.13039/|https?://doi.org/10.3030|https?://dx.doi.org') %>%
  str_remove('\\/')
numbers = numbers[!is.na(numbers)]
table(nchar(numbers)) # check for non-numbers
tab = table(numbers) %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  filter(Freq > 20)  # only funders with twenty or more hits

# loop through all funders
funder_text = NULL
N = nrow(tab)
for (k in 1:N){ # takes a little while due to web searches
  
  # find funder names from web
  url = paste(url_start, tab$numbers[k], sep='')
  funder = fromJSON(url)
  
  # paste all names together (preferred and alternative) ...
  if(is.null(funder$altLabel) == FALSE){ # ... only for those with multiple labels
    all_names = c(funder$prefLabel$Label$literalForm$content, funder$altLabel$Label$literalForm$content) %>%
      tolower() %>%
      paste(collapse='\\b|\\b') # paste all together; add breaks because of acronyms
  }
  if(is.null(funder$altLabel) == FALSE){
    all_names = tolower(funder$prefLabel$Label$literalForm$content)
  }
  # check
  if(length(all_names) != 1){cat('Error for k = ', k, '\n', sep='')}
  
  # concatenate data
  frame = data.frame(funder_number = as.character(tab$numbers[k]),
                     search_text = paste('\\b', all_names, '\\b', sep=''))
  funder_text = bind_rows(funder_text, frame)
}

# save
save(funder_text, file = 'data/1_funder_info.RData')
