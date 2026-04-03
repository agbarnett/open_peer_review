# 0_examine_funders.R
# looking for most common funders - extract all names associated with numbers
# had to be frugal with additional names, e.g., "Edwards Lifesciences" had an alternative name of "Edwards" which gave lots of false positives
# avoid false matches, e.g., NSF will match with "NSF of China"
# see https://www.crossref.org/documentation/funder-registry/funding-data-overview/
# April 2026
library(dplyr)
library(jsonlite) # for reading JSON files on funders
library(stringr)
url_start = 'http://dx.doi.org/10.13039/' # where funder data is; from crossref

# from 0_read_xml_data.R (slightly circular to use observed data at same stage)
load('data/0_unprocessed.RData')

# find most common funder numbers from PLOS data (first remove some extra text before the number)
numbers = unlist(data$funder_number_consolidated) %>%
  str_remove('https://data.crossref.org:443/fundingdata/funder|http://data.crossref.org/fundingdata/funder/10.13039/|https?://(dx.)?doi.org/10.13039/|https?://doi.org/10.3030|https?://dx.doi.org') %>%
  str_remove('\\/')
numbers = numbers[!is.na(numbers)]
table(nchar(numbers)) # check for non-numbers
tab = table(numbers) %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  filter(Freq > 5) # ignore those mentioned rarely; very unlikely to be predicotrs in model
N = nrow(tab)
cat('We examine ', N, ' funders.\n', sep='')

# loop through all funders (takes a few minutes)
funder_text = NULL
for (k in 1:N){ # takes a little while due to web searches
  
  # find funder names from web
  number = tab$numbers[k] # get one number
  url = paste(url_start, number, sep='')
  funder = fromJSON(url)
  
  # main name only; two many problems with English version of the name, for example "Heart Foundation" = 501100002996, but Dutch
  main_name = funder$prefLabel$Label$literalForm$content
  if(length(main_name)>1){cat('Multiple names for ', number, '\n', sep='')}
  
  # if 'u.s.' or other country acronyms
  alternatives = NULL
  if(str_detect(main_name, 'U\\.S\\.A\\. ')){
    alternative = str_replace(main_name, 'U\\.S\\.A\\. ', 'USA ')
    alternatives = c(alternatives, alternative)
  }
  if(str_detect(main_name, 'U\\.S\\. ')){
    alternative = str_replace(main_name, 'U\\.S\\. ', 'US ')
    alternatives = c(alternatives, alternative)
  }
  if(str_detect(main_name, 'U\\.K\\. ')){
    alternative = str_replace(main_name, 'U\\.K\\. ', 'UK ')
    alternatives = c(alternatives, alternative)
  }
  if(str_detect(main_name, 'G\\.B\\. ')){
    alternative = str_replace(main_name, 'G\\.B\\. ', 'GB ')
    alternatives = c(alternatives, alternative)
  }
  if(str_detect(main_name, ' and ')){ # to cover common use of ampersand
    alternative = str_replace(main_name, ' and ', ' & ')
    alternatives = c(alternatives, alternative)
  }
  all_names = c(main_name, alternatives)
  all_names = unique(all_names)
  
  # common processing for all names
  all_names = all_names[!str_detect(all_names, '^(The )?Department (of|Of) Health$')] # remove generic labels
  all_names = all_names[!is.na(all_names)] # remove missing
  all_names = tolower(all_names) # can covert to lower case
  all_names = paste('\\b', paste(all_names, collapse='\\b|\\b'), '\\b', sep='') # paste all together; add breaks to reduce false positives

  # check
  if(length(all_names) != 1){cat('Error for k = ', k, '\n', sep='')}
  
  # concatenate data
  frame = data.frame(funder_number = as.character(number),
                     lower_case = all_names) #
  funder_text = bind_rows(funder_text, frame)
  # reset
  all_names = NA
  Sys.sleep(0.1) # avoid timeouts from crossref
  # progress
  if(k%%500==0){cat('Up to ', k, '\n', sep='')}
}


# check
filter(funder_text, funder_number == 100010269) # should be Wellcome
filter(funder_text, funder_number == 501100000265) # should be UK MRC
filter(funder_text, funder_number == 501100000925) # should be NHMRC
filter(funder_text, funder_number == 100009054) # good check of empty lower case
filter(funder_text, funder_number == 501100000780) # EU
filter(funder_text, funder_number == 100007397) # check there is no EU or space
filter(funder_text, funder_number == 501100003921) # Australian department of health and ageing
filter(funder_text, funder_number == 100011893) # check no space

# one fix
funder_text = mutate(funder_text, lower_case = str_replace(lower_case, pattern='cooperation gmbh\\)', replacement='cooperation gmbh'))

# remove double Wellcome
funder_text = filter(funder_text, funder_number != 100004440)

# remove bland names that will have false positives
bland = c('ministry of education\\\\',
          'ministry of health and welfare\\\\')
bland = paste(bland, collapse='|')
funder_text = filter(funder_text, !str_detect(lower_case, bland))

# remove duplicates, e.g., boehringer ingelheim
funder_text = group_by(funder_text, lower_case) %>%
  arrange(lower_case, funder_number) %>%
  slice(1) %>%
  ungroup()

# save
save(funder_text, file = 'data/0_funder_info.RData')

# check duplicates
index = select(funder_text, lower_case) %>%
  duplicated()
funder_text[index,]
