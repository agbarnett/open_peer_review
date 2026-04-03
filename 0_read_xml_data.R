# 0_read_xml_data.R
# downloaded and unzipped entire PLOS corpus from https://plos.org/text-and-data-mining/ in March 2026
# some helpful code here https://pgomba.github.io/pgb_website/posts/08_10_23/
# February 2026
library(tidyverse)
library(rvest)
library(stringr)
source('99_conflict_pattern.R') # conflict of interest patterns
source('R/extract_funders.R') # function to extract funder name from text

# funder text from 0_examine_funders.R
load('data/0_funder_info.RData')

# types of paper to exclude, not peer reviewed
source('99_excluded.R')

# get all XML files in subfolder on downloads
here = getwd()
xml_location = "C:/Users/barnetta/Downloads/allofplos" # had to put on C drive because could not unzip on U drive
setwd(xml_location)
d = dir(xml_location) 
length(d) # should be 391,477
count_corrections = sum(str_detect(d, 'correction')) # needed for flow chart
d = d[!str_detect(d, 'correction')] # remove corrections
table(nchar(d)) # check, should all be 24
# re-order to catch bugs quicker
N = length(d)
d = sample(d, size = N, replace=FALSE)

# big loop to extract data
data = excluded = NULL
start = 5876 # for restarts
# missing_k = which(str_detect(d, pattern=paste(d_not,collapse='|'))) # for filling in missing papers
for (k in start:N){
  
  # infile = 'journal.pone.0240295.xml' # simple test
  infile = d[k] # 
  article = read_html(infile)
  
  # DOI
  doi = article %>%
    html_nodes("article-meta") %>%
    html_nodes("article-id[pub-id-type='doi']") %>%
    html_text2()
  
  
  # exclude if not an XML file
  if(is(article, 'xml_node') == FALSE){
    frame = data.frame(doi = doi, reason = 'XML issue')
    excluded = bind_rows(excluded, frame)
    next;
  }
  
  # paper type 
  paper_type <- article %>%
    html_nodes("subj-group[subj-group-type='heading']") %>%
    html_nodes("subject") %>%
    html_text2() %>% .[1] # safety net for occasional 
  # exclude if article is not peer reviewed
  if(paper_type %in% exclude_types){
    frame = data.frame(doi = doi, reason = paper_type)
    excluded = bind_rows(excluded, frame)
    next;
  }
  
  ## submission and acceptance dates
  dates <- article %>%
    html_nodes("history") %>%
    html_nodes('date')
  # exclude if no dates of acceptance and submission - possibly means not reviewed
  if(length(dates) == 0){
    frame = data.frame(doi = doi, reason = 'No date', type = paper_type)
    excluded = bind_rows(excluded, frame)
    next;
  }
  d1 = html_attrs(dates) %>% bind_rows()
  year = dates %>% html_nodes('year') %>% html_text()
  month = dates %>% html_nodes('month') %>% html_text()
  day = dates %>% html_nodes('day') %>% html_text()
  d2 = as.Date(ISOdate(year = year, month = month, day = day))
  dates = bind_cols(d1, dates = d2) 
  # fix occasional missing
  received = filter(dates, `date-type` == 'received') %>% pull(dates)
  if(length(received)==0){received = NULL} # very occasionally missing, e.g. "10.1371/journal.ppat.1000046"
  accepted = filter(dates, `date-type` == 'accepted') %>% pull(dates)
  if(length(accepted)==0){accepted = NULL} # very occasionally missing, e.g. "10.1371/journal.pcbi.0020079"; use NULL instead of NA for tibble
  # exclude if missing dates
  if(is.null(received)|is.null(accepted)){
    frame = data.frame(doi = doi, reason = 'Missing dates', type = paper_type)
    excluded = bind_rows(excluded, frame)
    next;
  }
  # exclude if submitted before May 22 2019, as this is when open peer review policy was introduced
  if(received < as.Date('2019-05-22')){
    frame = data.frame(doi = doi, reason = 'Before policy change', type = paper_type)
    excluded = bind_rows(excluded, frame)
    next;
  }
  
  ## funders as a list; there is also institution-id, but it's not always available
  ## sometimes there's only a funding statement, e.g., 10.1371/journal.pone.0240295
  # get statement
  funders_statement = article %>%
    html_nodes("funding-group") %>%
    html_nodes('funding-statement') %>%
    html_text2() %>%
    paste(collapse = ' ')
  # rare to be missing, glitch here: "10.1371/journal.pone.0259601"
  if(length(funders_statement) == 0){
    funders_statement = NA
  }
  # now extract funder number from statement
  if(length(funders_statement) > 0){
    funders_statement = extract_funders(text = funders_statement, funder_data = funder_text) # isolate funder number from the text
  }
  # get funder(s) names; second source
  funders = article %>%
    html_nodes("funding-group") %>%
    html_nodes("funding-source") %>%
    html_nodes("institution") %>%
    html_text2() %>%
    unique() %>%
    paste(collapse = ' ')
  # now extract funder number from names
  if(length(funders) > 0){
    funders = extract_funders(text = funders, funder_data = funder_text) # isolate funder number from the text
  }
  # assume missing means no funders
  if(length(funders) == 0){
    funders = NULL
  }
  # get funder number(s); third source
  funder_number = article %>%
    html_nodes("funding-group") %>%
    html_nodes("funding-source") %>%
    html_nodes("institution-id") %>%
    html_text2() %>%
    unique()
  funder_number = str_remove_all(funder_number, 'http://dx.doi.org/10.13039/')
  if(length(funder_number) == 0){
    funder_number = NULL
  }
  funder_number_consolidated = unique(c(funders_statement, funders, funder_number))
  
  # subjects (list)
  subject <- article %>%
    html_nodes("article-meta") %>%
    html_nodes(xpath = "//subj-group[starts-with(@subj-group-type,'Discipline')]") %>%
    html_nodes("subject") %>%
    html_text2()
  # can occasionally be missing from XML, e.g., 10.1371/journal.pbio.1000260
  if(length(subject) == 0){
    subject = "Missing"
  }
  
  # journal name
  journal_name <- article %>%
    html_nodes("journal-title-group") %>%
    html_nodes("journal-title") %>%
    html_text2()
  # back up for missing journal because of XML style
  if(length(journal_name) == 0){
    journal_name = article %>%
      html_nodes("journal-meta") %>%
      html_nodes("journal-id[journal-id-type='nlm-ta']") %>%
      html_text2()
  }
  
  # number of authors
  n_authors = article %>% 
    html_nodes('article-meta') %>%
    html_nodes("contrib[contrib-type='author']") %>%
    length()
  
  # publication date
  pdate <- article %>%
    html_nodes("article-meta") %>%
    html_nodes('pub-date[pub-type="epub"]')
  year = pdate %>% html_nodes('year') %>% html_text()
  month = pdate %>% html_nodes('month') %>% html_text()
  day = pdate %>% html_nodes('day') %>% html_text()
  d3 = as.Date(ISOdate(year = year, month = month, day = day))
  if(length(d3)!=1){stop('Error in publication date\n')}
  
  # Authors' affiliations (only gives unique affiliations, so is not equal to number of authors)
  aff <- article %>%
    html_nodes("article-meta") %>%
    html_nodes(xpath = "//aff[starts-with(@id,'aff')]") %>% # to exclude editor
    html_nodes('addr-line') %>%
    html_text2()
  # occasionally missing, e.g., 10.1371/journal.pbio.1000313, consortium authors
  if(length(aff) == 0){aff = 'Missing'}
  
  # Number of ORCIDs
  orcids = article %>%
    html_nodes("contrib-id[contrib-id-type='orcid']") %>%
    html_text2() %>%
    unique()
  n_orcids = length(orcids)
  
  # contact email domain (just from @)
  email <- article %>%
    html_nodes("author-notes") %>%
    html_nodes("corresp") %>%
    html_nodes("email") %>%
    html_text2() 
  # count unique emails
  n_emails = length(unique(email))
  # now remove everything before @
  email = str_remove(email, pattern = '.*@') 
  # back up for alternative XML 
  if(length(email) == 0){
    email = article %>%
      html_nodes("author-notes") %>%
      html_nodes("fn[fn-type='current-aff']") %>%
      html_nodes("email") %>%
      html_text2() 
    # count unique emails
    n_emails = length(unique(email))
    # now remove everything before @
    email = str_remove(email, pattern = '.*@') 
  } 
  # email can be missing, e.g., 10.1371/journal.pbio.0020110
  if(length(email) == 0){email = 'Missing'}

  ## conflict of interest
  conflict = article %>%
    html_nodes("fn[fn-type='conflict']") %>%
    html_text2() %>%
    paste(collapse = ' ') # avoid doubles
  if(length(conflict)==0){next}
  conflict = str_remove(conflict, '• ') # remove odd bullet
  # binary no conflict
  no_conflict = str_detect(tolower(conflict), pattern = conflict_pattern)

  
  ## are there any author comments, meaning: is peer review open? (dependent variable)
  comments <- article %>%
    html_nodes("sub-article") %>%
    html_attrs() %>% 
    bind_rows()
  review_available = nrow(comments) > 0
  
  # stop if data is not complete
  if(length(doi) == 0 | length(journal_name) == 0 | length(subject) == 0 | length(aff) == 0 | length(no_conflict) == 0 | 
     length(n_orcids) == 0 | length(dates) == 0 | length(email) == 0){stop('Incomplete data for ', k, '\n', sep='')}
  # stop if data is too long
  if(length(doi) > 1 | length(journal_name) > 1 | length(dates) != 2){stop('Too much data for ', k, '\n', sep='')}
  
  # make and store data frame
  frame = tibble(doi = doi,
                 journal = journal_name,
                 type = paper_type,
                 n_authors = n_authors,
                 n_orcids = n_orcids,
                 subjects = list(unique(subject)), # keep subjects as a list; remove duplicates 
                 aff = list(aff), # keep affiliations as a list
                 domain = list(email), # can be multiple emails, so keep as list
                 n_emails = n_emails,
                 no_conflict = no_conflict,
                 received = received, # date
                 accepted = accepted, # date
                 published = d3, # date
                 funder_number_consolidated = list(funder_number_consolidated), # can be multiple, so keep as list
                 review_available = review_available
  )
  data = bind_rows(data, frame)
  frame = NULL
  
  # progress update
  if(k %% 1000 == 0){cat('Up to ', k, '.\r', sep='')}
  
}

# tidy
data = mutate(data, 
              type = str_replace(type, "RESEARCH ARTICLE", "Research Article"),
              type = str_replace(type, "Methods & Resources", "Methods and Resources"),
              journal = str_replace(journal, "PLoS Biol$", "PLOS Biology"),
              journal = str_replace(journal, "PLoS", "PLOS"), # for consistency
              journal = str_replace(journal, "PLOS One", "PLOS ONE"),
              journal = str_replace(journal, "PLOS Medicin$", "PLOS Medicine")) %>%
  unique() # safety net from restarts

# check
if(nrow(excluded) + nrow(data) != N){cat('Error, missing papers\n')}
# what papers are missing
d_to_compare = str_remove_all(d,'journal\\.|\\.xml')
e_to_compare = mutate(excluded, doi = str_remove_all(doi, '10\\.1371/|journal.')) %>% pull(doi)
table(duplicated(e_to_compare))
i_to_compare = mutate(data, doi = str_remove_all(doi, '10\\.1371/|journal.')) %>% pull(doi)
table(duplicated(i_to_compare))
t_to_compare = c(e_to_compare, i_to_compare)
table(duplicated(t_to_compare))
d_not = d_to_compare[!d_to_compare%in%t_to_compare] # which files are missing
t_not = t_to_compare[!t_to_compare%in%d_to_compare] # which files are missing
# check for doubles, should all be 1
table(table(data$doi))

## save
setwd(here)
save(data, excluded, count_corrections, file = 'data/0_unprocessed.RData')
