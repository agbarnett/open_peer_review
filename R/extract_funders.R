# extract_funders.R
# extract the name(s) of the funder(s) from the funding statement using text matching
# funder names from 0_examine_funders.R
# Feb 2026
source('R/tricky_funder_match.R') # additional function used to split false positives

extract_funders = function(text, funder_data){
  
  # return null if nothing or no funders
  if(is.na(text)==TRUE){matches = NULL; return(matches)}
  if(nchar(text)<=5){matches = NULL; return(matches)}
  #if(str_detect(text, pattern = '\\b(was|had|received) no (external|research|study )?funding\\b') == TRUE){matches = NULL; return(matches)}
  
  # clean up
  text = str_remove_all(text, pattern = dot_pattern) # remove full-stops from acronyms that can be confused with full-stops
  text = str_remove_all(text, pattern = remove_brackets) # remove everything in brackets, often grant numbers
  text = str_squish(text)
  
  # find funders (not using loop)
  matches = mutate(funder_data, statement = text) %>%
    filter(!is.na(lower_case)) %>% # remove empty
    mutate(mentioned = str_detect(tolower(statement), pattern = lower_case)) %>%
    filter(mentioned == TRUE) %>%
    pull(funder_number)

  # fixes
  # a) MRC and NHMRC
  both  = c(501100000265,501100000925) # MRC and NHMRC
  if(all(both %in% matches)){
    matches = matches[!matches%in%both] # first remove both numbers from matches
    new_matches = tricky_match(intext = text, funder_data = funder_data, funder_nums = both)
    matches = c(matches, new_matches)
  }  
  # b) NSF and Swiss NSF, e.g, 10.1371/journal.pcbi.1011742
  both  = c(100000001,501100001711) # NSF and SNSF
  if(all(both %in% matches)){
    matches = matches[!matches%in%both] # first remove both numbers from matches
    new_matches = tricky_match(intext = text, funder_data = funder_data, funder_nums = both)
    matches = c(matches, new_matches)
  }
  # c) NSF and China NSF
  both  = c(100000001,501100001809) # NSF and China NSF
  if(all(both %in% matches)){
    matches = matches[!matches%in%both] # first remove both numbers from matches
    new_matches = tricky_match(intext = text, funder_data = funder_data, funder_nums = both)
    matches = c(matches, new_matches)
  }  
  #
  matches = unique(matches) # remove duplicates
  return(matches)
}