# extract_email_domain.R
# extract domain from email
# August 2025, copied from `extract_country.R` in publication_preferences repository

extract_email_domain = function(email){
  email = tolower(email)
  domain = case_when(
  # other common codes
    str_detect(email, '(^|\\b)edu\\b') ~ 'edu',
    str_detect(email, '(^|\\b)net\\b') ~ 'net',
   str_detect(email, '(^|\\b)cat\\b') ~ 'cat',
   str_detect(email, '(^|\\b)gov\\b') ~ 'gov',
   str_detect(email, '(^|\\b)org\\b') ~ 'org',
   str_detect(email, '(^|\\b)uni\\b') ~ 'uni',
   str_detect(email, '(^|\\b)ac\\b') ~ 'ac',
   str_detect(email, '(^|\\b)med\\b|(^|\\b)med(i|e)cine\\b') ~ 'med', # with non UK spelling
   str_detect(email, '(^|\\b)phd\\b|(^|\\b)hdr\\b|(^|\\b)student\\b') ~ 'phd',
   str_detect(email, '(^|\\b)nhs\\.') ~ 'nhs', # NHS
   str_detect(email, '(^|\\b)bio') ~ 'bio',
   str_detect(email, '(^|\\b)zool?\\b') ~ 'zoo',
   # combine .com but keep common ones
   str_detect(email, 'gmail\\.com') ~ 'gmail',
   str_detect(email, '163\\.com') ~ '163',
   str_detect(email, '126\\.com') ~ '126',
   str_detect(email, 'yahoo\\.(com?|fr)') ~ 'yahoo',
   str_detect(email, 'hotmail\\.(com?|fr)') ~ 'hotmail',
   str_detect(email, 'aol\\.com') ~ 'aol',
   str_detect(email, 'msn\\.com') ~ 'msn',
   str_detect(email, 'qq\\.com') ~ 'qq',
   str_detect(email, 'sina\\.com') ~ 'sina',
   str_detect(email, '\\.com?\\b') ~ 'com', # all other .com and .co
  TRUE ~ "Other" # otherwise
  )
  return(domain)
}
