# make_url.R
# function to make url from PLOS DOI; called by `validate` markdown files
# August 2025

make_url = function(doi){
  j = case_when(
    str_detect(doi, '\\.pntd') ~ 'plosntds',
    str_detect(doi, '\\.pone') ~ 'plosone',
    str_detect(doi, '\\.pgen') ~ 'plosgenetics',
    str_detect(doi, '\\.ppat') ~ 'plospathogens',
    str_detect(doi, '\\.pmed') ~ 'plosmedicine',
    str_detect(doi, '\\.pgph') ~ 'globalpublichealth',
    str_detect(doi, '\\.pbio') ~ 'plosbiology',
    str_detect(doi, '\\.pclm') ~ 'climate',
    str_detect(doi, '\\.pcbi') ~ 'ploscompbiol',
    str_detect(doi, '\\.pdig') ~ 'digitalhealth'
  )
  url = paste('https://journals.plos.org/', j, '/article?id=', doi, sep='')
  return(url)
}
