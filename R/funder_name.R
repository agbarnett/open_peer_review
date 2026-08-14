# funder_name.R
# function to replace funder numbers with names
# see https://www.crossref.org/documentation/funder-registry/funding-data-overview/
# and from http://dx.doi.org/10.13039/
# August 2026

# version for loops checking single rows
funder_name_single = function(funder) {
  has_start = str_detect(funder, pattern = 'Funder = ')
  if (has_start == TRUE) {
    funder = str_remove(funder, '^Funder = ')
  }
  outname = case_when(
    # replace numbers with names
    funder == '501100007601' ~ "Horizon 2020",
    funder == '501100000781' ~ "European Research Council", #
    funder == '501100000265' ~ 'Medical Research Council',
    funder == '501100001809' ~ 'National Natural Science Foundation of China',
    funder == '100000001' ~ 'National Science Foundation',
    funder == '100000002' ~ 'National Institutes of Health',
    funder == '100010269' ~ 'Wellcome', # combined with 100004440
    funder == '501100001321' ~ 'National Research Foundation, South Africa',
    funder ==
      '100000060' ~ 'National Institute of Allergy and Infectious Diseases',
    funder == '100010002' ~ 'Ministry of Education, China',
    funder == '501100002701' ~ 'Ministry of Education, Republic of Korea',
    funder == '501100001691' ~ "Japan Society for the Promotion of Science",
    funder == '501100001659' ~ "German Research Foundation",
    funder == '100010663' ~ 'H2020 European Research Council',
    funder == '501100001665' ~ 'French National Research Agency',
    funder == '501100004189' ~ 'Max Planck Society', # for the Advancement of Science; cut label
    funder == '100000011' ~ 'Howard Hughes Medical Institute',
    TRUE ~ as.character(funder)
  )

  # add back
  if (has_start == TRUE) {
    outname = paste('Funder =', outname)
  }

  return(outname)
}

# version for checking multiple rows
funder_name = function(funder) {
  outname = case_when(
    # replace numbers with names
    funder == '501100007601' ~ "Horizon 2020",
    funder == '501100000781' ~ "European Research Council", #
    funder == '501100000265' ~ 'Medical Research Council',
    funder == '501100001809' ~ 'National Natural Science Foundation of China',
    funder == '100000001' ~ 'National Science Foundation',
    funder == '100000002' ~ 'National Institutes of Health',
    funder == '100010269' ~ 'Wellcome', # combined with 100004440
    funder == '501100001321' ~ 'National Research Foundation, South Africa',
    funder ==
      '100000060' ~ 'National Institute of Allergy and Infectious Diseases',
    funder == '100010002' ~ 'Ministry of Education, China',
    funder == '501100002701' ~ 'Ministry of Education, Republic of Korea',
    funder == '501100001691' ~ "Japan Society for the Promotion of Science",
    funder == '501100001659' ~ "German Research Foundation",
    funder == '100010663' ~ 'H2020 European Research Council',
    funder == '501100001665' ~ 'French National Research Agency',
    funder == '501100004189' ~ 'Max Planck Society', # for the Advancement of Science; cut label
    funder == '100000011' ~ 'Howard Hughes Medical Institute',
    TRUE ~ as.character(funder)
  )

  return(outname)
}
