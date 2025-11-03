# 99_random_checks.R
# random checks of countries
# September 2025
library(rmarkdown)
library(dplyr)
library(stringr)

this_country = 'Ethiopia'
#this_country = 'Poland'
outfile = paste("99_random_checks_", this_country, ".docx", sep='')
render(input = "99_random_checks.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate checks directory
       output_file = outfile)

# export data to tab-delimted file; need to remove lists
outfile = paste('data/99_country_', this_country, '.txt', sep='')
country_data = select(country_data, -subjects) # too messy to export
df = data.frame(lapply(country_data, as.character), stringsAsFactors=FALSE) # remove lists
write.table(df, file = outfile, quote=FALSE, row.names = FALSE, sep='\t')
