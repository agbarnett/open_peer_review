# 2_validate_data.R
# validate the processed data
# September 2025
library(rmarkdown)

render(input = "2_validate_data.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate checks directory
       output_file = "2_validate_data.docx")