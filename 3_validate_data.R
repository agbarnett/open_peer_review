# 3_validate_data.R
# validate the processed data
# September 2025
library(rmarkdown)

render(input = "3_validate_data.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate checks directory
       output_file = "3_validate_data.docx")