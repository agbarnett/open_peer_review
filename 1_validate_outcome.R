# 1_validate_outcome.R
# validate the main outcome
# August 2025
library(rmarkdown)

render(input = "1_validate_outcome.Rmd",
       output_format = 'word_document',
       output_dir = 'checks', # into separate checks directory
       output_file = "1_validate_outcome.docx")