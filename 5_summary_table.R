# 5_summary_table.R
# makes the giant baseline table
# August 2026
library(rmarkdown)

render(input = "5_summary_table.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate results directory
       output_file = "5_summary_table.docx")
