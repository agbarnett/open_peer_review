# 2_summary_table.R
# September 2025

library(rmarkdown)

render(input = "2_summary_table.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate results directory
       output_file = "2_summary_table.docx")