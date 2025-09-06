# 5_plot_bayes_results.R
# August 2025

library(rmarkdown)

render(input = "5_plot_bayes_results.Rmd",
       output_format = 'word_document',
       output_dir = 'results', # into separate results directory
       output_file = "5_plot_bayes_results.docx")