# run_all.R
# August 2026

# 0. Get the PLOS data
source("0_examine_funders.R")
source("0_read_xml_data.R")

# 1. Validate the outcome - not essential for reproducing the results
#source("1_validate_outcome.R")

# 2. Data processing
source("2_process.R")
#source("2_extract_reviews.R") # extract a random sample of reviews - not essential for reproducing the results

# 3. Get author experience data from OpenAlex, takes a long while and is prone to crashing from 429 errors
source("3_add_author_experience.R")
source("3_combine_experience_data.R") # combine the files made above
# source("3_validate_data.R") # validate the data - not essential for reproducing the results

# 4. Models
source("4_fractional_polynomial.R")
source("4_citations.R")
source("4_survival_retractions.R")
source("4_stability_selection.R")

# 5. Checks and plots
source('5_model_checks.R')
source('5_plots.R')
source('5_model_checks.R')
source('5_summary_table.R')
