# 99_email_table.R
# make email table for the appendix
# June 2026
library(dplyr)
library(stringr)
library(xtable)
source('R/extract_email_domain.R') # for emails

#
example = c('a.barnett@qut.edu.au',
             'matt.spick@surrey.ac.uk',
             'dispy@yahoo.com',
             'po@harvard.edu',
             'Laa-laa@up.lublin.pl')
for_table = as.data.frame(examples) %>%
  mutate(domain = extract_email_domain(example))

# export 
print(xtable(for_table), include.rownames=FALSE, hline.after=FALSE, file = "results/99_email_table.tex")

