# 99_list_subjects.R
# list all subjects for the paper's appendix
# March 2026
library(dplyr)

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# prepare the data
source('4_data_prepare.R')

# by alphabetical order - split by letter of the alphabet
out = file('results/99_subject.tex', 'w')
cat('A\n\n', file=out, sep='')
current_letter = 'A'
for (word in subjects_to_use){
  if(substr(word,1,1) != current_letter){
    current_letter = substr(word,1,1)
    cat('\n\n', current_letter, '\n\n', file=out, sep='')
  }
  cat(word, ', ', file=out, sep='')
}
close(out)
