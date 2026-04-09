# 99_citations_cheng.R
# using approach by Cheng et al who found opposite association between open review and citations (https://doi.org/10.1016/j.joi.2024.101540)
# April 2026

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# prepare the data
source('4_data_prepare_citations.R')

data =  mutate(data,
               rec = (as.numeric(received)-19000)/365.25, # making calendar time
               rec2 = log2(as.numeric(received)-14000))

cheng1 = glm(log2(citations+1) ~ review_available + log2(follow_up/55), data = data) # adjusting for follow-up time
summary(cheng1)
cheng2 = glm(log2(citations+1) ~ review_available + rec2, data = data) # adjusting for calendar time
summary(cheng2)
# shows a large benefit of open review
tidy(cheng1)
tidy(cheng2)
# t-test, copying Cheng 
one = filter(data, review_available == TRUE) %>% mutate(citations = log2(citations+1)) %>% pull(citations) 
two = filter(data, review_available == FALSE) %>% mutate(citations = log2(citations+1)) %>% pull(citations) 
t.test(one, two, alternative = 'two.sided')
# Cheng removed all citations of 0 and 1!