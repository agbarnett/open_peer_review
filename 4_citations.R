# 4_citations.R
# model of citation counts dependent on open peer review
# March 2026
library(dplyr)
library(stringr)
library(broom)
library(DHARMa) # for residuals

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# prepare the data
source('4_data_prepare.R')

# make follow-up time to offset citation counts (scaled to per year)
date.searched = as.Date('2026-03-02') # date that open alex was searched
data = mutate(data, follow_up = as.numeric(date.searched - published)/365.25)

# follow-up time as offset, over-dispersion is needed; scaled offset to a year
model = glm(citations ~ review_available, 
            offset=log(follow_up), 
            data = data, 
            family=quasipoisson())
summary(model)
tidy(model, conf.int=TRUE, exponentiate = TRUE)

# residual checks using DHARMa
model_not_dispersed = glm(citations ~ review_available, offset=log(follow_up), data = data, family=poisson()) # can`t use over-dispersed for residuals
simulationOutput <- simulateResiduals(fittedModel = model_not_dispersed, plot = FALSE)
hist(simulationOutput$scaledResiduals)
# plot of the residuals against the predicted value:
plotResiduals(simulationOutput) # larger residuals for larger predictions

# estimate difference in citation numbers at 3 years (it is simply proportional, but the intercept gives the idea of the absolute numbers)
fu = 3 # in years
newdata = data.frame(follow_up = fu, review_available = c(TRUE, FALSE) ) 
# pred_manual = model$coefficients[1] + model$coefficients[2] + log(fu) # verifying that pred below includes follow-up
pred = predict(model, newdata = newdata, se.fit = TRUE) # this does include follow-up
newdata = mutate(newdata,
                 mean = pred$fit,
                 z = qnorm(0.975),
                 se = pred$se.fit,
                 lower = mean - (z*se),
                 upper = mean + (z*se),
                 mean = exp(mean),
                 lower = exp(lower),
                 upper = exp(upper))
newdata

# adjusted model; some reduction in dispersion
modela = glm(citations ~ review_available + log2(n_authors) + journal, offset=log(follow_up), data = data, family=quasipoisson())
summary(modela)


## using approach by Cheng et al who found opposite association between open review and citations (https://doi.org/10.1016/j.joi.2024.101540)
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