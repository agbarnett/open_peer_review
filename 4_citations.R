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
source('4_data_prepare_citations.R')

### part 1: unadjusted model
# follow-up time as offset, over-dispersion is needed; scaled offset to a year
model = glm(citations ~ review_available, 
            offset = log(follow_up), 
            data = data, 
            family = quasipoisson())
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


### part 2: adjusted model (takes a while)
# variables decided based on those likely to impact citations 
# parameter heavy model
model_adjusted = glm(citations ~ review_available +
              published + # date
              time_between + # peer review time
               n_authors + # number of authors
               subject_mat + # subject
              country_mat, # country
            offset = log(follow_up), 
            data = data, 
            family = quasipoisson())
summary(model_adjusted)
ests_adjusted = tidy(model_adjusted, conf.int=TRUE, exponentiate = TRUE) # takes a while

# store the results
save(model, model_adjusted, ests_adjusted, file='results/4_citation_models.RData')
