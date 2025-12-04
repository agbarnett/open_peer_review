# 2_citations.R
# model of citation counts dependent on open peer review
# November 2025
library(dplyr)
library(broom)
library(DHARMa) # for residuals

# get data, from 1_process.R
load("data/1_processed.RData")
# make follow-up time to offset citation counts (scaled to per year)
date.searched = as.Date('2025-09-01')
data = mutate(data, follow_up = as.numeric(date.searched - published)/365.25)

# follow-up time as offset, over-dispersion is needed; scaled offset to a year
model = glm(citations ~ review_available, offset=log(follow_up), data = data, family=quasipoisson())
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
