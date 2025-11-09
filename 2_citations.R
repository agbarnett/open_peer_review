# 2_citations.R
# model of citation counts dependent on open peer review
# Sep 2025
library(dplyr)
library(contrast)

# get data, from 1_process.R
load("data/1_processed.RData")
# make follow-up time to offset citation counts
date.searched = as.Date('2025-09-01')
data = mutate(data, follow_up = as.numeric(date.searched - published))

# follow-up time as offset, over-dispersion is needed
model = glm(citations ~ review_available, offset=log(follow_up), data = data, family=quasipoisson())
summary(model)
#
# more residual checks to come ... 
hist(resid(model))

# estimate numbers at 3 years
newdata = data.frame(follow_up = c(365.25*3,365.25*3), review_available = c(TRUE, FALSE) )
pred = predict(model, newdata = newdata, se.fit = TRUE)
newdata = mutate(newdata,
                 mean = pred$fit,
                 z = qnorm(0.975),
                 se = pred$se.fit,
                 lower = mean - (z*se),
                 upper = mean + (z*se),
                 mean = exp(mean),
                 lower = exp(lower),
                 upper = exp(upper))
# contrast for difference, not that useful
open_review =
  contrast(model, 
           list(review_available = FALSE),
           list(review_available = TRUE)
  )
print(open_review, X = TRUE, fun = exp)


# adjusted model
modela = glm(citations ~ review_available + log2(n_authors) + journal, offset=log(follow_up), data = data, family=quasipoisson())
summary(modela)
