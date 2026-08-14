# 4_survival_retractions.R
# survival models for time to retraction
# added separate analysis for retraction and expression of concern
# September 2026
library(dplyr)
library(stringr)
library(survival)

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# prepare the data
source('4_data_prepare_retractions.R')
# get the DOIs from authors with many retractions for a sensitivity analysis
load('data/99_heavy_retractions.RData') # from 99_find_repeat_retractions.R

## run the KM survival model
# i) retraction and expression of concern
smodel = survfit(Surv(time_to_retraction, retracted) ~ open_review, data = data)
summary(smodel)
# ii) retraction only
smodel_ret_only = survfit(
  Surv(time_to_retraction, event = ret_type == 'retracted') ~ open_review,
  data = data
)
# iii) concern only
smodel_eoc_only = survfit(
  Surv(time_to_retraction, event = ret_type == 'concern') ~ open_review,
  data = data
)

## Cox models
# i) retraction and expression of concern
cmodel = coxph(
  Surv(time = time_to_retraction, event = retracted) ~ open_review,
  data = data
)
summary(cmodel)
# needed for text: total follow-up time (years)
cat(
  'Total follow-up time',
  format(round(sum(data$time_to_retraction)), big.mark = ','),
  '\n'
)
# needed for text: average follow-up time (years)
cat(
  'Average follow-up time',
  round(10 * sum(data$time_to_retraction) / nrow(data)) / 10,
  '\n'
)

# ii) retraction only (concerns censored)
cmodel_ret_only = coxph(
  Surv(
    time = time_to_retraction,
    event = ret_type == 'retracted'
  ) ~ open_review,
  data = data
)
# iii) expression of concern only (retractions censored)
cmodel_eoc_only = coxph(
  Surv(time = time_to_retraction, event = ret_type == 'concern') ~ open_review,
  data = data
)

# retraction numbers
group_by(data, open_review, retracted) %>% tally()

# sensitivity analysis without authors with more than 10 retractions
cmodel_ret_only_without_10 = coxph(
  Surv(
    time = time_to_retraction,
    event = ret_type == 'retracted'
  ) ~ open_review,
  data = filter(data, !doi %in% frequent_retractions)
)


## model with time-dependent coefficient (see survival/vignettes/timedep.pdf)
# use retractions only as outcome
# explode the data over all key times (over 45 million rows!)
ret_times <- filter(data, ret_type == 'retracted') %>%
  select(time_to_retraction) %>%
  unique() %>%
  pull(time_to_retraction)
exploded <- survSplit(
  Surv(time = time_to_retraction, event = ret_type == 'retracted') ~ ., # retractions only
  data = select(
    data,
    'doi',
    'open_review',
    'time_to_retraction',
    'ret_type',
    'retracted'
  ), # getting errors otherwise, so just keep key variables
  cut = ret_times,
  episode = "timegroup"
)
exploded = mutate(
  exploded,
  xtime = time_to_retraction,
  open_reviewn = as.numeric(open_review == 'Yes')
)
cmodel_time = coxph(
  Surv(
    tstart,
    time_to_retraction,
    event
  ) ~ open_reviewn + # can now just use `event` as this is carried forward from above
    open_reviewn:xtime, # using start and stop
  data = exploded
)
summary(cmodel_time)
# predictions (done on HPC)
pred_time = data.frame(
  open_reviewn = 1,
  xtime = c(0.01, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)
)
pred = predict(cmodel_time, newdata = pred_time, se.fit = TRUE)
pred_time = mutate(
  pred_time,
  mean = pred$fit, #
  se = pred$se.fit,
  z = qnorm(0.975),
  lower = mean - (z * se),
  upper = mean + (z * se)
)

### sensitivity analysis, stratify on year (vary baseline hazard)
# no change
ymodel = coxph(
  Surv(time = time_to_retraction, event = retracted) ~ review_available +
    strata(year),
  data = data
)
summary(ymodel)

# mean follow-up by group (almost no difference)
group_by(data, review_available) %>%
  summarise(n = n(), mean = mean(time_to_retraction))


### Adjusted model
# variables decided based on those likely to impact retractions, takes a while
cmodel_adjusted = coxph(
  Surv(time_to_retraction, retracted) ~ open_review +
    published + # date
    time_between + # peer review time
    n_authors + # number of authors
    subject_mat + # subject
    country_mat, # country
  data = data
)
summary(cmodel_adjusted)
# some variables with infinite SE, hence using lasso instead

# store the results
save(
  smodel,
  smodel_ret_only,
  smodel_eoc_only,
  cmodel,
  cmodel_ret_only,
  cmodel_eoc_only,
  cmodel_ret_only_without_10,
  cmodel_adjusted,
  cmodel_time,
  ymodel,
  pred_time,
  file = 'results/4_retraction_models.RData'
)
