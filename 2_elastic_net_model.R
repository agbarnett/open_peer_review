# 2_elastic_net_model.R
# use glmnet (elastic net) to find predictors of open peer review
# September 2025
library(naniar) # missing summary
library(dplyr)
library(glmnet)
library(broom)
library(stringr)
TeachingDemos::char2seed("exeter")

# get data, from 1_process.R
load("data/1_processed.RData")

## check missing
miss_case_table(data)
miss_var_summary(data)

# find best non-linear association for continuous variables
source('2_fractional_polynomial.R')
# prepare the data
source('2_data_prepare.R')

# use normal model for binary outcome as sample size allows
y <- data$review_available
model <- glmnet(y = y, x = x, family = "gaussian", alpha = 0.95)
plot(model)
cvfit <- cv.glmnet(y = y, x = x, family = "gaussian", alpha = 0.95, n.folds = 10)
plot(cvfit)
glance(cvfit)
ests <- tidy(model)

## Bootstrap to get uncertainty in variable selections (takes a while)
B = 100
boot_ests = NULL
TeachingDemos::char2seed("andorra")
for (k in 1:B){
  index = sample(1:nrow(x), size = nrow(x), replace = TRUE)
  y_boot = y[index]
  x_boot = x[index,]
  model_boot <- glmnet(y = y_boot, x = x_boot, family = "gaussian", alpha = 0.95)
  cvfit_boot <- cv.glmnet(y = y_boot, x = x_boot, family = "gaussian", alpha = 0.95, n.folds = 10)
  ests_boot <- tidy(model_boot)
  best_boot = filter(ests_boot,
                abs(lambda - cvfit_boot$lambda.1se) < 0.00001) %>%
    mutate(boot = k)
  boot_ests = bind_rows(boot_ests, best_boot)
  cat('Boot number = ', k, '\r', sep='')
}

## save
save(ests, cvfit, boot_ests, file = "results/2_ests.RData")
