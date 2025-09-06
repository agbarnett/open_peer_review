# 2_projection_predictive_selection.R
# projection predictive variable selection
# see https://avehtari.github.io/modelselection/bodyfat.html
# September 2025
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(loo)
library(projpred)
library(dplyr)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(stringr)
SEED = TeachingDemos::char2seed('reading')

# get data, from 1_process.R
load("data/1_processed.RData")
# prepare the data
source('2_data_prepare.R')

# make formula, `pred` comes from 2_data_prepare.R
outcome <- "review_available"
formula <- as.formula(paste(outcome, " ~ ", paste(pred[1:400], collapse = " + ")))
p <- length(pred)

# regression model with regularized horseshoe prior
n <- nrow(data)
p0 <- 25 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
rhs_prior <- hs(global_scale = tau0)
data_for_stan = bind_cols(data$review_available, x)
names(data_for_stan)[1] = outcome
fitrhs <- stan_glm(formula, data = data_for_stan, prior=rhs_prior, QR=TRUE, 
                   seed = SEED, refresh = 0)
summary(fitrhs)
# model is crashing!
