# 4_bayes_model.R
# fit a Bayes model with a tight prior to create shrinkage using the selected variables from the elastic net
# moved to aqua (HPC)
# August 2025
library(nimble)
library(dplyr)
library(stringr)
library(tidyr) # for separate
source('R/extract_nimble.R') # for table estimates

# get the data, from 1_process.R
load('data/1_processed.RData')
# prepare the data, including standardise continuous variables
source('2_data_prepare.R')

# get the elastic net results using the bootstrap, from 3_plots.R
load('results/3_candidates.RData')
# get the best df and sigma for the t-distribution
sd.beta = filter(what_t, diff == 0) %>% pull(mean_sd)
tau.beta = 1/sqrt(sd.beta)
df.beta = filter(what_t, diff == 0) %>% pull(df)

# remove small estimates to save on computation load and remove associations unlikely to have an impact
## get variable names
# a) subjects
subjects_to_include = filter(candidates, str_detect(term, 'subject_')) %>%
  pull(term) %>%
  str_remove('subject_') 
subject_mat <- t(+sapply(data$subjects, "%in%", x = subjects_to_include)) # takes a while
if(ncol(subject_mat) != length(subjects_to_include)){cat('Check missing variables.\n')}
# b) countries
countries_to_include = filter(candidates, str_detect(term, 'country_')) %>%
  pull(term) %>%
  str_remove('country_') 
country_mat <- t(+sapply(data$country, "%in%", x = countries_to_include)) # takes a while
if(ncol(country_mat) != length(countries_to_include)){cat('Check missing variables.\n')}
# c) types
types_to_include = filter(candidates, str_detect(term, '^type')) %>%
  pull(term)
type_mat <- model.matrix(~ -1 + type, data = data)
cnames = colnames(type_mat) # clean up names to match those used by 3_plots.R
cnames = str_replace_all(cnames, ' ', '_') 
cnames = str_remove_all(cnames, "'")
cnames = str_remove_all(cnames, "-")
colnames(type_mat) = cnames
type_mat = type_mat[,colnames(type_mat) %in% types_to_include]
if(ncol(type_mat) != length(types_to_include)){cat('Check missing variables.\n')}
# d) domain
domains_to_include = filter(candidates, str_detect(term, '^domain_')) %>%
  pull(term) %>%
  str_remove('domain_') 
edomain_mat <- t(+sapply(data$edomain, "%in%", x = domains_to_include))
if(ncol(edomain_mat) != length(domains_to_include)){cat('Check missing variables.\n')}
# e) other variables
other = filter(candidates, !str_detect(term,'^type|subject_|country_|domain_|Intercept')) %>%pull(term)
other_mat = select(data, all_of(other)) %>%
  as.matrix()
# combine all variables into one X matrix
X = cbind(other_mat) %>%
  cbind(subject_mat) %>% 
  cbind(country_mat) %>% 
  cbind(type_mat) %>%
  cbind(edomain_mat) %>%
  as.matrix()
# check we have all parameters, minus 1 for intercept
if((nrow(candidates)-1) != ncol(X)){cat('Warning, missing variables\n')}

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    open[i] ~ dnorm(mu[i], tau)
    mu[i] <- intercept + inprod(beta[1:M], X[i,1:M])
  }
  # 
  for(j in 1:M){ # using a t-distribution with an informative prior
    beta[j] ~ dt(mu = 0, tau = tau.beta, df = df.beta)
  }
  intercept ~ dbeta(1, 1) # vague prior between [0,1]
  tau ~ dgamma(0.1, 0.1) # vague prior
  sd <- 1/sqrt(tau)
})

## data
M = ncol(X)
constants <- list(N = nrow(data),
                  df.beta = df.beta, # found by fitting t-distribution, see 3_plots.R
                  tau.beta = tau.beta, # fixed; found by fitting t-distribution, see 3_plots.R
                  M = M,
                  X = X)
y_data <- list(open = as.numeric(data$review_available))

## initial values, use estimates from elastic net (X-matrix order is subject, country, type, domain)
beta_start_country = filter(candidates, str_detect(term, '^country')) %>% pull(mean)
beta_start_subject = filter(candidates, str_detect(term, '^subject')) %>% pull(mean)
beta_start_type = filter(candidates, str_detect(term, '^type')) %>% pull(mean)
beta_start_domain = filter(candidates, str_detect(term, '^domain')) %>% pull(mean)
beta_start_other = filter(candidates, term %in% other) %>% pull(mean)
beta_start = c(beta_start_other, beta_start_subject, beta_start_country, beta_start_type, beta_start_domain) # match order of X matrix
if(length(beta_start) != ncol(X)){cat('Missing initial values.\n')}
inits <- list(intercept = filter(candidates, str_detect(term, 'Intercept')) %>% pull(mean),
              tau = 1/(sd.beta^2), # from t-distribution
              beta = beta_start)

# parameters to store
parms = c('intercept','beta','sd')

# models
model <- nimbleModel(code, 
                     data = y_data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
source('99_mcmc.R')
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds,
                       WAIC = FALSE)

# get summary table
table = extract_nimble(mcmc_out)

# quick check of convergence, plot chain for intercept
plot_it = FALSE
if(plot_it==TRUE){
  index = which(colnames(mcmc_out$samples$chain1) == 'intercept')
  plot(mcmc_out$samples$chain1[,index])
  acf(mcmc_out$samples$chain1[,index])
}

# save estimates
save(table, mcmc_out, MCMC, thin, n.chains, file = 'results/3_model_estimates.RData')
