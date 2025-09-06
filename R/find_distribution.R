# find_distribution.R
# find the best fit to a t-distribution using a Bayesian approach
# September 2025

find_distribution = function(x, df_range){
  
  # run nimble model
  code <- nimbleCode({
    ## Likelihood
    for (i in 1:N){ # 
      x[i] ~ dt(0, df = df, tau = tau)
    }
    tau ~ dgamma(0.1,0.1)
    sd <- 1/sqrt(tau)
  })
  
  # loop through degrees of freedom
  fit_results = NULL
  for (df in df_range){
    
    ## data
    constants <- list(N = length(x), df = df)
    data <- list(x = x)
    
    ## initial values
    inits <- list(tau = 1)
    
    # parameters to store
    parms = c('sd')
    
    # models
    model <- nimbleModel(code, 
                         data = data, 
                         inits = inits, 
                         constants = constants)
    
    # MCMC samples
    MCMC = 2000; n.chains = 2; thin = 2
    seeds = rep(0,2)
    seeds[1] = TeachingDemos::char2seed('andorra')
    seeds[2] = TeachingDemos::char2seed('senegal')
    mcmc_out <- nimbleMCMC(model = model,
                           inits = inits,
                           monitors = parms,
                           niter = MCMC*2*thin, # times 2 for burn-in 
                           thin = thin,
                           nchains = n.chains, 
                           nburnin = MCMC,
                           summary = TRUE, 
                           setSeed = seeds,
                           WAIC = TRUE)
    this_result = data.frame(df = df, 
                             pWAIC = mcmc_out$WAIC$pWAIC,
                             WAIC = mcmc_out$WAIC$WAIC, 
                             mean_sd = mcmc_out$summary$all.chains[1])
    
    #
    fit_results = bind_rows(fit_results, this_result)
  }
  # difference from best
  fit_results = mutate(fit_results, diff = WAIC - min(WAIC))
  
  #
  return(fit_results)
}


