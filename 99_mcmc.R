# 99_mcmc.R
# options for chains
# August 2025
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('Orient')
seeds[2] = TeachingDemos::char2seed('Reading')
MCMC = 1000
n.chains = 2
thin = 3