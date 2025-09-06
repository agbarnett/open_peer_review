# extract_nimble.R
# extract summary and add posterior p-values from nimble
# August 2025

# assumes two chains
extract_nimble = function(mcmc, n.chains=2, double_index = FALSE){
  if(n.chains!=2){stop('Only works with two chains.\n')}
  table = as.data.frame(mcmc$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) 
  if(double_index==TRUE){
    table = separate(table, col=index, into=c('row','col')) # add if there's a double index 
  }
  
  # add posterior p-values
  pos = rbind(mcmc$samples$chain1, mcmc$samples$chain2) > 0
  pos = colMeans(pos)
  pos.dash = 1 - pos
  pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC)) # two-sided, times 2 for two chains
  table = bind_cols(table, pval)
  if(double_index==FALSE){
    names(table) = c('rowname','mean','median','sd','lower','upper','row','pvalue')
  }
  if(double_index==TRUE){
    names(table) = c('rowname','mean','median','sd','lower','upper','row','col','pvalue')
  }
  #
  return(table)
}
