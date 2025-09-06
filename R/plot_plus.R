# plot_plus.R
# plot the Bayes and elastic net results together
# August 2025

plot_plus = function(elastic, # elastic net estimates
                     estring, 
                     bayes_table, # summary results from Bayesian model
                     index, 
                     expand = 0.012, # size to expand y-axis
                     acolors, 
                     colour_names=FALSE) # use coloured names in y-axis
  {
  # pull out the elastic net results
  eres = filter(elastic, str_detect(term, estring)) %>%
    mutate(term = str_remove(term, estring),
           term = str_remove(term, '^_'),
           link = 1:n())
  # bayes results
  bayes = filter(bayes_table, row %in% index) %>%
    mutate(var = str_remove(var, estring),
           link = 1:n())
  # merge two results
  to_plot = full_join(eres, bayes, by='link') %>%
    arrange(mean) %>% # plot by mean
    mutate(x = 1:n())
  # check merge
  check = filter(to_plot, var != term) 
  if(nrow(check)!=0){stop('Merge problem.\n')}
  
  # filter for high probability of non-zero
  to_plot = filter(to_plot, pvalue < 0.01) %>%
    mutate(xnew = 1:n()) %>%
    select(-x)
  
  # alternating colours
  aacolors = rep(acolors, times = nrow(to_plot))
  aacolors = aacolors[1:nrow(to_plot)]
  # plot
  cplot = ggplot(data = to_plot, aes(x = xnew, y = mean, ymin = lower, ymax = upper))+
    geom_point(col = aacolors, size=2)+
    geom_point(data = to_plot, aes(x=xnew, y=estimate), col='black', pch=1)+
    geom_hline(lty=2, yintercept=0)+
    geom_errorbar(width=0, col= aacolors)+
    ylab('Difference in probability of open review')+
    xlab(NULL)+
    scale_x_continuous(breaks = 1:nrow(to_plot), 
                       labels = to_plot$var,
                       expand = c(expand, expand))+
    coord_flip()+
    g.theme
  # add colours to names
  if(colour_names == TRUE){
    cplot = cplot + 
      scale_x_continuous(breaks = 1:nrow(to_plot),
                       expand = c(0.012,0.012),
                       labels = function(x, text = to_plot$var) {
                         col <- ifelse(x%%2!=0 , acolors[1], acolors[2])
                         lab <- text
                         glue::glue("<span style = 'color:{col}'>{lab}</span>")
                       })+
      theme(axis.text.y = element_markdown(size=8, hjust=1, vjust=0.5)) # to make html work
  }
  #
  return(cplot)
}

