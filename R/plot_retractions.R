# plot_retractions.R
# function to plot retractions / expressions of concern
# August 2026

plot_retractions = function(inmodel, indata, xlab, ptime = 6.5, vjust) {
  # adjustment for labels
  if(is.null(vjust)){ 
    vjust = c(-2, 1.5)
  }
  splot = ggsurvplot(
    inmodel,
    data = indata,
    palette = cbPalette[1:2],
    conf.int = TRUE, # Add confidence interval
    xlab = xlab,
    ylab = 'Cumulative incidence',
    xlim = c(0, 6.5), # avoid white space
    censor.size = 0, # suppress censoring ticks
    ncensor.plot = FALSE,
    legend.labs = c("No", 'Yes'),
    legend.title = 'Open review',
    fun = 'event', # reverse curve to plot events
    risk.table = FALSE
  )
  # move legend inside
  lplot = splot$plot +
    theme(legend.position = "inside", legend.position.inside = c(0.19, 0.82))
  # add text labels
  end = summary(inmodel, times = ptime) # can't do max time as it's not available for yes group
  label = data.frame(
    time = ptime - 0.1,
    rate = 1 - end$surv,
    label = paste('n = ', end$n.event, sep = '')
  ) 
  lplot_black = lplot +
    theme(plot.margin = unit(c(0, 1.2, 0, 0), "cm")) +
    geom_text(
      data = label,
      aes(x = time, y = rate, label = label), adj=0, vjust = vjust # adjust labels; really needed for concern
    ) +
    coord_cartesian(clip = "off")

  return(lplot_black)
} # end of function
