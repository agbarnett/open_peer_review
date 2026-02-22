# 5_plot_date.R
# plot the continuous estimate for date published
# called by 5_plot_stability.R
# November 2025

# create data for predictions
n_predict = 50
new_data = matrix(data = 0, nrow = n_predict, ncol = small_model$rank)
new_data[,1] = 0 # intercept on = 1 or off = 0 for all
# get range of dates for predictions, then standardised
dstart = as.numeric(as.Date('2019-07-01'))
dend = as.numeric(as.Date('2025-07-18'))
dates = round(seq(dstart, dend, length.out=n_predict))
stan_dates = (as.numeric(dates) - 18000)/365.25 # scale first to remove huge numbers (as per 2_stability_selection.R)
stan_dates = stan_dates ^-1 # best fractional polynomial
stan_dates = (stan_dates - mean_published)/sd_published # further scale because of lasso, see 3_data_prepare.R
index = which(x_selected_names == 'published')
new_data[,index+1] = stan_dates # only change one column in X (plus 1 for intercept)
# get predictions using matrix multiplication (because glm was fit using matrix)
vcov = vcov(small_model)
vests = as.vector(small_model$coefficients)
mean = new_data%*%vests
var = diag(new_data%*%vcov%*%t(new_data))
pred = data.frame(published = new_data[,2], mean = mean, var = var, dates = dates) %>%
  mutate(z = qnorm(0.975),
         lower = mean - z*sqrt(var),
         upper = mean + z*sqrt(var))
# date labels 
year_labels = 2019:2025
year_start = as.numeric(as.Date(paste(year_labels,'-01-01', sep='')))
#
colour = 'orange1'
rplot = ggplot(data = pred, aes(x=dates, y= mean, ymin=lower, ymax=upper))+
  geom_line(col=colour, linewidth=1.05)+
  geom_hline(lty=3, yintercept=0)+
  geom_ribbon(alpha = 0.5, fill=colour)+
  scale_x_continuous(breaks = year_start, labels = year_labels)+
  xlab('Date')+
  ylab('Difference in the probability of choosing open review')+
  g.theme
rplot
