# 4_plot_orcid.R
# plot the continuous estimate for ORCID proportion
# called by 4_plot_stability.R
# November 2025

# create data for predictions
n_predict = 50
new_data = matrix(data = 0, nrow = n_predict, ncol = small_model$rank) # start with blank matrix
new_data[,1] = 0 # intercept on = 1 or off = 0 for all
# get range of ORCID
p_orcid = seq(0.01, 1, length.out = n_predict)
poly_orcid = p_orcid^2 # best fractional polynomial
poly_orcid = (poly_orcid - mean_orcid)/sd_orcid # scale because of lasso; same scaling as data
index = which(x_selected_names == 'p_orcid')
new_data[,index+1] = poly_orcid # only change one column in X (plus 1 for intercept)
# get predictions using matrix multiplication (because glm was fit using matrix)
vcov = vcov(small_model)
vests = as.vector(small_model$coefficients)
mean = new_data%*%vests
var = diag(new_data%*%vcov%*%t(new_data))
pred = data.frame(orcid = p_orcid, mean = mean, var = var) %>%
  mutate(z = qnorm(0.975),
         lower = mean - z*sqrt(var),
         upper = mean + z*sqrt(var))
#
colour = 'darkseagreen3'
oplot = ggplot(data = pred, aes(x=orcid, y= mean, ymin=lower, ymax=upper))+
  geom_line(col=colour, linewidth=1.05)+
  geom_hline(lty=3, yintercept=0)+
  geom_ribbon(alpha = 0.5, fill=colour)+
  scale_x_continuous(breaks = seq(0,1,0.2))+
  xlab('Proportion of authors with an ORCID')+
  ylab('Difference in the probability of choosing open review')+
  g.theme
oplot
