# 3_plot_elastic_net.R
# plot the elastic net results
# see https://broom.tidymodels.org/reference/tidy.cv.glmnet.html
# August 2025
library(ggplot2)
library(broom)
library(dplyr)

# get the results from 2_elastic_net_model.R
load('results/2_ests.RData')

## plot of MSE as a function of lambda
tidied_cv <- tidy(cvfit)
glance_cv <- glance(cvfit)
# add non-zero numbers along top
tidied_cv = mutate(tidied_cv, top = max(estimate), step=1:n())
for_label = select(tidied_cv, lambda, top, nzero, step) %>%
  mutate(diffl = c(0,diff(lambda)),
         diff = c(0,diff(nzero))) %>%
  filter(step%%6 == 0)
#
g <- ggplot(tidied_cv, aes(lambda, estimate)) +
  geom_line(col='navy') +
  scale_x_log10()+
  ylab('Mean square error')+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill='navy', alpha = .25)+
  geom_vline(xintercept = glance_cv$lambda.min, col='grey55') +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2, col='grey55') + 
  geom_text(data = for_label, aes(x = lambda, y = top+0.0005, label = nzero), col='darkgoldenrod', angle=45, size = 3)+
  theme_bw()
g
#
ggsave('figures/3_elastic_net_cv.jpg', width=4.2, height=4, units='in', dpi=500)

## plot of number of zeros for each choice of lambda
nz = ggplot(tidied_cv, aes(lambda, nzero)) +
  geom_line() +
  scale_x_log10()
nz

## coefficient plot with min lambda shown - not working
tidied <- tidy(cvfit$glmnet.fit)
eplot = ggplot(tidied, aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line() +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)+
  theme_bw()
eplot

