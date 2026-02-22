# 5_plots.R
# plot the results from the elastic net and the bootstrap
# August 2025
library(stringr)
library(dplyr)
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
source('R/find_distribution.R')

# get the results from 4_stability_selection.R
load('results/4_ests.RData')

# select the parameters at the parsimonious model threshold
best = filter(ests,
         abs(lambda - cvfit$lambda.1se) < 0.00001, 
         abs(estimate) > 0.01) # must be bigger than a 1% change

# table for estimates that are not plotted (none bar intercept)
not_plot = filter(best, !str_detect(term, '^domain|^type|^country|^subject'))

## plot types
# extract types
types = filter(best, str_detect(term, '^type')) %>%
  mutate(term = str_remove(term, '^type')) %>%
  arrange(estimate) %>%
  mutate(x = 1:n())
#
tplot = ggplot(data = types, aes(x = x, y = estimate))+
  geom_point(col = 'darkorchid3', size=2)+
  geom_hline(lty=2, yintercept=0)+
  ylab('Difference in probability of open review')+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(types), labels = types$term)+
  coord_flip()+
  g.theme
tplot
ggsave('figures/5_type.jpeg', tplot, width=4, height=4, units='in', dpi = 500)

## plot subjects
# extract
subjects = filter(best, str_detect(term, '^subject')) %>%
  mutate(term = str_remove(term, '^subject_')) %>%
  arrange(estimate) %>%
  filter(abs(estimate)>0.01) %>% # 
  mutate(x = 1:n())
splot = ggplot(data = subjects, aes(x = x, y = estimate))+
  geom_point(col = 'darkorange2', size=2)+
  geom_hline(lty=2, yintercept=0)+
  ylab('Difference in probability of open review')+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(subjects), labels = subjects$term, expand=c(0,0))+ # remove space at ends of axis
  coord_flip()+
  g.theme
splot
ggsave('figures/5_subjects.jpeg', splot, width=4, height=8, units='in', dpi = 500)

## plot countries
# extract countries
countries = filter(best, str_detect(term, '^country')) %>%
  mutate(term = str_remove(term, '^country_')) %>%
  arrange(estimate) %>%
  mutate(x = 1:n())
#
cplot = ggplot(data = countries, aes(x = x, y = estimate))+
  geom_point(col = 'dodgerblue', size=2)+
  geom_hline(lty=2, yintercept=0)+
  ylab('Difference in probability of open review')+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(countries), labels = countries$term, expand=c(0.01,0.01))+
  coord_flip()+
  g.theme
cplot
ggsave('figures/5_country.jpeg', cplot, width=4, height=4, units='in', dpi = 500)

## look at uncertainty in best model from bootstrap
# 1) histogram of numbers of selected variables
numbers = group_by(boot_ests, boot) %>%
  summarise(n_vars = n())
#
hplot = ggplot(data = numbers, aes(x = n_vars))+
  geom_histogram(col='grey88', fill='darkseagreen3')+
  g.theme+
  xlab('Total number of variables in elastic net selection')
hplot
ggsave('figures/5_bootstrap_elastic_net_variable_numbers.jpeg', hplot, width=4, height=4, units='in', dpi = 500)

# 2) what variables were most commonly selected; plus the average size
size = group_by(boot_ests, term) %>%
  summarise(n = n(),
          mean = mean(estimate)) %>%
  arrange(desc(n))
vplot = ggplot(data = size, aes(x = n))+
  geom_histogram(col='grey88', fill='goldenrod2')+
  g.theme+
  xlab('Number of times individual variables were selected')
vplot
summary(size$n)
# mean effect size
eplot = ggplot(data = size, aes(x = mean))+
  geom_histogram(col='grey88', fill='yellow3')+
  g.theme+
  xlab('Mean effect size')
eplot

# number of variables that were selected more than 50 times out of 100, with an average over 0.01
candidates = filter(size, n>=50, abs(mean)>0.01) 
nrow(candidates)
# plot size against selection
splot = ggplot(data = candidates, aes(x = n, y = mean))+
  geom_point(col = 'dodgerblue')+
  g.theme+
  xlab('Number of times individual variables were selected')+
  ylab('Mean effect size')
splot

# to estimate standard deviation and degrees of freedom for Bayesian model
for_sd = filter(size, n>=50, !str_detect(term, "Intercept")) # do not have size restriction; remove intercept as no expectation that this would be zero
what_t = find_distribution(x = for_sd$mean, df_range = 1:20)
# plot findings
tplot = ggplot(data = what_t, aes(x=df, y=WAIC))+
  geom_point(col='brown3', size=2)+
  geom_line(col='brown3', linewidth=1.05)+
  g.theme+
  xlab('Degrees of freedom for t-distribution')+
  ylab('WAIC')
tplot
ggsave('figures/5_best_t_distribution.jpeg', tplot, width=4.2, height=3.7, units='in', dpi = 500)

# save the list of candidates for use in the Bayesian model
save(candidates, what_t, file = 'results/5_candidates.RData')
filter(candidates, !str_detect(term, '^domain|^type|^country|^subject'))
