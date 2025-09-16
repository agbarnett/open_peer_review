# 3_plot_stability.R
# plot the estimates from the stability selection
# September 2025
library(xtable) # for latex
library(stringr)
library(dplyr)
library(ggplot2)
library(ggforce) # for better spaced facet_wrap
g.theme = theme_bw()+ theme(panel.grid.minor = element_blank())
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "yellow3", "#0072B2", "#D55E00", "#CC79A7")

# load the results
load('results/2_ests_stability.RData') # from 2_stability_selection.R

### Plot 1: plot selected variables ###
for_plot = data.frame(stab.lasso$max) %>%
  tibble::rownames_to_column() %>%
  arrange(desc(stab.lasso.max)) 
# exclude small probabilities, otherwise it will not fit in plot area
n_small = filter(for_plot, stab.lasso.max < 0.25) %>% nrow()
selected = names(stab.lasso$selected)
for_plot = filter(for_plot, stab.lasso.max >= 0.25) %>%
  mutate(x = 1:n(),
         selected = rowname %in% selected) 
# labels
labels = str_replace(for_plot$rowname, '^type', 'type_') # type did not start with _
labels = str_replace(labels, '_', ' = ') # replace first underbar with equals ...
labels = str_replace_all(labels, '_', ' ') # ... and then remaining with space
labels = str_replace_all(labels, 'published', 'Date published') # better label

#
colours = c("coral3", "yellow3")
pplot = ggplot(data = for_plot, aes(x = x, y = stab.lasso.max, col = selected))+
  geom_point(size = 3)+
  scale_x_continuous(breaks = 1:nrow(for_plot), labels = labels, expand=c(0.01,0.01))+
  scale_y_continuous(breaks = seq(0.25,1,0.25))+
  scale_color_manual(NULL, values = colours)+
  xlab(NULL)+
  ylab('Selection probability')+
  geom_hline(yintercept = stab.lasso$cutoff, col = 'darkred', lty=2)+
  coord_flip()+
  g.theme +
  theme(legend.position = 'none')
pplot
# export
ggsave('figures/3_stability_selection.jpg', pplot, width = 4.9, height=5.2, units='in', dpi=500)
cat('There were ', filter(for_plot, stab.lasso.max==1)%>%nrow(), ' variables that were selected in all 100 bootstrap samples.\n', sep='')

### Plot 2: plot categorical estimates ###
to_plot = filter(ests, !str_detect(term, 'Intercept|published$')) %>%
  mutate(term = str_remove(term, '^x_selected'),
         group = str_extract(term, '^subject_|^country_|^domain_|^type'),
         group = str_remove(group, '_$'),
         group = str_to_title(group), # first letter capital
         group = ifelse(group == 'Domain', 'Email domain', group), # nicer name
         group = ifelse(group == 'Type', 'Article type', group), # nicer name
         term = str_remove(term, '^subject_|^country_|^domain_|^type'),
         term = str_replace_all(term, '_', ' '),
         term = str_replace_all(term, ' and ', ' & '), # to save space
         # add numbers:
         term = paste(term, '\n(n=', str_squish(format(n,big.mark = ',')), ')', sep = '')
         ) %>%
  arrange(group, desc(estimate)) %>%
  mutate(x = 1:n())
# plot
expand = 0.4
cplot = ggplot(data = to_plot, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, col=group))+
  geom_point(size=2)+
  geom_hline(lty=2, yintercept=0)+
  geom_errorbar(width=0, linewidth=1.05)+
  ylab('Difference in the probability of choosing open review')+
  xlab(NULL)+
  scale_color_manual(NULL, values = cbbPalette)+
  scale_x_continuous(breaks = 1:nrow(to_plot), 
                     labels = to_plot$term,
                     expand = expansion(mult = 0, add = expand))+ # using add for consistent gap by panels
  g.theme+
  theme(legend.position = 'none',
        strip.text.x = element_text(margin = margin(t=0.7, r=0, b=0.7, l=0, "mm")))+ # reduce facet size
  ggforce::facet_col(vars(group), scales='free', space='free')+ #
  coord_flip()
cplot
# export
ggsave('figures/3_stability_estimates.jpg', cplot, width = 5.8, height=7.2, units='in', dpi=500)

# plot country separately
to_plot_country = filter(to_plot, group == 'Country')
cplot2 = ggplot(data = to_plot_country, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, col=group))+
  geom_point(size=2)+
  geom_hline(lty=2, yintercept=0)+
  geom_errorbar(width=0, linewidth=1.05)+
  ylab('Difference in the probability of choosing open review')+
  xlab(NULL)+
  scale_color_manual(NULL, values = cbbPalette)+
  scale_x_continuous(breaks = 1:nrow(to_plot), 
                     labels = to_plot$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(legend.position = 'none')+
  coord_flip()
cplot2
# export
ggsave('figures/3_stability_country.jpg', cplot2, width = 4.7, height=4.5, units='in', dpi=500)

## alternative version to above with separate panels that are assembled using grid.arrange
# see https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap

## plot continuous published estimate ##
n_predict = 50
new_data = matrix(data = 0, nrow = n_predict, ncol = small_model$rank)
new_data[,1] = 0 # intercept on for all
# get range of dates for predictions, then standardised
dstart = as.numeric(as.Date('2019-07-01'))
dend = as.numeric(as.Date('2025-07-18'))
dates = round(seq(dstart, dend, length.out=n_predict))
stan_dates = (as.numeric(dates) - 18000)/365.25 # scale first to remove huge numbers (as per 2_stability_selection.R)
stan_dates = stan_dates ^-1 # best fractional polynomial
new_data[,2] =  stan_dates # published (second variable)
# get predictions using matrix multiplication (because glm was fit using matrix)
vcov = vcov(small_model)
ests = as.vector(small_model$coefficients)
mean = new_data%*%ests
var = diag(new_data%*%vcov%*%t(new_data))
pred = data.frame(published = new_data[,2], mean = mean, var = var, dates = dates) %>%
  mutate(z = qnorm(0.975),
         lower = mean - z*sqrt(var),
         upper = mean + z*sqrt(var))
# date labels 
year_labels = 2019:2025
year_start = as.numeric(as.Date(paste(year_labels,'-01-01', sep='')))
#
colour = 'orange2'
rplot = ggplot(data = pred, aes(x=dates, y= mean, ymin=lower, ymax=upper))+
  geom_line(col=colour, linewidth=1.05)+
  geom_ribbon(alpha = 0.5, fill=colour)+
  scale_x_continuous(breaks = year_start, labels = year_labels)+
  xlab('Date')+
  ylab('Difference in the probability of choosing open review')+
  g.theme
rplot
# export
ggsave('figures/3_date_effect.jpg', rplot, width = 5.2, height=4.2, units='in', dpi=500)

## tabulate the parameter estimates
to_latex = mutate(to_plot,
                  estimate = round(estimate*100)/100,
                  conf.low = round(conf.low*100)/100,
                  conf.high = round(conf.high*100)/100,
                  cell = paste(estimate, ' (', conf.low, ', ', conf.high, ')', sep='')) %>%
  select(group, term, cell) %>%
  mutate(cell = str_replace_all(cell, '-', '--')) # latex negative
print(xtable(to_latex, digits=2), math.style.negative=TRUE, include.rownames=FALSE, hline.after=FALSE, file = "results/3_estimates_table.tex")
