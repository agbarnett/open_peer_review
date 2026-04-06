# 5_table_stability.R
# tabulate the estimates from the model based on the stability selection
# December 2025
library(stringr)
library(dplyr)
library(xtable) # for latex
library(ggplot2)

# load the results
load('results/4_ests_stability.RData') # from 4_stability_selection.R

#
to_table = filter(ests, !str_detect(term, 'Intercept|published$|orcid')) %>%
  mutate(term = str_remove(term, '^x_selected'),
         group = str_extract(term, '^subject_|^country_|^domain_|^type|^funder_'),
         group = str_remove(group, '_$'),
         group = str_to_title(group), # first letter capital
         group = ifelse(group == 'Domain', 'Email domain', group), # nicer name
         group = ifelse(group == 'Type', 'Article type', group), # nicer name
         term = str_remove(term, '^subject_|^country_|^domain_|^type|^funder_'),
         term = str_replace_all(term, '_', ' '),
         term = str_replace_all(term, ' and ', ' & '), # to save space
         # add funders (from http://dx.doi.org/10.13039/)
         term = str_replace(term, '501100000781', 'European Research Council'),
         term = str_replace(term, '501100000265', 'Medical Research Council'),
         # add narrower intervals
         crit = qnorm(1-(0.50/2)), # critical value
         lower_50 = estimate - (std.error*crit),
         upper_50 = estimate + (std.error*crit),
         # add numbers:
         term = paste(term, '\n(n=', str_squish(format(n,big.mark = ',')), ')', sep = '')
  ) %>%
  select(-crit) %>%
  arrange(group, desc(estimate)) 

## tabulate the parameter estimates
to_latex = mutate(to_table,
                  estimate = round(estimate*100)/100,
                  conf.low = round(conf.low*100)/100,
                  conf.high = round(conf.high*100)/100,
                  cell = paste(estimate, ' (', conf.low, ', ', conf.high, ')', sep='')) %>%
  select(group, term, cell) %>%
  mutate(cell = str_replace_all(cell, '-', '--')) # latex negative
print(xtable(to_latex, digits=2), math.style.negative=TRUE, include.rownames=FALSE, hline.after=FALSE, file = "results/5_estimates_table.tex")


## compare logistic and linear models
pred1 = fitted(small_model)
pred2 = fitted(small_model_logistic)
cor(pred1, pred2)
# bland-altman
ba = data.frame(linear = pred1, logistic = pred2) %>%
  mutate(av = (linear+logistic)/2,
         diff = linear - logistic)
#
label1 = data.frame(av = 0.01, diff = 0.009, label = 'Logistic greater')
label2 = data.frame(av = 0.01, diff = -0.009, label = 'Linear greater')
# limits of agreement
loa_lower = quantile(ba$diff, 0.025)
loa_upper = quantile(ba$diff, 1 - 0.025)
#
bplot = ggplot(data = ba, aes(x = av, y=diff))+
  geom_text(data = label1, aes(x = av, y =diff, label = label), col='grey66', adj=0)+
  geom_text(data = label2, aes(x = av, y =diff, label = label), col ='grey66', adj=0)+
  geom_point(alpha=0.75, size=0.5, pch=1, col='darkseagreen3') + # increase transparency
  geom_hline(lty=2, col='navy', yintercept=0, linewidth=0.5)+ # lines on top of dots
  geom_hline(lty=2, col='darkred', yintercept=loa_lower, linewidth=0.5)+
  geom_hline(lty=2, col='darkred', yintercept=loa_upper, linewidth=0.5)+
  theme_bw()+
  theme(panel.grid.minor=element_blank())+
  scale_x_continuous(lim=c(0,NA), expand=c(0,0))+ # start at zero
  xlab('Average of linear and logistic probability')+
  ylab('Linear minus logistic probability')
bplot
# export
ggsave(filename = 'figures/5_bland_altman_probs.jpg', bplot, width = 5, height=4, units='in', dpi=400)

# look at prediction ranges
summary(pred1)
summary(pred2)
