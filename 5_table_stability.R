# 5_table_stability.R
# tabulate the estimates from the model based on the stability selection
# December 2025
library(stringr)
library(dplyr)
library(xtable) # for latex

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
