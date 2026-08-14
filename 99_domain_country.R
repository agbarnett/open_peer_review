# 99_domain_country.R
# investigate a link between email domain and country
# August 2026
library(stringr)
library(dplyr)
library(xtable)

# need to run to get mean and SD used for standardisation
load('data/3_plus_experience.RData')
source('4_data_prepare.R')

## look at top users of edu and gmail
# loop through countries
cnames = str_remove(country_mat_names, 'country_')
dnames = str_remove(domain_mat_names, 'domain_')
all_stats = NULL
for (c in 1:ncol(country_mat)) {
  index = which(country_mat[, c] == 1)
  stats = data.frame(
    country = cnames[c],
    domain = dnames,
    n = colSums(edomain_mat[index, ]),
    means = colMeans(edomain_mat[index, ])
  )
  all_stats = bind_rows(all_stats, stats)
}

#
top_10_edu = filter(all_stats, n > 100, domain == 'edu') %>%
  arrange(desc(means)) %>%
  slice(1:10)
#
top_10_gmail = filter(all_stats, n > 100, domain == 'gmail') %>%
  arrange(desc(means)) %>%
  slice(1:10)
for_table = bind_cols(top_10_gmail, top_10_edu) %>%
  mutate(
    means...4 = round(means...4 * 100),
    means...8 = round(means...8 * 100)
  ) %>%
  select(-contains('domain'))

# top rank in each country
per_country = group_by(all_stats, country) %>%
  arrange(desc(means)) %>%
  slice(1) %>%
  ungroup()

# export to latex
print(
  xtable(for_table, digits = 0),
  math.style.negative = FALSE,
  include.rownames = FALSE,
  hline.after = FALSE,
  file = "results/99_email_domains.tex"
)
