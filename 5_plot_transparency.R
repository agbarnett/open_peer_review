# 5_plot_transparency.R
# plot country estimates against transparency index
# downloaded on 1 September 2025
# September 2025
library(dplyr)
library(ggplot2)
library(rvest)
library(janitor)
library(stringr)
library(ggrepel)

## wikipedia page with 2024 data
url = 'https://en.wikipedia.org/wiki/Corruption_Perceptions_Index'
html <- html <- html <- read_html(url)
# extract the table from the html
table = html %>% 
  html_nodes('body') %>% 
  html_nodes("table[class='mw-datatable wikitable sortable']") %>% 
  html_table()
table = table[[1]] %>%
  clean_names() %>%
  rename('country' = 'nation_or_territory')
# fix one error
table = mutate(table, 
               score = ifelse(country == 'Denmark', '90', score),
               score = as.numeric(score)) %>%
  select(-rank_change) %>%
  rename('rank' = 'number')

## prepare elastic net results
load('results/4_ests.RData')
# select the parameters at the parsimonious model threshold
best = filter(ests,
              abs(lambda - cvfit$lambda.1se) < 0.00001,
              str_detect(term, '^country_')) # just get country estimates
# add country column
best = mutate(best, 
              country = str_remove(term, '^country_'),
              country = case_when( # changes to keep consistent country names
                country =='Republic of Korea' ~ 'South Korea',
                country =='United States of America' ~ 'United States',
                TRUE ~ as.character(country) # otherwise
              ))

## merge with estimates
merged = full_join(table, best, by = 'country') %>%
  filter(!is.na(score), !is.na(estimate))

## plot
plot = ggplot(data = merged, aes(x=estimate, y=score, label = country))+
  geom_point(size=2, col='darkred')+
  geom_text_repel(size=2)+
# scale_y_continuous(limits = c(7,NA))+ # to reduce squash
  theme_bw()+
  xlab('Difference in open peer review probability')+
  ylab('Transparency score')
plot
with(merged, cor(estimate, score, use='complete.obs'))
# non-linear pattern? C-shaped pattern? Use that score...
#
ggsave('figures/5_transparency_scatter.jpeg', plot, width=4.6, height=4.6, units='in', dpi = 500)

