# 6_stability_selection_retraction.R
# stability selection for retraction model
# August 2026
library(dplyr)
library(stringr)
library(ggplot2)
library(ggtext) # for element_markdown

# need to run to get variable names
load('data/3_plus_experience.RData')
source('4_data_prepare_retractions.R')

# from HPC, 5_combine.R; 102 models
load('results/5_combine_stability_lasso.RData')
n.boot = length(unique(lasso$boot)) # should be 100, up to 102 which is fine

# get counts from 102 runs, look at top results
freq = group_by(lasso, variable) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(p = n / n.boot)
head(freq)

# how often did open peer review make it in? - to do
filter(freq, str_detect(variable, pattern = 'review'))

# add country names
country_names = str_remove_all(country_mat_names, 'country_')
subject_names = str_remove_all(subject_mat_names, 'subject_')
for_plot = filter(freq, p > 0.65) %>% # only plot those above 65% to reduce clutter
  mutate(
    selected = p > 0.75, # colour by 0.75 probability
    num = str_remove_all(variable, pattern = '[a-z]*'),
    num = as.numeric(str_remove(num, pattern = '_')),
    var = case_when(
      str_detect(variable, pattern = 'country_mat') ~ paste(
        'Country = ',
        country_names[num],
        sep = ''
      ),
      str_detect(variable, pattern = 'subject_mat') ~ paste(
        'Subject = ',
        subject_names[num],
        sep = ''
      ),
      variable == 'time_between' ~ 'Peer review time',
      variable == 'n_authors' ~ 'Number of authors',
      variable == 'published' ~ 'Date published',
    )
  ) %>%
  mutate(x = 1:n())

## has colour labels for selected variables
colours = c("coral3", "green3")
labels = for_plot$var
n_selected = sum(for_plot$selected) # number selected
pplot = ggplot(
  data = for_plot,
  aes(x = x, y = p, col = selected)
) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:nrow(for_plot),
    expand = c(0.01, 0.01),
    labels = function(x, text = to_plot$var) {
      col <- ifelse(x > n_selected, colours[1], colours[2])
      lab <- labels
      glue::glue("<span style = 'color:{col}'>{lab}</span>")
    }
  ) +
  scale_y_continuous(breaks = c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(
    'Included in\nfinal model',
    values = colours,
    labels = c('No', 'Yes')
  ) +
  xlab(NULL) +
  ylab('Selection probability') +
  coord_flip() +
  g.theme +
  theme(
    legend.margin = margin(1, 1, 1, 1),
    legend.position = 'inside',
    legend.position.inside = c(0.79, 0.88),
    legend.spacing = unit(1, 'mm'),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.text.y = element_markdown(size = 8, hjust = 1, vjust = 0.5)
  ) # to make html work
pplot
# export
ggsave(
  'figures/6_stability_selection_retraction.jpg',
  pplot,
  width = 4.9,
  height = 5.2,
  units = 'in',
  dpi = 500
)
