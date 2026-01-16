# 99_dag.R
# DAG for what impacts open peer review
# see https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-dags.html
# August 2025
library(ggdag)
library(dplyr)
library(stringr)
library(ggplot2)
theme_set(theme_dag())
TeachingDemos::char2seed('lincoln')

# add article type? 99% are research articles, so possibly does not help here
# add email domain, education email

# DAG
dag_e <- dagify(#open ~ journal, # openness will depend on journal -- turned off, assume all effects happen via topic
                open ~ country, # openness will depend on country
                open ~ funder, # openness will depend on funder
                open ~ time, # openness will depend on country
                open ~ topic, # openness will depend on topic
                open ~ good, # openness will depend on whether reviews were good/glowing
                open ~ misconduct, # openness will be lower in those engaging in misconduct
                open ~ n_authors, # openness will depend on number of authors (more authors -> less open)
                open ~ e_authors,
                open ~ peer_time, # openness will depend on time spent in peer review
                good ~ paper_quality,
                paper_quality ~ misconduct,
                journal ~ topic, # journal chosen according to topic
                paper_quality ~ e_authors,
                country ~ time, # change in country make-up over time
                misconduct ~ time, # misconduct growing over time
                misconduct ~ country, # misconduct dependent on country
                funder ~ country, # funder will depend on country
                labels = c(
                  "open" = "Open peer\nreview",
                  "time" = "Time\ntrend",
                  "topic" = "Topics",
                  "country" = "Author's\ncountry",
                  'n_authors' = 'Number of\nauthors',
                  'misconduct' = 'Research\nmisconduct',
                  'journal' = 'Journal',
                  'peer_time' = 'Peer review\ntime',
                  'good' = 'Positive\nreviews',
                  'paper_quality' = 'Paper\nquality',
                  'e_authors' = 'Authors`\nexperience',
                  'funder' = 'Funder'
                ),
                latent = c("good",'misconduct','paper_quality'),
                exposure = "time",
                outcome = "open"
)

# get data and plot to allow unmeasured variables in different shape
for_plot = tidy_dagitty(dag_e) %>%
  mutate(dependent = str_detect(name, pattern='^open'),
         latent = str_detect(name, pattern='good|misconduct|paper_quality'))
# adjustments to coordinates to make DAG look better
#for_plot$data = mutate(for_plot$data,
#                       yend = ifelse(name=='country' & to == 'english', -0.1, yend),

# plot
dag_plot = ggplot(for_plot, aes(x = x, y = y, xend = xend, yend = yend, col = dependent, shape=latent)) +
  geom_dag_point(show.legend = FALSE) + # stops both legends
  geom_dag_edges() +
  geom_dag_text(col='grey44', aes(x = x, y = y, label= label), cex=2.1)+
  scale_shape_manual('Measured', values=c(1,0), labels=c('Yes','No'))+
  scale_color_manual(NULL, values=c('darkorchid2','darkseagreen4'))+
  theme_dag() 
dag_plot

# export
ggsave(filename = 'figures/dag.jpg', plot = dag_plot, width=6, height=5, units='in', dpi=500)

#
ggdag_collider(dag_e) # find colliders
control_for(dag_e, var = "open")
ggdag_adjust(dag_e, var = "open")
ggdag_adjustment_set(dag_e)


## simpler model for citations
# citations depend on open peer review
# quality influences open peer review
# quality influences citations

## retractions
# time to retracion depends on open peer review
# confounder of topic? fit without confounders?