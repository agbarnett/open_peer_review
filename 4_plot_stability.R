# 4_plot_stability.R
# plot the categorical estimates from the stability selection
# January 2026
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra) # for arranging multiple plots
library(ggtext) # for markdown
library(ggforce) # for better spaced facet_wrap
source('R/dark_theme.R') # for slide theme
g.theme = theme_bw()+ theme(panel.grid.minor = element_blank())
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "yellow3", "#0072B2", "#D55E00", "#CC79A7")
dash_colour = 'white' # for reference line at zero
# folder for figures:
loc = 'C:/Users/barnetta/OneDrive - Queensland University of Technology/talks/AIMOS5/figures/'

# load the results
load('results/3_ests_stability.RData') # from 3_stability_selection.R

# need to run for mean and SD used for standardisation
load('data/2_plus_experience.RData')
source('3_data_prepare.R')

### Plot 1: plot which variables were selected ###
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
labels = str_replace_all(labels, 'p_orcid', 'ORCID proportion') # better label
labels = str_replace(labels, '501100000781', 'European Research Council') # change funder numbers to names
labels = str_replace(labels, '501100000265', 'Medical Research Council')
labels = str_replace(labels, '100004440', 'Wellcome')
labels = str_replace(labels, '100000011', 'Howard Hughes Medical Institute')

labels = str_replace(labels, '_', ' = ') # replace first underbar with equals ...
labels = str_replace_all(labels, '_', ' ') # ... and then remaining with space
labels = str_replace_all(labels, 'published', 'Date published') # better label

## has colour labels for selected variables
colours = c("coral3", "green3")
n_selected = sum(for_plot$selected) # number selected
pplot = ggplot(data = for_plot, aes(x = x, y = stab.lasso.max, col = selected))+
  geom_point(size = 3)+
  scale_x_continuous(breaks = 1:nrow(for_plot), 
                     expand = c(0.01,0.01),
                     labels = function(x, text = to_plot$var) {
                       col <- ifelse(x > n_selected , colours[1], colours[2])
                       lab <- labels
                       glue::glue("<span style = 'color:{col}'>{lab}</span>")
                     })+
  scale_y_continuous(breaks = seq(0.25,1,0.25))+
  scale_color_manual('Included in\nfinal model', values = colours, labels=c('No','Yes'))+
  xlab(NULL)+
  ylab('Selection probability')+
#  geom_hline(yintercept = stab.lasso$cutoff, col = dash_colour, lty=3, linewidth=1.25)+ # not needed because of colours
  coord_flip()+
  g.theme +
  theme(legend.margin = margin(1, 1, 1, 1),
        legend.position = 'inside',
        legend.position.inside = c(0.79, 0.88),
        legend.spacing = unit(1, 'mm'),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.y = element_markdown(size=8, hjust=1, vjust=0.5)) # to make html work
pplot
# export
ggsave('figures/4_stability_selection.jpg', pplot, width = 4.9, height=5.2, units='in', dpi=500)
cat('There were ', filter(for_plot, stab.lasso.max==1)%>%nrow(), ' variables that were selected in all 100 bootstrap samples.\n', sep='')
cat('There were ', n_selected, ' variables selected.\n', sep='')

### Plot 2: plot categorical estimates ###
to_plot = filter(ests, !str_detect(term, 'Intercept|published$|orcid')) %>%
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
  arrange(group, desc(estimate)) %>%
  mutate(x = 1:n())
# plot, start with standard version
expand = 0.4
cplot = ggplot(data = to_plot, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, col=group))+
  geom_point(size=3)+
  geom_hline(lty=3, yintercept=0)+
  geom_errorbar(width=0, linewidth=1.05)+ # stuck with single interval
 # geom_errorbar(data = to_plot, aes(x = x, y = estimate, ymin = lower_50, ymax = upper_50, col=group), width=0, linewidth=1.05)+ # wider for 50% interval
  ylab('Difference in the probability of choosing open review')+
  xlab(NULL)+
  scale_color_manual(NULL, values = cbbPalette)+
  scale_x_continuous(breaks = 1:nrow(to_plot), 
                     labels = to_plot$term,
                     expand = expansion(mult = 0, add = expand))+ # using add for consistent gap by panels
  g.theme+
  theme(legend.position = 'none',
        strip.text.x = element_text(margin = margin(t=0.7, r=0, b=0.7, l=0, "mm")))+ # reduce facet size
  coord_flip()
# now make column version ...
cplot_column = cplot + ggforce::facet_col(vars(group), scales='free', space='free') # forced into one column
# export
ggsave('figures/4_stability_estimates.jpg', cplot_column, width = 5.8, height=7.2, units='in', dpi=500)

# ... and square version 
expand = 0.6
cplot_square = cplot + facet_wrap(~group, ncol = 2, scales='free') + 
  scale_x_continuous(breaks = 1:nrow(to_plot), 
                     labels = to_plot$term,
                     expand = expansion(mult = 0, add = expand)) + # need bigger expand
  theme(plot.margin = unit(c(0,0.3,0,0), "cm")) # avoid cut-off at right side
ggsave('figures/4_stability_estimates_square.jpg', cplot_square, width = 7.2, height=7.2, units='in', dpi=500)

# plot country separately
to_plot_country = filter(to_plot, group == 'Country') %>%
  mutate(x=1:n())
expand = 0.03
cplot2 = ggplot(data = to_plot_country, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, col=group))+
  geom_point(size=2)+
  geom_hline(lty=3, yintercept=0, col = dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.05)+
  ylab('Difference in the probability of choosing open review')+
  xlab(NULL)+
  scale_color_manual(NULL, values = cbbPalette)+
  scale_x_continuous(breaks = 1:nrow(to_plot_country), 
                     labels = to_plot_country$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(legend.position = 'none')+
  coord_flip()
cplot2
# export
ggsave('figures/4_stability_country.jpg', cplot2, width = 4.7, height=4.5, units='in', dpi=500)
# version for slide
cplot_slide = cplot2 + dark.theme + 
  geom_point(size = 4, col = 'darkseagreen1')+ # thicker and more noticeable
  geom_errorbar(width=0, linewidth=1.25, col = 'darkseagreen1')
out = paste(loc, '4_country_slide.jpg', sep='')
ggsave(filename = out, cplot_slide, width=6.5, height=4.2, units='in', dpi = 500, bg = 'transparent')

# plot estimates bar country separately
to_plot_not_country = filter(to_plot, group != 'Country') %>%
  mutate(x=1:n(),
         term = ifelse(str_detect(term, 'environ'), "Ecology & environ-\nmental sciences\n(n=10,293)", term)) # to squeeze in
expand = 0.04
cplot3 = ggplot(data = to_plot_not_country, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high, col=group))+
  geom_point(size=4)+
  geom_hline(lty=3, yintercept=0, col=dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.25)+
  ylab('Difference in the probability of choosing open review')+
  xlab(NULL)+
  scale_color_manual(NULL, values = cbbPalette)+
  scale_x_continuous(breaks = 1:nrow(to_plot_not_country), 
                     labels = to_plot_not_country$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(legend.position = 'none',
        plot.margin = unit(c(0,0.3,0,0), "cm"))+ # avoid cut-off at right side
  coord_flip()+
  facet_wrap(~group, scales='free')
cplot3
# version for slide
cplot3_slide = cplot3 + dark.theme + 
  geom_point(size = 4)+ # thicker and more noticeable
  geom_errorbar(width=0, linewidth=1.25)
out = paste(loc, '4_not_country_slide.jpg', sep='')
ggsave(filename = out, cplot3_slide, width=8.5, height=4.7, units='in', dpi = 500, bg = 'transparent')

## alternative version to above with separate panels that are assembled using grid.arrange
# see https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap


#### layout version with combined areas and a bigger area for country ###
expand = 0.03
dash_colour = 'grey12'
## type
colour = 'navy'
to_plot_type = filter(to_plot, group == 'Article type') %>%
  mutate(x=1:n(),
         term = str_replace(term, '^Research ','Research\n'),
         term = str_replace(term, '& ','&\n')) # to squeeze in
tplot = ggplot(data = to_plot_type, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high))+
  ggtitle('Article type')+
  geom_point(size=2, color = colour)+
  geom_hline(lty=3, yintercept=0, color = dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.05, color = colour)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(to_plot_type), 
                     labels = to_plot_type$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(plot.margin = unit(c(t=1,r=2,b=1,l=12), "mm"))+ # use left margin to match others
  coord_flip()
## country
cplot2 = cplot2 + 
  ggtitle('Country') + 
  ylab(NULL) +
  geom_hline(lty=3, yintercept=0, color = dash_colour, linewidth=1.25)+
  theme(plot.margin = unit(c(t=1,r=2,b=1,l=1), "mm"))
## domain
colour = 'darkseagreen3'
to_plot_domain = filter(to_plot, group == 'Email domain') %>%
  mutate(x=1:n())
dplot = ggplot(data = to_plot_domain, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high))+
  ggtitle('Email domain')+
  geom_point(size=2, color = colour)+
  geom_hline(lty=3, yintercept=0, color = dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.05, color = colour)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(to_plot_domain), 
                     labels = to_plot_domain$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(plot.margin = unit(c(t=1,r=2,b=1,l=14), "mm"))+ # expand left margin to match other plots (trial and error)
  coord_flip()
## subject
colour = 'yellow3'
to_plot_subject = filter(to_plot, group == 'Subject') %>%
  mutate(term = str_replace(term, "environmental", "environ-\nmental"), # to squeeze in
         x=1:n())
splot = ggplot(data = to_plot_subject, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high))+
  ggtitle('Subject')+
  geom_point(size=2, color = colour)+
  geom_hline(lty=3, yintercept=0, color = dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.05, color = colour)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(to_plot_subject), 
                     labels = to_plot_subject$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(plot.margin = unit(c(t=1,r=2,b=1,l=1), "mm"))+
  coord_flip()
#
## funder
colour = 'brown'
to_plot_funder = filter(to_plot, group == 'Funder') %>%
  mutate(x=1:n(),
         term = str_replace(term, ' Research ','\nResearch\n')) # to reduce white space
fplot = ggplot(data = to_plot_funder, aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high))+
  ggtitle('Funder')+
  geom_point(size=2, color = colour)+
  geom_hline(lty=3, yintercept=0, color = dash_colour, linewidth=1.25)+
  geom_errorbar(width=0, linewidth=1.05, color = colour)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(breaks = 1:nrow(to_plot_funder), 
                     labels = to_plot_funder$term,
                     expand = c(expand, expand))+
  g.theme+
  theme(plot.margin = unit(c(t=1,r=2,b=1,l=12), "mm"))+
  coord_flip()

## X-axis label
axis_label = 'Difference in the probability of choosing open review'
lplot = ggplot(data = NULL)+
  geom_text(aes(x=0,y=0,label=axis_label))+
  theme_void()

# using layout for combined plot
lmat = matrix(c(1,2,3,6,4,4,5,6), ncol=2, byrow=FALSE) 
jpeg('figures/4_stability_estimates_square.jpg', width = 7.2, height=7.2, units='in', res=500)
grid.arrange(tplot, dplot, splot, cplot2, fplot, lplot,
             layout_matrix = lmat, heights = c(1,1,1,0.1)) # heights for x-axis label at bottom
dev.off()

