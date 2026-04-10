# 5_model_checks.R
# run model checks
# March 2026
source('R/vif_matrix.R') # for VIF
library(xtable) # for latex
library(stringr)
library(rpart) # for trees
library(dplyr)
library(broom)
library(ggplot2)
library(ggrepel)
library(gridExtra)
g.theme = theme_bw()+ theme(panel.grid.minor = element_blank())
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "yellow3", "#0072B2", "#D55E00", "#CC79A7")

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# prepare the data
source('4_data_prepare.R')

# get the model estimates, from 4_stability_selection.R
load('results/4_ests_stability.RData')

# influential diagnostics
influential = influence.measures(small_model)
#which(apply(influential$is.inf, 1, any))
#plot(rstudent(small_model) ~ hatvalues(small_model)) # recommended by some

## large Cook's distance
index = which(colnames(influential$infmat) == 'cook.d')
cookd = influential$infmat[,index]
index = which(cookd > 1e-04)
index_y = colnames(x) %in% x_selected_names
x_influential = x[index,index_y]
y_influential = y[index]
d_influential = cbind(y_influential, x_influential)
# check what's driving high Cook's distances 
my.control = rpart.control()
my.control$maxdepth = 2 # small tree
tree_cook = rpart(cookd ~ x, control = my.control)
tree_cook
# summary stats for ERC vs not
index_erc  = colnames(x) == 'funder_501100000781' # funder is ERC
summary(cookd[x[,index_erc]==1])
summary(cookd[x[,index_erc]==0])

# using 4/n threshold; could not find good reference for this, appears arbitrary
n = nrow(x)
table(cookd > 4/n)
prop.table(table(cookd > 4/n))

# plot
to_plot_cook = data.frame(cookd)
cplot = ggplot(data = to_plot_cook, aes(x=cookd))+
  geom_histogram(fill = cbbPalette[3])+
  xlab('Cook`s distance')+
  ylab('Count')+
#  geom_vline(lty=2, xintercept=4/n)+ # threshold
  g.theme
cplot
ggsave('figures/5_cooks_distance.jpg', cplot, width = 4.2, height=4.2, units='in', dpi=500)
cat('The largest Cook`s distance was ', format(max(to_plot_cook$cookd), scientific=FALSE), '.\n', sep='')

## largest df-betas, are for 501100000781 - European Research Council
dfb = dfbeta(small_model)
to_plot_df = data.frame(dfb) %>%
  reshape::melt() %>%
  mutate(variable = str_remove(variable, '^X.|x_selected'))
dplot = ggplot(to_plot_df, aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable, scales='free')+
  g.theme
dplot
#
mutate(to_plot_df, abs = abs(value)) %>%
  arrange(value) %>%
  head()

# check colinearity, car does not work as it expects model.matrix
vif = vif_matrix(small_model)
to_export = data.frame(vif) %>%
  tibble::rownames_to_column() %>%
  mutate(# labels
    rowname = str_replace(rowname, '501100000781', 'European Research Council'), # change funder numbers to names
    rowname = str_replace(rowname, '501100000265', 'Medical Research Council'),
    rowname = str_replace(rowname, '^published', 'Date published'),
    rowname = str_replace(rowname, 'p_orcid', 'ORCID proportion'), 
    rowname = str_replace(rowname, '^type', 'type_'), # type did not start with _
    rowname = str_replace(rowname, '_', ' = '), # replace first underbar with equals ...
    rowname = str_replace_all(rowname, '_', ' ') # ... and then remaining with space
  )
# export to latex
print(xtable(to_export, digits=2), include.rownames=FALSE, hline.after=FALSE, file = "results/5_vif.tex")


## smooth fit against predicted
pred = predict(small_model)
for_plot_pred = data.frame(observed = as.numeric(y), 
                      predicted = pred,
                      random = runif(n=length(pred), min=0, max=0.15)) %>% # for jitter
  mutate(observed_jitter = case_when(
    y == 1 ~ observed - random,
    y == 0 ~ observed + random
  ),
  over_1 = as.factor(as.numeric(predicted>1)),
  negative = as.factor(as.numeric(predicted<0)))
# 
p_neg = 100*sum(pred<0) / length(pred)
cat('There were ', sum(pred<0), ' negative predictions which is ', round(p_neg*100)/100, '%.\n', sep='')
p_over = 100*sum(pred>1) / length(pred)
cat('There were ', sum(pred>1), ' predictions over 1 which is ', round(p_over*100)/100, '%.\n', sep='')
# text labels
text1 = data.frame(x=0.75, y=0.25, text = 'Model overestimates\nprobability')
text2 = data.frame(x=0.25, y=0.75, text = 'Model underestimates\nprobability')

# a) dots
pplot = ggplot(data = for_plot_pred, aes(x=predicted, y=observed))+
  geom_point(col='transparent')+ # not plotted, but used for smooth
  geom_smooth()+
  geom_label(data = text1, aes(x=x, y=y, label=text), col='grey55')+
  geom_label(data = text2, aes(x=x, y=y, label=text), col='grey55')+
  geom_abline(intercept = 0, slope=1, lty=2)+
  geom_point(data = for_plot_pred, aes(x=pred, y=observed_jitter, colour = negative), pch=1, size=1)+ # used for plot but not smooth
  scale_color_manual(NULL, values=c('navy','darkred'))+
  scale_y_continuous(breaks=c(0,1), labels=c('No','Yes'), expand=c(0,0))+
  coord_cartesian(ylim=c(0,1),xlim=c(0,1))+ # limit y-axis, make sure x axis goes from 0 to 1
  xlab('Predicted probability')+
  ylab('Open review')+
  g.theme+
  theme(legend.position = 'none')
pplot
ggsave('figures/5_fit_vs_observed_dots.jpg', pplot, width = 5.4, height=4.8, units='in', dpi=500)
# b) histogram - using grid.arrange
# i) smooth without dots
pplot1 = ggplot(data = for_plot_pred, aes(x=predicted, y=observed))+
  geom_point(col='transparent')+ # not plotted, but used for smooth
  geom_label(data = text1, aes(x=x, y=y, label=text), col='grey55')+
  geom_label(data = text2, aes(x=x, y=y, label=text), col='grey55')+
  geom_smooth()+
  geom_abline(intercept = 0, slope=1, lty=2)+
  scale_y_continuous(breaks=c(0,1), labels=c('No','Yes'), expand=c(0,0))+
  coord_cartesian(ylim=c(0,1),xlim=c(0,1))+ # limit y-axis, make sure x axis goes from 0 to 1
  xlab('Predicted probability')+
  ylab('Open review')+
  g.theme+
  theme(legend.position = 'none')
pplot1
# ii) top histogram
hplot1 = ggplot(data = filter(for_plot_pred, observed==1), aes(x=predicted))+
  geom_histogram(fill='darkseagreen3', breaks=seq(0,1,0.02))+
  coord_cartesian(xlim=c(-0.1,1))+ # make sure x axis goes up to 1
  xlab(NULL)+
  ylab(NULL)+
  g.theme
hplot1
# ii) bottom histogram
hplot2 = ggplot(data = filter(for_plot_pred, observed==0), aes(x=predicted))+
  geom_histogram(fill='darkseagreen1', breaks=seq(0,1,0.02))+
  coord_cartesian(xlim=c(-0.1,1))+ # make sure x axis goes up to 1
  xlab(NULL)+
  ylab(NULL)+
  g.theme
hplot2
# boxplots instead of histograms
bplot = ggplot(data = for_plot_pred, aes(x=factor(observed), y=predicted, col=factor(observed)))+
  geom_boxplot(outlier.shape = 1)+
  scale_color_manual(NULL, values = cbbPalette[c(3,5)])+
  coord_flip(ylim=c(NA,1))+ # make sure upper limit is 1
  g.theme+
  xlab(' ')+ # rely on other axis
  scale_y_continuous(breaks=seq(0,1,0.25), labels=NULL)+ # use labels from other plot
  scale_x_discrete(breaks=c(0,1), labels=c('No','Yes'), expand=c(0.22,0.22))+
  ylab(NULL)+
  theme(legend.position='none')
bplot
#
jpeg('figures/5_fit_vs_observed.jpg', width = 5.4, height=4.8, units='in', res=500)
grid.arrange(bplot, pplot1, ncol = 1, heights = c(0.15,1))
dev.off()

# examine large residuals (high predicted probability but not shared)
vars_to_keep = filter(ests, term!='(Intercept)') %>%
  pull(term) %>%
  str_remove('x_selected') %>%
  c('predicted')
large = bind_cols(x, for_plot_pred) %>% # add observed data
  filter(predicted > 0.7, observed==0) %>%
  select(all_of(vars_to_keep))
# no clear pattern

## covratio - measure the influence of individual observations on the variance of the regression estimates
cr = covratio(small_model)
to_plot = bind_cols(x, cr) %>%
  as.data.frame() %>%
  mutate(id = 1:n(),
         acr = abs(cr - 1)) # to judge against limit
n = 116359; p =22; limit = 3*p/n # 
quantile(cr, 0.99) # observed upper limit
#
cplot = ggplot(data = to_plot, aes(x = id, y = cr, col = factor(funder_501100000781)))+
  geom_point(shape=1, size=1)+
  xlab('index')+
  ylab('COVRATIO')+
#  geom_hline(lty = 2, yintercept=limit) + # limit
  scale_color_manual('Funder = European Research Council', values=c('darkseagreen2','darkorchid2'), labels=c('No','Yes'))+
#  scale_color_manual('Methods and resources', values=c('darkseagreen2','darkorchid2'), labels=c('No','Yes'))+ # previous problem
  g.theme+
  theme(legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
        legend.margin = margin(0, 0, 0, 0), # reduce space
        legend.position = 'top')
cplot
#
ggsave(filename = 'figures/5_covratio.jpg', plot = cplot, width = 5.4, height=4.5, units='in', dpi=500)

# find what's driving separate covratio values 
my.control = rpart.control()
my.control$maxdepth = 2 # small tree
tree = rpart(cr ~ x, control = my.control)
tree

## models with and without funder (ERC)
index1 = colnames(x) %in% x_selected_names
with_funder = glm(y ~ x[,index1])
x_selected_names_dash = x_selected_names[x_selected_names!='funder_501100000781']
index2 = colnames(x) %in% x_selected_names_dash
without_funder = glm(y ~ x[,index2])
# compare using Bland-Altman
ests_with = tidy(with_funder) %>%
  mutate(term = str_remove(term, 'x\\[, index1\\]'))
ests_without = tidy(without_funder)%>%
  mutate(term = str_remove(term, 'x\\[, index2\\]')) %>%
  rename('estimate2' = 'estimate',
         'std.error2' = 'std.error',
         'statistic2' = 'statistic',
         'p.value2' = 'p.value')
both = full_join(ests_with, ests_without, by='term') %>%
  mutate(av = (estimate+estimate2)/2,
         diff = estimate - estimate2,
         term = str_replace(term, 'country_', 'Country =\n'),
         term = str_replace(term, 'subject_', 'Subject =\n'),
         term = str_replace(term, 'domain_', 'Domain =\n'),
         term = str_replace(term, '^type', 'Type =\n'),
         term = str_replace(term, 'p_orcid', 'ORCID'),
         term = str_replace(term, 'published', 'Date published'),
         term = str_replace_all(term, '_', ' ')) %>%
  filter(!is.na(av),
         term != '(Intercept)')
# labels
label1 = data.frame(av = 0.01, diff = 0.009, label = 'Without greater')
label2 = data.frame(av = 0.01, diff = -0.009, label = 'With greater')
# limits of agreement
loa_lower = quantile(both$diff, 0.025)
loa_upper = quantile(both$diff, 1 - 0.025)
#
baplot = ggplot(data = both, aes(x = av, y=diff, label=term))+
  geom_hline(lty=2, col='navy', yintercept=0, linewidth=0.5)+ # lines on top of dots
  geom_hline(lty=2, col='darkred', yintercept=loa_lower, linewidth=0.5)+
  geom_hline(lty=2, col='darkred', yintercept=loa_upper, linewidth=0.5)+
  #  geom_text(data = label1, aes(x = av, y =diff, label = label), col='grey66', adj=0)+
#  geom_text(data = label2, aes(x = av, y =diff, label = label), col ='grey66', adj=0)+
  geom_point(size=3, pch=19, col='darkseagreen4') + #  
  geom_text_repel(size=2)+
  theme_bw()+
  theme(panel.grid.minor=element_blank())+
 # scale_x_continuous(lim=c(0,NA))+ # start at zero
  xlab('Average of probabilities with and without ERC')+
  ylab('Probability with ERC minus without ERC')
baplot
# export
ggsave(filename = 'figures/5_estimates_without_funder.jpg', baplot, width = 5, height=4, units='in', dpi=400)
