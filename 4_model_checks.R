# 4_model_checks.R
# run model checks
# November 2025
source('R/vif_matrix.R')
library(xtable) # for latex
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
g.theme = theme_bw()+ theme(panel.grid.minor = element_blank())
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "yellow3", "#0072B2", "#D55E00", "#CC79A7")

# get the data from 2_add_author_experience.R
load('data/2_plus_experience.RData')
# prepare the data
source('3_data_prepare.R')

# get the model estimates, from 3_stability_selection.R
load('results/3_ests_stability.RData')

# influential diagnostics
influential = influence.measures(small_model)
#which(apply(influential$is.inf, 1, any))
#plot(rstudent(small_model) ~ hatvalues(small_model)) # recommended by some

## large Cook's distance
index = which(colnames(influential$infmat) == 'cook.d')
cookd = influential$infmat[,index]
index = which(cookd > 6e-04)
index_y = colnames(x) %in% x_selected_names
x_influential = x[index,index_y]
y_influential = y[index]
d_influential = cbind(y_influential, x_influential)
# plot
to_plot = data.frame(cookd)
cplot = ggplot(data = to_plot, aes(x=cookd))+
  geom_histogram(fill = cbbPalette[3])+
  xlab('Cook`s distance')+
  ylab('Count')+
  g.theme
cplot
ggsave('figures/4_cooks_distance.jpg', cplot, width = 4.2, height=4.2, units='in', dpi=500)
cat('The largest Cook`s distance was ', format(max(to_plot$cookd), scientific=FALSE), '.\n', sep='')

## largest df-betas
dfb = dfbeta(small_model)
to_plot = data.frame(dfb) %>%
  reshape::melt() %>%
  mutate(variable = str_remove(variable, '^X.|x_selected'))
dplot = ggplot(to_plot, aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable, scales='free')+
  g.theme
dplot
#
mutate(to_plot, abs = abs(value)) %>%
  arrange(value) %>%
  head()

# check colinearity, car does not work as it expects model.matrix
vif = vif_matrix(small_model)
to_export = data.frame(vif) %>%
  tibble::rownames_to_column() %>%
  mutate(# labels
    rowname = str_replace(rowname, '^type', 'type_'), # type did not start with _
    rowname = str_replace(rowname, '_', ' = '), # replace first underbar with equals ...
    rowname = str_replace_all(rowname, '_', ' ') # ... and then remaining with space
  )
# export to latex
print(xtable(to_export, digits=2), include.rownames=FALSE, hline.after=FALSE, file = "results/4_vif.tex")


## smooth fit against predicted
pred = predict(small_model)
for_plot = data.frame(observed = as.numeric(y), 
                      predicted = pred,
                      random = runif(n=length(pred), min=0, max=0.15)) %>% # for jitter
  mutate(observed_jitter = case_when(
    y == 1 ~ observed - random,
    y == 0 ~ observed + random
  ),
  negative = as.factor(as.numeric(predicted<0)))
# 
p_neg = 100*sum(pred<0) / length(pred)
cat('There were ', sum(pred<0), ' negative predictions which is ', round(p_neg*100)/100, '%.\n', sep='')
# a) dots
pplot = ggplot(data = for_plot, aes(x=predicted, y=observed))+
  geom_point(col='transparent')+ # not plotted, but used for smooth
  geom_smooth()+
  geom_abline(intercept = 0, slope=1, lty=2)+
  geom_point(data = for_plot, aes(x=pred, y=observed_jitter, colour = negative), pch=1, size=1)+ # used for plot but not smooth
  scale_color_manual(NULL, values=c('navy','darkred'))+
  scale_y_continuous(breaks=c(0,1), labels=c('No','Yes'), expand=c(0,0))+
  coord_cartesian(ylim=c(0,1),xlim=c(NA,1))+ # limit y-axis, make sure x axis goes up to 1
  xlab('Predicted probability')+
  ylab('Open review')+
  g.theme+
  theme(legend.position = 'none')
pplot
ggsave('figures/4_fit_vs_observed_dots.jpg', pplot, width = 5.4, height=4.8, units='in', dpi=500)
# b) histogram - using grid.arrange
# i) smooth without dots
pplot1 = ggplot(data = for_plot, aes(x=predicted, y=observed))+
  geom_point(col='transparent')+ # not plotted, but used for smooth
  geom_smooth()+
  geom_abline(intercept = 0, slope=1, lty=2)+
  scale_y_continuous(breaks=c(0,1), labels=c('No','Yes'), expand=c(0,0))+
  coord_cartesian(ylim=c(0,1),xlim=c(NA,1))+ # limit y-axis, make sure x axis goes up to 1
  xlab('Predicted probability')+
  ylab('Open review')+
  g.theme+
  theme(legend.position = 'none')
pplot1
# ii) top histogram
hplot1 = ggplot(data = filter(for_plot, observed==1), aes(x=predicted))+
  geom_histogram(fill='darkseagreen3', breaks=seq(0,1,0.02))+
  coord_cartesian(xlim=c(-0.1,1))+ # make sure x axis goes up to 1
  xlab(NULL)+
  ylab(NULL)+
  g.theme
hplot1
# ii) bottom histogram
hplot2 = ggplot(data = filter(for_plot, observed==0), aes(x=predicted))+
  geom_histogram(fill='darkseagreen1', breaks=seq(0,1,0.02))+
  coord_cartesian(xlim=c(-0.1,1))+ # make sure x axis goes up to 1
  xlab(NULL)+
  ylab(NULL)+
  g.theme
hplot2
# boxplots instead of histograms
bplot = ggplot(data = for_plot, aes(x=factor(observed), y=predicted, col=factor(observed)))+
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
jpeg('figures/4_fit_vs_observed.jpg', width = 5.4, height=4.8, units='in', res=500)
grid.arrange(bplot, pplot1, ncol = 1, heights = c(0.15,1))
dev.off()
