# 3_survival_retractions.R
# survival models for time to retraction
# December 2025
library(dplyr)
library(stringr)
library(survival)
library(survminer) # for plotting
source('R/dark_theme.R') # for dark slide theme for AIMOS presentation
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999") # colours
# folder for figures:
loc = 'C:/Users/barnetta/OneDrive - Queensland University of Technology/talks/AIMOS5/figures/'

# get the data from 2_patch_author_experience.R
load('data/2_plus_experience.RData')

# prepare the data
source('3_data_prepare.R')
# create outcome
data = mutate(data, 
              open_review = case_when(
                review_available == TRUE ~ 'Yes',
                review_available == FALSE ~ 'No'
              ),
              time_to_retraction = time_to_retraction / 365.25) # convert to years

# run the survival model
smodel = survfit(Surv(time_to_retraction,retracted) ~ open_review, data = data)
summary(smodel)
# plot inverse survival
splot = ggsurvplot(smodel, 
                   data = data,
                   palette = cbPalette[1:2],
                   conf.int = TRUE, # Add confidence interval
                   xlab = "Time to retraction (years)",
                   ylab = 'Cumulative retraction',
                   xlim = c(0, 5.2), # last retraction is 5.1 years
                   censor.size = 0, # suppress censoring ticks
                   ncensor.plot = FALSE, 
                   legend.labs=c("No",'Yes'),  
                   legend.title = 'Open review',
                   fun = 'event', # reverse curve to plot events
                   risk.table = FALSE) 
# move legend inside
lplot = splot$plot + theme(legend.position = "inside", legend.position.inside = c(0.19,0.82))
# add text labels
end = summary(smodel, times = 5.2) # can't do max time as it's not available for yes group
label = data.frame(time = 5.3, rate = 1-end$surv, label = paste('n = ', end$n.event, sep=''))
lplot_black = lplot + 
  theme(plot.margin = unit(c(0,1.2,0,0), "cm"))+
  geom_text(data = label, aes(x=time, y=rate, label=label), adj=-0.85) +
  coord_cartesian(clip = "off")
# export
ggsave('figures/3_survival_retraction.jpg', lplot_black, width=4.5, height=4.2, units='in', dpi = 500)

# version for slide #
splot_slide = lplot + dark.theme +
  geom_text(data = label, aes(x=time, y=rate+0.0005, label=label), col='white', adj=-0.85)+
  coord_cartesian(clip = "off")
out = paste(loc, '3_survival_retraction_slide.jpg', sep='')
ggsave(filename = out, splot_slide, width=6.5, height=4.2, units='in', dpi = 500, bg = 'transparent')

# Cox model
cmodel = coxph(Surv(time = time_to_retraction, event = retracted) ~ review_available, data = data)
summary(cmodel)

# retraction numbers
group_by(data, review_available, retracted) %>% tally()

# check proportional hazards assumption
test.ph <- cox.zph(cmodel)
test.ph

# Total follow-up time (years)
format(round(sum(data$time_to_retraction)), big.mark=',')
# average follow-up time (years)
round(10*sum(data$time_to_retraction) / nrow(data))/10

