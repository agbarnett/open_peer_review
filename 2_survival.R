# 2_survival.R
# survival models for time to retraction
# August 2025
library(dplyr)
library(survival)
library(survminer) # for plotting
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999") # colours

# get data, from 1_process.R
load("data/1_processed.RData")

# prepare data
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
                   censor.size = 0, # supress censoring ticks
                   ncensor.plot = FALSE, 
                   legend.labs=c("No",'Yes'),  
                   legend.title = 'Open review',
                   fun = 'event', # reverse curve to plot events
                   risk.table = FALSE) 
# move legend inside
lplot = splot$plot + theme( legend.position = "inside", legend.position.inside = c(0.19,0.82))
# export
ggsave('figures/2_survival_retraction.jpeg', lplot, width=4.5, height=4.2, units='in', dpi = 500)

# Cox model
cmodel = coxph(Surv(time = time_to_retraction, event = retracted) ~ review_available, data = data)
summary(cmodel)

# retraction numbers
group_by(data, review_available, retracted)%>% tally()

# check proportional hazards assumption

# Total follow-up time (years)
format(round(sum(data$time_to_retraction)), big.mark=',')
# average follow-up time (years)
round(10*sum(data$time_to_retraction) / nrow(data))/10

