# 4_fractional_polynomial.R
# fractional polynomials for the five continuous variables
# run before stability selection
# used for open review (yes/no), citations (count), retraction (survival)
# April 2026
library(ggplot2)
library(dplyr)
library(survival)

# get the data from 3_combine_experience_data.R on HPC
load('data/3_plus_experience.RData')
# remove small amount of missing country and last authors paper count (do not run 3_data_prepare.R)
data <- filter(data, !is.na(country))
data <- filter(data, !is.na(author_papers))
data <- filter(data, lengths(subjects)>1)

# make follow-up time to offset citation counts (scaled to per year)
date.searched = as.Date('2026-03-02') # date that open alex was searched
data = mutate(data, follow_up = as.numeric(date.searched - published)/365.25)

## create survival outcome for retractions
data = mutate(data, 
              open_review = case_when(
                review_available == TRUE ~ 'Yes',
                review_available == FALSE ~ 'No'
              ),
              time_to_retraction = time_to_retraction / 365.25) # convert to years

# works on unstransformed variables (have not yet called 4_data_prepare.R)
powers <- c(-2, -1, -0.5, 0, 0.5, 1, 2, 3) # fractional polynomials
fit = fit_citations = fit_retractions = NULL
for (p in powers) {
  data_p <- mutate(data,
                   # proportion with of ORCID
                   p_orcid = (n_orcids+0.5)/(n_authors+0.5), # avoid zero
                   orcid_power = case_when(
                     p == 0 ~ log2(p_orcid),
                     p != 0 ~ p_orcid^p
                   ),
                   # number of authors
                   n_authors = n_authors + 1,
                   author_power = case_when(
                     p == 0 ~ log2(n_authors),
                     p != 0 ~ n_authors^p
                   ),
                   # last author's paper count (experience)
                   author_papers = author_papers + 1,
                   experience_power = case_when(
                     p == 0 ~ log2(author_papers),
                     p != 0 ~ author_papers^p
                   ),
                   # time for peer review
                   time_between = time_between + 0.1,
                   time_power = case_when(
                     p == 0 ~ log2(time_between),
                     p != 0 ~ time_between^p
                   ),
                   # publication date
                   pdate = (as.numeric(published) - 18000)/365.25, # scale
                   date_power = case_when(
                     p == 0 ~ log2(pdate),
                     p != 0 ~ pdate^p
                   )
  )
  
  # retractions
  rmodel1 = coxph(Surv(time = time_to_retraction, event = retracted) ~ author_power, data = data_p)
  rmodel2 = coxph(Surv(time = time_to_retraction, event = retracted) ~ experience_power, data = data_p)
  rmodel3 = coxph(Surv(time = time_to_retraction, event = retracted) ~ time_power, data = data_p)
  rmodel4 = coxph(Surv(time = time_to_retraction, event = retracted) ~ date_power, data = data_p)
  rmodel5 = coxph(Surv(time = time_to_retraction, event = retracted) ~ orcid_power, data = data_p)
  rframe1 <- data.frame(model = "Number of authors", power = p, AIC = extractAIC(rmodel1)[2])
  rframe2 <- data.frame(model = "Last author`s experience", power = p, AIC = extractAIC(rmodel2)[2])
  rframe3 <- data.frame(model = "Peer review time", power = p, AIC = extractAIC(rmodel3)[2])
  rframe4 <- data.frame(model = "Publication date", power = p, AIC = extractAIC(rmodel4)[2])
  rframe5 <- data.frame(model = "ORCID proportion", power = p, AIC = extractAIC(rmodel5)[2])
  fit_retractions <- bind_rows(fit_retractions, rframe1, rframe2, rframe3, rframe4, rframe5)
  
  # citations
  cmodel1 <- glm(citations ~ author_power, data = data_p, family = poisson(), offset=log(follow_up)) # 
  cmodel2 <- glm(citations ~ experience_power, data = data_p, family = poisson(), offset=log(follow_up)) # 
  cmodel3 <- glm(citations ~ time_power, data = data_p, family = poisson(), offset=log(follow_up)) # 
  cmodel4 <- glm(citations ~ date_power, data = data_p, family = poisson(), offset=log(follow_up)) # 
  cmodel5 <- glm(citations ~ orcid_power, data = data_p, family = poisson(), offset=log(follow_up)) # 
  cframe1 <- data.frame(model = "Number of authors", power = p, AIC = AIC(cmodel1))
  cframe2 <- data.frame(model = "Last author`s experience", power = p, AIC = AIC(cmodel2))
  cframe3 <- data.frame(model = "Peer review time", power = p, AIC = AIC(cmodel3))
  cframe4 <- data.frame(model = "Publication date", power = p, AIC = AIC(cmodel4))
  cframe5 <- data.frame(model = "ORCID proportion", power = p, AIC = AIC(cmodel5))
  fit_citations <- bind_rows(fit_citations, cframe1, cframe2, cframe3, cframe4, cframe5)

  # open review
  model1 <- glm(review_available ~ author_power, data = data_p) # 
  model2 <- glm(review_available ~ experience_power, data = data_p) # 
  model3 <- glm(review_available ~ time_power, data = data_p) # 
  model4 <- glm(review_available ~ date_power, data = data_p) # 
  model5 <- glm(review_available ~ orcid_power, data = data_p) # 
  frame1 <- data.frame(model = "Number of authors", power = p, AIC = AIC(model1))
  frame2 <- data.frame(model = "Last author`s experience", power = p, AIC = AIC(model2))
  frame3 <- data.frame(model = "Peer review time", power = p, AIC = AIC(model3))
  frame4 <- data.frame(model = "Publication date", power = p, AIC = AIC(model4))
  frame5 <- data.frame(model = "ORCID proportion", power = p, AIC = AIC(model5))
  fit <- bind_rows(fit, frame1, frame2, frame3, frame4, frame5)
}

## add difference from best
#
fit = group_by(fit, model) %>%
  mutate(diff = AIC - min(AIC)) %>%
  ungroup()
arrange(fit, model, diff)
#
fit_citations = group_by(fit_citations, model) %>%
  mutate(diff = AIC - min(AIC)) %>%
  ungroup()
arrange(fit_citations, model, diff)
#
fit_retractions = group_by(fit_retractions, model) %>%
  mutate(diff = AIC - min(AIC)) %>%
  ungroup()
arrange(fit_retractions, model, diff)


## plots
# open review
fplot = ggplot(data = fit, aes(x = factor(power), y = diff))+
  scale_x_discrete()+
  geom_point()+
  theme_bw()+
  facet_wrap(~model, scales='free_y')+
  xlab('Fractional polynomial power (0 = log-transform)')+
  ylab('Difference in AIC from best model')
fplot
# export
ggsave('figures/4_fractional_polynomial_AIC.jpg', fplot, width=6, height=4, units='in', dpi=500)

# citations
fplot2 = ggplot(data = fit_citations, aes(x = factor(power), y = diff))+
  scale_x_discrete()+
  geom_point()+
  theme_bw()+
  facet_wrap(~model, scales='free_y')+
  xlab('Fractional polynomial power (0 = log-transform)')+
  ylab('Difference in AIC from best model')
fplot2
# export
ggsave('figures/4_fractional_polynomial_AIC_citations.jpg', fplot2, width=6, height=4, units='in', dpi=500)

# retractions
fplot3 = ggplot(data = fit_retractions, aes(x = factor(power), y = diff))+
  scale_x_discrete()+
  geom_point()+
  theme_bw()+
  facet_wrap(~model, scales='free_y')+
  xlab('Fractional polynomial power (0 = log-transform)')+
  ylab('Difference in AIC from best model')
fplot3
# export
ggsave('figures/4_fractional_polynomial_AIC_retractions.jpg', fplot3, width=6, height=4, units='in', dpi=500)
