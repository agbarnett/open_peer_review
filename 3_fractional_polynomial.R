# 3_fractional_polynomial.R
# fractional polynomials for the four continuous variables
# January 2026
library(ggplot2)
library(dplyr)

# get the data from 2_patch_author_experience.R
load('data/2_plus_experience.RData')
# remove small amount of missing country and last authors paper count (do not run 3_data_prepare.R)
data <- filter(data, !is.na(country))
data <- filter(data, !is.na(author_papers))
data <- filter(data, lengths(subjects)>1)
data <- filter(data, type != 'Formal Comment') 

# works on unstransformed variables (have not yet called 3_data_prepare.R)
powers <- c(-2, -1, -0.5, 0, 0.5, 1, 2, 3) # fractional polynomials
fit <- NULL
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
# add difference from best
fit = group_by(fit, model) %>%
  mutate(diff = AIC - min(AIC)) %>%
  ungroup()
arrange(fit, model, diff)

# plot
fplot = ggplot(data = fit, aes(x = factor(power), y = diff))+
  scale_x_discrete()+
  geom_point()+
#  geom_line(linewidth=0.1)+
  theme_bw()+
  facet_wrap(~model, scales='free_y')+
  xlab('Fractional polynomial power (0 = log-transform)')+
  ylab('Difference in AIC from best model')
fplot
# export
ggsave('figures/3_fractional_polynomial_AIC.jpg', fplot, width=6, height=4, units='in', dpi=500)
