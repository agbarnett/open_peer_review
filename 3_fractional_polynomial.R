# 3_fractional_polynomial.R
# fractional polynomials for the four continuous variables
# November 2025
library(ggplot2)
library(dplyr)

# get the data from 2_add_author_experience.R
load('data/2_plus_experience.RData')
# remove small amount of missing country and last authors paper count
data <- filter(data, !is.na(country))
data <- filter(data, !is.na(author_papers))

# works on unstransformed variables (have not yet called 3_data_prepare.R)
powers <- c(-2, -1, -0.5, 0, 0.5, 1, 2, 3) # fractional polynomials
fit <- NULL
for (p in powers) {
  data_p <- mutate(data,
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
  frame1 <- data.frame(model = "Number of authors", power = p, AIC = AIC(model1))
  frame2 <- data.frame(model = "Last author`s experience", power = p, AIC = AIC(model2))
  frame3 <- data.frame(model = "Peer review time", power = p, AIC = AIC(model3))
  frame4 <- data.frame(model = "Publication date", power = p, AIC = AIC(model4))
  fit <- bind_rows(fit, frame1, frame2, frame3, frame4)
}
# add difference from best
fit = group_by(fit, model) %>%
  mutate(diff = AIC - min(AIC))
arrange(fit, model, diff)

# plot
fplot = ggplot(data = fit, aes(x = factor(power), y = diff))+
  scale_x_discrete()+
  geom_point()+
  geom_line()+
  theme_bw()+
  facet_wrap(~model, scales='free_y')+
  xlab('Power')+
  ylab('Difference in AIC from best model')
fplot
# export
ggsave('figures/3_fractional_polynomial_AIC.jpg', fplot, width=6, height=4, units='in', dpi=500)
