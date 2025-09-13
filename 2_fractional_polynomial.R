# 2_fractional_polynomial.R
# fractional polynomials for the three continuous variables
# called by 2_elastic_net_model.R
# Sep 2025

# works on unstransformed variables (have not yet called 2_data_prepare.R)
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
  model2 <- glm(review_available ~ time_power, data = data_p) # 
  model3 <- glm(review_available ~ date_power, data = data_p) # 
  frame1 <- data.frame(model = "Number of authors", power = p, AIC = AIC(model1))
  frame2 <- data.frame(model = "Peer review time", power = p, AIC = AIC(model2))
  frame3 <- data.frame(model = "Publication date", power = p, AIC = AIC(model3))
  fit <- bind_rows(fit, frame1, frame2, frame3)
}
# add difference from best
fit = group_by(fit, model) %>%
  mutate(diff = AIC - min(AIC))
arrange(fit, model, diff)

# plot
library(ggplot2)
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
ggsave('figures/2_fractional_polynomial_AIC.jpg', fplot, width=6, height=4, units='in', dpi=500)
