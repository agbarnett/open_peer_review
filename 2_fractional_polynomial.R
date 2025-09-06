# 2_fractional_polynomial.R
# number of authors is very skewed, so first try fractional polynomial; try for peer review time too
# called by 2_elastic_net_model.R
# Sep 2025

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
                   )
  )
  model1 <- glm(review_available ~ author_power, data = data_p) # adjust for date
  model2 <- glm(review_available ~ time_power, data = data_p) # adjust for date
  frame1 <- data.frame(model = "authors", power = p, AIC = AIC(model1))
  frame2 <- data.frame(model = "time between", power = p, AIC = AIC(model2))
  fit <- bind_rows(fit, frame1, frame2)
}
group_by(fit, model) %>%
  mutate(diff = AIC - min(AIC)) %>%
  arrange(model, diff)
