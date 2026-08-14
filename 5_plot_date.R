# 5_plot_date.R
# plot the continuous estimate for date published
# called by 5_plot_stability_continuous.R
# the policy was introduced in March 2019, short delay to first paper in July 2019
# August 2026

# label for policy change
change = data.frame(date = as.Date('2019-05-22')) %>% # see https://theplosblog.plos.org/2024/01/four-years-of-published-peer-review-history/
  mutate(daten = as.numeric(date))

# create data for predictions
n_predict = 60
new_data = matrix(data = 0, nrow = n_predict, ncol = small_model$rank)
new_data[, 1] = 0 # intercept on = 1 or off = 0 for all
# get range of dates for predictions, then standardised
dstart = as.numeric(as.Date('2019-07-01'))
dend = as.numeric(as.Date('2025-10-01'))
dates = round(seq(dstart, dend, length.out = n_predict))
stan_dates = (as.numeric(dates) - 18000) / 365.25 # scale first to remove huge numbers (as per 2_stability_selection.R)
stan_dates = stan_dates^-1 # best fractional polynomial
stan_dates = (stan_dates - mean_published) / sd_published # further scale because of lasso, see 3_data_prepare.R
index = which(x_selected_names == 'published')
new_data[, index + 1] = stan_dates # only change one column in X (plus 1 for intercept)
# get predictions using matrix multiplication (because glm was fit using matrix)
vcov = vcov(small_model)
vests = as.vector(small_model$coefficients)
mean = new_data %*% vests
var = diag(new_data %*% vcov %*% t(new_data))
pred_date = data.frame(
  published = new_data[, 2],
  mean = mean,
  var = var,
  dates = dates
) %>%
  mutate(
    z = qnorm(0.975),
    lower = mean - z * sqrt(var),
    upper = mean + z * sqrt(var)
  )
# date labels
year_labels = 2019:2025
year_start = as.numeric(as.Date(paste(year_labels, '-01-01', sep = '')))
# policy change label
label = data.frame(
  date = as.Date('2021-01-01'),
  p = -0.18,
  label = 'Open review policy change',
  lower = NA,
  upper = NA
) %>%
  mutate(daten = as.numeric(date))
#
colour = 'orange1'
scatter_date = ggplot(
  data = pred_date,
  aes(x = dates, y = mean, ymin = lower, ymax = upper)
) +
  geom_line(col = colour, linewidth = 1.05) +
  geom_ribbon(alpha = 0.5, fill = colour) +
  geom_hline(lty = 3, yintercept = 0) +
  geom_vline(lty = 2, xintercept = change$daten, col = 'grey55') + # policy change
  geom_text(
    data = label,
    aes(x = daten, y = p, label = label),
    adj = 0,
    col = 'grey55'
  ) +
  geom_segment(
    aes(x = 18600, y = -0.18, xend = 18038 + 4, yend = -0.21),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    col = 'grey55'
  ) +
  scale_x_continuous(breaks = year_start, labels = year_labels, expand = 0.02) +
  xlab(NULL) +
  ylab('Difference in the probability\nof choosing open review') +
  g.theme

## Create the bottom histogram
start_date <- as.Date("2019-07-01")
end_date <- as.Date("2026-01-01")
start_year <- as.Date("2020-01-01")
end_year <- as.Date("2025-01-01")
quarter_breaks <- seq(from = start_date, to = end_date, by = "3 months")
year_breaks <- seq(from = start_year, to = end_year, by = "1 year")
ybreaks = which(quarter_breaks %in% year_breaks)
ylabels = 2020:2025
# create frequencies
for_bar = mutate(data,
                 quarters = cut(ppublished, quarter_breaks)) %>%
  group_by(quarters) %>%
  tally() %>%
  ungroup() %>%
  mutate(num = as.numeric(quarters))

# Create a sequence of quarter start dates
hist_date <- ggplot(for_bar, aes(x = num, y = n)) +
  geom_bar(stat = 'identity', fill = colour, color = "white") +
  scale_x_continuous(
    expand = 0.01,
    minor_breaks = 1:26,
    breaks = ybreaks,
    labels = ylabels
  ) +
  scale_y_continuous(expand = 0.01) +
  ylab('Number of articles') +
  xlab('Date') +
  g.theme

# 3. Stack them vertically using the / operator and adjust relative heights
rplot <- scatter_date / hist_date + plot_layout(heights = c(3, 1))
