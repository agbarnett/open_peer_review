# ks_test_distribution.R
# KS test to find ideal distribution 
# did not work well, see find_distribition.R instead

# randomly simulate data
x = rnorm(n = 1000, mean = 0, sd = 1.2)
x = rt(n = 1000, df = 2)

# degrees of freedom to try
df = c(1:10,20,40)
fit = NULL
for (d in df){
  ks = ks.test(z, y = 'pt', d, simulate.p.value = TRUE, B = 2000)
  res = data.frame(df = d, D = ks$statistic)
  fit = bind_rows(fit, res)
}
with(fit, plot(df, D))