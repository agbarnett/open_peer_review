# 3_data_prepare.R
# prepare the data for the models
# September 2025
# called by 2_stability_selection.R and table?

# remove small amount of missing country and last authors paper count
data <- filter(data, !is.na(country)) # 98
data <- filter(data, !is.na(author_papers)) # 111

# scale data; best transformations from 3_fractional_polynomial.R; no clear minimum for time between (peer review time), so left as linear
data <- mutate(data,
               author_papers = log2(author_papers+1), # best fractional polynomial
               n_authors = sqrt(n_authors+1), # best fractional polynomial
               published = (as.numeric(published) - 18000)/365.25, # scale first to remove huge numbers
               published = published ^-1, # best fractional polynomial
               time_between = time_between # no transformation
) 
# further scale by standardising because we use lasso
data = mutate(data,
              author_papers = scale(author_papers),
              published = scale(published),
              time_between = scale(time_between),
              n_authors = scale(n_authors))

## make binary matrices
# domain
domains_available <- sort(unique(unlist(data$edomain)))
edomain_mat <- t(+sapply(data$edomain, "%in%", x = domains_available)) # binary matrix
# country
ctab <- table(unlist(data$country))
ctab.min <- ctab[ctab >= 100] # only countries with over 100 results
countries_to_use <- names(ctab.min)
country_mat <- t(+sapply(data$country, "%in%", x = countries_to_use)) # binary matrix
# article type
type_mat <- model.matrix(~ -1 + type, data = data) # binary matrix
## subjects (just those with 1,000+ mentions)
tab <- table(unlist(data$subjects))
tab.min <- tab[tab >= 1000] # only subjects with over 1,000 results
subjects_to_use <- names(tab.min)
subject_mat <- t(+sapply(data$subjects, "%in%", x = subjects_to_use)) # binary matrix; takes a while

# make matrix for "simpler" predictors
depvars <- c("published", "time_between", "n_authors","author_papers")
x_others <- select(data, all_of(depvars))

# combine predictors (x matrix) and add variable names
x <- bind_cols(x_others, type_mat, edomain_mat, country_mat, subject_mat)
x <- as.matrix(x)
country_mat_names <- paste("country_", countries_to_use, sep = "") # need country prefix to avoid clashes with subjects
subject_mat_names <- paste("subject_", subjects_to_use, sep = "") #
domain_mat_names <- paste("domain_", domains_available, sep = "") #
colnames(x) <- c(depvars, colnames(type_mat), domain_mat_names, country_mat_names, subject_mat_names)

# tidy up variable names
pred = colnames(x)
pred = str_replace_all(pred, ' ', '_') # clean up names
pred = str_remove_all(pred, "'")
pred = str_remove_all(pred, "-")
colnames(x) = pred

# dependent variable
y <- data$review_available
