# 4_data_prepare_retractions.R
# prepare the data for the retraction models
# not using email domain or funder
# April 2026
# called by 4_survival_retractions.R

# remove small amount of missing country and last authors paper count
data <- filter(data, !is.na(country)) # 103
data <- filter(data, !is.na(author_papers)) # 191
data <- filter(data, lengths(subjects) > 1) # 721 (subject = 'missing' if there's only one)

# create survival outcome
data = mutate(data, 
              open_review = case_when(
                review_available == TRUE ~ 'Yes',
                review_available == FALSE ~ 'No'
              ),
              time_to_retraction = time_to_retraction / 365.25) # convert to years

# scale data; best transformations from 4_fractional_polynomial.R; 
data <- mutate(data,
               # add year published for sensitivity analysis in survival models
               year = format(published, '%Y'),
               # add ORCID proportion (do before author transform)
               p_orcid = (n_orcids+0.5)/(n_authors+0.5), # avoid zero
               p_orcid = sqrt(p_orcid), # best fractional polynomial
               author_papers = log2(author_papers+1), # best fractional polynomial
               n_authors = n_authors^-2, # best fractional polynomial
               published = (as.numeric(published) - 18000)/365.25, # scale first to remove huge numbers
               published = published ^-2, # best fractional polynomial
               time_between = sqrt(time_between) # square-root transformation
               ) 
## further scale by standardising because we use lasso
mean_orcid = mean(data$p_orcid) # need to store these for plots (see 4_plot_stability.R)
sd_orcid = sd(data$p_orcid)
mean_published = mean(data$published)
sd_published = sd(data$published)
data = mutate(data, 
              author_papers = scale(author_papers),
              published = scale(published),
              time_between = scale(time_between),
              n_authors = scale(n_authors),
              p_orcid = scale(p_orcid)) 

## make binary matrices
# country
ctab <- table(unlist(data$country))
ctab.min <- ctab[ctab >= 100] # only countries with over 100 results
countries_to_use <- names(ctab.min)
country_mat <- t(+sapply(data$country, "%in%", x = countries_to_use)) # binary matrix

# article type
type_mat <- model.matrix(~ -1 + type, data = data) # binary matrix

## subjects (just those with 1,000+ mentions)
subjects = lapply(data$subjects, unique) # Remove double subjects per article
tab <- table(unlist(subjects))
tab.min <- tab[tab >= 500] # only subjects with over 500 results (861 columns)
cat('We used ', length(tab.min), ' subjects out of ',length(tab),'.\n', sep='')
subjects_to_use <- names(tab.min)
subject_mat <- t(+sapply(data$subjects, "%in%", x = subjects_to_use)) # binary matrix; takes a while
# remove subjects that are perfectly correlated (see 99_correlations.R)
# get list of subjects to remove due to correlation
source('99_subjects_to_remove.R')
index = subjects_to_use %in% to_remove
subject_mat = subject_mat[, !index]
subjects_to_use = subjects_to_use[!subjects_to_use %in% to_remove]
cat('There were ', length(to_remove), ' subjects removed for high correlation.\n', sep='')

## make matrix for "simpler" predictors
depvars <- c("review_available", "published", "time_between", "n_authors", "author_papers", 'p_orcid')
x_others <- select(data, all_of(depvars))

# combine predictors (x matrix) and add variable names
x <- bind_cols(x_others, type_mat, country_mat, subject_mat)
x <- as.matrix(x)
country_mat_names <- paste("country_", countries_to_use, sep = "") # need country prefix to avoid clashes with subjects
subject_mat_names <- paste("subject_", subjects_to_use, sep = "") #
colnames(x) <- c(depvars, colnames(type_mat), country_mat_names, subject_mat_names)

# tidy up variable names
pred = colnames(x)
pred = str_replace_all(pred, ' ', '_') # clean up names
pred = str_remove_all(pred, "'")
pred = str_remove_all(pred, "-")
colnames(x) = pred

# dependent variable
y <- data$retracted

# check
if(nrow(x) != length(y)){cat('Error, mismatch in x and y.\n')}
