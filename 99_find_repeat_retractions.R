## 99_find_repeat_retractions.R
# find repeated retractions from same authors for sensitivity analysis
# August 2026
library(dplyr)
library(stringr)
library(openalexR)
source('0_my_openalex_key_do_not_share.R')

# get the data
load('data/3_plus_experience.RData')
retracted = filter(data, ret_type == 'retracted')

# get author IDs from
things_to_keep = c('doi', 'authorships')
all_authors = NULL
re_start = 484
for (k in re_start:nrow(retracted)) {
  res = oa_fetch(
    entity = 'works',
    doi = retracted$doi[k],
    mailto = "a.barnett@qut.edu.au", #
    api_key = my_open_alex_key,
    options = list(select = things_to_keep),
    abstract = FALSE,
    count_only = FALSE,
    verbose = FALSE
  )
  Sys.sleep(1)
  this_author = res$authorships[[1]] %>%
    select(id, display_name) %>%
    mutate(doi = res$doi)
  all_authors = bind_rows(all_authors, this_author)
}

# find heavy retractions
heavy = group_by(all_authors, id) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1:10)

# DOIs for those with 10 or more retractions
frequent_retractions = filter(heavy, !is.na(id), n >= 10) %>%
  left_join(all_authors, by = 'id') %>%
  pull(doi) %>%
  unique() %>%
  str_remove("https://doi.org/")

# save, used by 4_survival_retractions.R
save(frequent_retractions, file = 'data/99_heavy_retractions.RData')
