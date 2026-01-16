# 99_wikipedia_universities.R
# count number of universities; all pages are different
# https://en.wikipedia.org/wiki/Lists_of_universities_and_colleges_by_country
# called by 99_open_science_by_country.R
library(dplyr)

# not used as numbers are not useful because they don't capture all eligible institutions

# Ethiopia
url = 'https://en.wikipedia.org/wiki/List_of_universities_and_colleges_in_Ethiopia'
page = read_html(url)
table = page %>% 
  html_elements("table") %>% # 
  .[1] %>% #  # first table
  html_table(header = TRUE, fill = TRUE) 
table = table[[1]]
f1 = data.frame(country = 'Ethiopia', n_unis = nrow(table))

# Saudi Arabia
url = 'https://en.wikipedia.org/wiki/List_of_universities_and_colleges_in_Saudi_Arabia'
page = read_html(url)
table = page %>% 
  html_elements("table") %>% # 
  .[1] %>% #  # first table
  html_table(header = TRUE, fill = TRUE) 
table = table[[1]] %>%
  filter(City != `University/College`) # remove table headers
f2 = data.frame(country = 'Saudi Arabia', n_unis = nrow(table))

# UK
url = 'https://en.wikipedia.org/wiki/List_of_universities_in_the_United_Kingdom'
page = read_html(url)
table = page %>% 
  html_elements("table") %>% # 
  .[1] %>% #  # first table
  html_table(header = TRUE, fill = TRUE) 
table = table[[1]]
f3 = data.frame(country = 'UK', n_unis = nrow(table))

# South Korea
url = 'https://en.wikipedia.org/wiki/List_of_research_universities_in_South_Korea'
page = read_html(url)
table1 = page %>% 
  html_elements("table") %>% # 
  .[1]%>% #  # first table
  html_table(header = TRUE, fill = TRUE) 
table2 = page %>% 
  html_elements("table") %>% # 
  .[2]%>% #  # 2nd table
  html_table(header = TRUE, fill = TRUE) 
table = bind_rows(table1[[1]], table2[[1]])
f4 = data.frame(country = 'South Korea', n_unis = nrow(table))

# Netherlands
url = 'https://en.wikipedia.org/wiki/List_of_universities_in_the_Netherlands'
page = read_html(url)
table1 = page %>% 
  html_elements("table") %>% # 
  .[2]%>% #  # first table
  html_table(header = TRUE, fill = TRUE) 
table1 = table1[[1]]
table1 = table1[,c(-2,-10)] # remove duplicate column (sub-institute)
table1 =  filter(table1, Name !='')
# remove those that no longer exist
stop = which(table1$Name == 'Universities that no longer exist')
table1 = table1[1:(stop-1),] %>%
  filter(Established!=Name) # remove header rows
f5 = data.frame(country = 'Netherlands', n_unis = nrow(table1))

# Pakistan (split by region)
url = 'https://en.wikipedia.org/wiki/List_of_universities_in_Pakistan'
page = read_html(url)
table1 = page %>% 
  html_elements("table") %>% # 
  .[2] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table1 = table1[[1]]
table2 = page %>% 
  html_elements("table") %>% # 
  .[3] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table2 = table2[[1]]
table3 = page %>% 
  html_elements("table") %>% # 
  .[4] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table3 = table3[[1]]
table4 = page %>% 
  html_elements("table") %>% # 
  .[5] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table4 = table4[[1]]
table5 = page %>% 
  html_elements("table") %>% # 
  .[6] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table5 = table5[[1]]
table6 = page %>% 
  html_elements("table") %>% # 
  .[7] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table6 = table6[[1]]
table7 = page %>% 
  html_elements("table") %>% # 
  .[8] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table7 = table7[[1]]
total_rows = nrow(table1) + nrow(table2)+ nrow(table3) + nrow(table4) + nrow(table5) + nrow(table6) + nrow(table7)
f6 = data.frame(country = 'Pakistan', n_unis = total_rows)

# France
url = 'https://fr.wikipedia.org/wiki/Liste_des_%C3%A9tablissements_publics_%C3%A0_caract%C3%A8re_scientifique,_culturel_et_professionnel' # version in French
page = read_html(url)
table1 = page %>% 
  html_elements("table") %>% # 
  .[1] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table1 = table1[[1]]
table2 = page %>% 
  html_elements("table") %>% # 
  .[2] %>% #  
  html_table(header = TRUE, fill = TRUE) 
table2 = table2[[1]]
table = bind_rows(table1, table2)
f7 = data.frame(country = 'France', n_unis = nrow(table))

# China
#url = ''
# was too hard to download pages as there are too many; instead took estimate from main Wikipedia page
f8 = data.frame(country = 'China', n_unis = 3167)

### combine data
universities = bind_rows(f1, f2, f3, f4, f5, f6, f7, f8)
