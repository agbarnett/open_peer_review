# open_alex_experience.R
# function to get openalex data in batches
# Feb 2026

get_alex_experience = function(dois, dates, oa_key){
  
  # checks
  if(any(is.na(dates)) | any(is.null(dates))){stop('Missing dates')}
  if(any(is.na(dois)) | any(is.null(dois))){stop('Missing DOIs')}

  n_input = length(dois)
  all_data = NULL
  
  # search for papers to get author IDs (request to OpenAlex)
  paper_data = oa_fetch(entity = 'works',
                        abstract = FALSE,
                        mailto = 'a.barnett@qut.edu.au', 
                        api_key = oa_key,
                        options = list(select = c("doi", "authorships","corresponding_author_ids")), # only need limited data
                        doi = dois)
  # find duplicate
  if(lengths(paper_data)[1] > n_input){
    cat('Extra DOI from open alex.\n')
    to_compare = str_remove(paper_data$doi, pattern = 'https://doi.org/')
    # find duplicate and remove
    index = which(duplicated(to_compare))
    temp1 = paper_data$doi[-index] # bit clunky, but had to make temporary lists as overwriting did not work
    temp2 = paper_data$authorships[-index]
    paper_data = NULL
    paper_data$doi = temp1
    paper_data$authorships = temp2
  }
  # find missing
  if(lengths(paper_data)[1] < n_input){
    cat('Missing DOI from open alex.\n')
    to_compare = str_remove(paper_data$doi, pattern = 'https://doi.org/')
    # find missing and add to data
    missing_dois = dois[!dois %in% to_compare] 
    fframe = data.frame(doi = missing_dois, author_papers = NA)
    all_data = bind_rows(all_data, fframe)
    # reduce loop count used below:
    n_input = n_input - length(missing_dois) 
  }
  
  # loop through papers
  for (paper_loop in 1:n_input){
    this_doi = str_remove(paper_data$doi[[paper_loop]], pattern = 'https://doi.org/')
    # get track record for last author
    these_authors = paper_data$authorships[[paper_loop]]
    if(is.null(nrow(these_authors))){ # rare occasion with no authors, e.g., 10.1371/journal.pcbi.1012260
      fframe = data.frame(doi = this_doi, atype = 'missing', author_papers = NA)
      all_data = bind_rows(all_data, fframe)
      next
    }
   if(nrow(these_authors) == 0){ # rare occasion with no authors, e.g., 10.1371/journal.pcbi.1012260 (after null above)
      fframe = data.frame(doi = this_doi, atype = 'No authors', author_papers = NA)
      all_data = bind_rows(all_data, fframe)
      next
    }
     last_author = filter(these_authors, author_position == 'last')
    id = str_remove(last_author$id, pattern='https://openalex.org/') # Author ID from Open Alex
    if(length(id)==0){id = NA}
    atype = 'last' # keep what type of author
    if(is.na(id) == TRUE){ # if last missing, use first author
      first_author = filter(these_authors, author_position == 'first')
      id = str_remove(first_author$id, pattern='https://openalex.org/') # Author ID from Open Alex
      if(length(id)==0){id = NA}
      atype = 'first'
    }
    if(is.na(id) == TRUE){ # if first missing, use random author
      random_author = filter(these_authors, !is.na(id)) %>%
        sample_n(1)
      id = str_remove(random_author$id, pattern='https://openalex.org/') # Author ID from Open Alex
      atype = 'random' # keep what type of author
    }
    query = paste("https://api.openalex.org/works?filter=author.id:", id, sep='')
    # get all papers by first author (request to OpenAlex)
    result = tryCatch(oa_request(query,
                                 mailto = 'a.barnett@qut.edu.au'), error = function(e) { NULL })
    # loop through to process all papers by author
    n_papers = length(result)
    aframe = NULL
    if(n_papers == 0){ # if there are no papers
      fframe = data.frame(doi = this_doi, atype = 'No papers', author_papers = 0)
      all_data = bind_rows(all_data, fframe)
      next
    }
    for (k in 1:n_papers){
      this_frame = data.frame(date = result[[k]]$publication_date)
      aframe = bind_rows(aframe, this_frame)
    }
    # now count papers prior to submission
    submitted.date = as.Date(dates[paper_loop])
    author_count = filter(aframe, date <= submitted.date) %>%
      nrow()
    # final frame
    fframe = data.frame(doi = this_doi, atype = atype, author_papers = author_count)
    all_data = bind_rows(all_data, fframe)
    
    # short pause to avoid time-outs (time based on trial and error)
    #Sys.sleep(2) # not needed after paid API
    
  } # end of paper_loop
  
  return(all_data)
  
}

