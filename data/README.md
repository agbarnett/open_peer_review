# PLOS open review data

The data are based on the full corpus of PLOS papers available [here](https://plos.org/text-and-data-mining/).

## Files

* 0_unprocessed.RData, after reading in the XML data for the full PLOS corpus
* 1_processed.RData, processed data that includes citations and retractions, and authors' countries
* 1_funder_info.RData, 
* 2_plus_experience.RData, further processed by adding author's experience from _OpenAlex_
* all_countries.csv, data on all countries used to help with assigning authors' countries
* pone.0281659.s001.xlsx, is Excel data from DOI: [10.1371/journal.pone.0281659](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0281659) and is used to verify our estimates of the country-level rate of preprints
* funderNames.csv, is the funder ID numbers and names

## Data dictionary for analysis data

* doi = digital object identifier (character)
* journal = PLOS journal, e.g., PLOS ONE (character)
* type = article type, e.g., research article (character)
* n_authors = number of authors (integer)
* subject = articles subjects (list format) 
* received = date paper was received at journal (date)
* published = date paper was published (date)
* review_available = open peer review report available (dependent variable, TRUE/FALSE)
* domain = email domain (list format)
* country = authors' countries (list format)
* citations = citation counts from _OpenAlex_ (integer)
* oa_retracted = retracted according to _OpenAlex_ (TRUE/FALSE)
* retracted = retracted according to _RetractionWatch_ database on _CrossRef_  (TRUE/FALSE)
* time_to_retraction = time from publication to retraction or right-censoring (days)
* time_between = peer review time, the time between submission and acceptance (days)
* experience = last author's paper numbers at submission (integer)