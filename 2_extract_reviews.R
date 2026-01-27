# 2_extract_reviews.R
# extract a random sample of the text of reviews
# xml data from here https://plos.org/text-and-data-mining/
# January 2026
library(dplyr)
library(stringr)
library(rvest) # for reading XML files

# sample size per year; years to use
n_sample = 100
years = 2023:2025

# get data from 1_process.R
load('data/1_processed.RData')
TeachingDemos::char2seed('rotherham')
to_sample = filter(data, review_available) %>%
  mutate(year_published = as.numeric(format(published, '%Y'))) %>%
  filter(year_published %in% years) %>%
  group_by(year_published) %>%
  sample_n(size = n_sample) %>%
  ungroup()
# table(to_sample$year_published) # check
sampled_dois = pull(to_sample, doi)

# file location
here = getwd()
xml_location = "C:/Users/barnetta/Downloads/allofplos" # had to put on C drive because could not unzip on U drive
setwd(xml_location)

# remove standard instructions
inst_pattern = c('While revising your submission, ',
                 '^Data Requirements:$',
                 '^Figure Files:$',
                 '^.Press.$',
                 '^Reproducibility:$',
                 '^References:$',
                 '^CODE POLICY.?$',
                 '^DATA POLICY.?$',
                 'plos-journals-now-open-for-published-peer-review', # link to peer review blog
                 'journals.plos.org/plosbiology/s/', 
                 '@plos.org,?$',
                 ', PhD$', # to remove editor's name
                 '^Journal Requirements.?$',
                 '^(.. )?.Blot and Gel Data Policy.$',
                 '^(.. )?.Protocols deposition.$',
                 '^(.. )?.Re.?submission Checklist.$',
                 '^(.. )?.PLOS Data Policy.$',
                 '^BLOT AND GEL REPORTING REQUIREMENTS:$',
                 "^(Warm|Best|Kind) regards,",
                 "^Sincerely,",
                 "^(Academic|Senior|Section|Guest) Editor",
                 "^Editor-in-Chief$",
                 "^Staff$",
                 '^Comments to the Author',
                 '^The reviews are attached below\\. ',
                 "^PLOS (Biology|ONE|Computational Biology|Digital Health)$",
                 "^Submitted filename: ",
                 '^Please note that per journal policy, ',
                 'Thank you again for your submission to our journal',
                 'Thank you for your patience while your manuscript',
                 'You.ll see that the reviewers are all broadly positive,',
                 'When you are ready to resubmit your revised manuscript',
                 'please also upload a .track.changes. version of your manuscript',
                 '^We’re pleased to inform you that your manuscript has been judged',
                 '^Within one week, you’ll receive an e-mail detailing the required',
                 'We cannot make any decision about publication until we have seen the revised manuscript',
                 'When you are ready to resubmit, please upload',
                 'Two versions of the revised manuscript: one with either highlights',
                 'Important additional instructions are given below your reviewer comments',
                 'Please prepare and submit your revised manuscript within ',
                 'We hope that our editorial process has been constructive so far',
                 'Editorial Note: Please make a final read and edit of the paper',
                 'we would like to invite you to revise the work to (thoroughly )?address',
                 '^An invoice for payment will follow shortly after the formal acceptance',
                 '^If your institution or institutions have a press office',
                 'To submit a revised version of your manuscript',
                 'Having discussed the reviews with the Academic Editor',
                 'this should detail your responses to the editorial requests',
                 'In your point.by.point response to the reviewers',
                 'Please note here if the review is uploaded as an attachment',
                 'Please make sure to read the following important policies and guidelines',
                 'You should also cite any additional relevant literature that has been published',
                 'you may have the opportunity to make the peer review history publicly available',
                 'The manuscript must describe a technically sound piece of scientific research with data that supports the conclusions',
                 'The PLOS Data policy requires authors to make all data underlying the findings described in their manuscript fully available',
                 'Please note that as a condition of publication PLOS. data policy',
                 # 'PLOS authors have the option to publish the peer review history of their article ', # keep because it gives context to answers
                 'PLOS \\w (\\w )?does not copyedit accepted manuscripts, so the language in submitted articles must be clear',
                 'Please use the space provided to explain your answers to the questions above',
                 'The PLOS Data policy requires authors to make all data and code underlying the findings described in their manuscript fully available',
                 'Do you want your identity to be public for this peer review',
                 'If you choose .no., your identity will remain anonymous',
                 'Please ensure that all data files are uploaded as .Supporting Information.',
                 'Note that we do not require all raw data',
                 'You may be aware of the PLOS Data Policy,',
                 'Please also provide the accession code or a reviewer link so that we may view your data before publication',
                 'NOTE: the numerical data provided should include all replicates',
                 'Please also ensure that figure legends in your manuscript include information',
                 'Please ensure that your Data Statement in the submission system accurately',
                 'We appreciate that these requests represent a great deal of extra work',
                 'your manuscript remains formally under active consideration at our journal',
                 'If you need to cite a retracted article, indicate the',
                 'Please add the links to the funding agencies in the Financial Disclosure statement',
                 'Thank you for your patience while we considered your revised manuscript',
                 'Your manuscript has been evaluated by the PLOS Biology editors',
                 'Based on their specific comments and following discussion with the Academic Editor',
                 'To enhance the reproducibility of your results, we recommend that',
                 'Please note that, as a condition of publication, PLOS',
                 'Review your reference list to ensure that it is complete and correct',
                 'Thank you very much for submitting your manuscript ',
                 '^We expect to receive your revised manuscript within ',
                 'To submit your revision, please go to ',
                 'If Supporting Information files are included with your article',
                 'a cover letter that should detail your responses to any editorial requests',
                 'a Response to Reviewers file that provides a detailed response',
                 'a track.changes file indicating any changes that you have made to the manuscript',
                 'As with all papers reviewed by the journal, your manuscript ',
                 '\\[NOTE: If reviewer comment',
                 'Use this section to provide overall comments, discuss strengths',
                 'Use this section for editorial suggestions as well as relatively minor modifications',
                 'as the code that you have generated is important to support',
                 'please take this last chance to review your reference list',
                 'Your revisions should address the specific points made by each reviewer',
                 'IMPORTANT - SUBMITTING YOUR REVISION',
                 'We are pleased to inform you that your manuscript',
                 'Please note that your manuscript will not be scheduled for publication until you have made the required changes',
                 'Before your manuscript can be formally accepted ',
                 'The editorial review process is now complete',
                 'we are looking forward to publishing your work in PLOS',
                 'We look forward to receiving your revised manuscript',
                 'Note: HTML markup is below.',
                 'provided you satisfactorily address the following',
                 'press office or the journal office choose to press release your paper',
                 'Please do not hesitate to contact me should you have any questions',
                 'we cannot make a decision about publication until we have seen the revised manuscript',
                 'if you have any questions or concerns, or would like to request an extension',
                 'uncropped and minimally adjusted images supporting all blot and gel results')
inst_pattern = paste(inst_pattern, collapse='|')

# get review text from XML
review_tibble = NULL
for (this_doi in sampled_dois){
  # process the ID
  this_doi = str_remove(this_doi, '10.1371/')
  xml_name = paste(this_doi, '.xml', sep='')
  # read the full XML
  article <- read_html(xml_name) 
  # extract the review
  review <- article %>%
    html_elements("sub-article[article-type='aggregated-review-documents']") %>%
    html_elements('p') %>% # remove front-stub
    html_text()
  index = str_detect(review, pattern = inst_pattern)
  review = review[!index]
  
  # concatenate
  this_row = tibble(id = this_doi, review = review)
  review_tibble = bind_rows(review_tibble, this_row)
  review = NULL
}

# save
setwd(here)
save(review_tibble, file = 'data/2_reviews.RData')

# export to text files, one for each review
for (this_doi in sampled_dois){
  # process file name
  this_doi = str_remove(this_doi, '10.1371/')
  outfile = paste('reviews/', this_doi, '.txt', sep='')  
  # select the review and add carriage returns
  review = filter(review_tibble, id == this_doi) %>% pull(review)
  review = paste(review, collapse='\n')
  # write to file
  zz <- file(outfile, "w")  # open an output file connection
  cat(review, file = zz)
  close(zz)
}
