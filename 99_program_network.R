# 99_program_network.R
# how the R programs link together
# to share on github
# March 2026
library(diagram)
library(stringr)

# to do, need to add results folder
# add more colours to boxes? files that make figures/tables?
# then also add legend

# labels
files = dir(pattern='.R$')
files = files[str_detect(files, '0_|1_|2_|3_|4_|5_')] # should go from zero to five; do not need test files 99_
files = c(files, '3_combine_experience_data.R') # add file from HPC
files = str_remove(files, '\\.R$')
# remove files that are not part of the main flow
remove = c('0_my_openalex_key_do_not_share', # not important for network
           '2_extract_reviews', # for exporting reviews to file
           '2_get_funder_openalex', # was not as useful as XML data
           '4_random_forest', # test file
           '5_plot_transparency' # test file
           )
files = files[!files %in% remove]
# add three data sources (order is important)
sources = c('OpenAlex','PLOS','crossref')
files = c(sources, files)
n_files = length(files)

# data files
data = dir('data', pattern='RData$')
data = str_remove(data, '\\.RData$')
# remove files that are not part of the main flow
dremove = c('2_reviews' # for exporting reviews to file
)
data = data[!data %in% dremove]
data = c(data,'temporary')

# add checks as outputs. other coloured boxes

# positions based on prefix number
pos = as.numeric(str_extract(files, '^[0-9]')) + 2 # + 2 due to zero and data sources
pos = as.numeric(table(pos))
pos = c(3,pos) # add three for data sources

# joins between boxes
M = matrix(nrow = n_files, ncol = n_files, byrow = TRUE, data = 0)
M[which(files=='0_read_xml_data'), which(files=='PLOS')] = "' '" # just arrow
M[which(files=='1_examine_funders'), which(files=='crossref')] = "' '" # just arrow
M[which(files=='2_process'), which(files=='crossref')] = "' '" # just arrow
M[which(files=='2_process'), which(files=='OpenAlex')] = "' '" # just arrow
M[which(files=='2_process'), which(files=='0_read_xml_data')] = paste("'", data[1], "'", sep='')
M[which(files=='1_examine_funders'), which(files=='0_read_xml_data')] = paste("'", data[1], "'", sep='')
M[which(files=='1_validate_outcome'), which(files=='0_read_xml_data')] = paste("'", data[1], "'", sep='')
M[which(files=='2_process'), which(files=='1_examine_funders')] = paste("'", data[2], "'", sep='')
M[which(files=='3_add_author_experience'), which(files=='OpenAlex')] = "' '" # just arrow
M[which(files=='3_add_author_experience'), which(files=='2_process')] = paste("'", data[3], "'", sep='')
M[which(files=='3_patch_author_experience'), which(files=='3_add_author_experience')] = "' '" # just arrow (over-writes data)
M[which(files=='3_validate_data'), which(files=='2_process')] = paste("'", data[3], "'", sep='')
#M[which(files=='4_stability_selection'), which(files=='4_prepare_data')] = ' ' # no data, but a link
#M[which(files=='4_stability_selection'), which(files=='3_add_author_experience.R')] = paste("'", data[3], "'", sep='') # needs to be results 3_plus_experience.RData
#M[which(files=='5_plots.R'), which(files=='4_stability_selection')] = paste("'", data[3], "'", sep='') # needs to be results ests

# box colours
box.colour =rep('grey79', n_files)
box.colour[1:3] = 'darkseagreen2' # for bibliographic data

## make figure 
jpeg('figures/99_program_network.jpg', width=8, height=5, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, 
        pos = pos, 
        name = files, 
        lwd = 0.1, 
        shadow.size=0, 
        dtext = 0.2, # controls the position of arrow text relative to arrowhead
        box.lwd = 2, 
        cex.txt = 1, 
        box.size = 0.14, 
        box.col = box.colour,
        box.type = 'rect', 
        box.prop = 0.20, 
        curve = 0.2,
        arr.pos = 0.4,
        arr.col = 'navy', # colour for arrows
        arr.tcol = 'darkorchid', # colour for text along arrows
        txt.col = 'black') # colour for text in boxes
dev.off()

