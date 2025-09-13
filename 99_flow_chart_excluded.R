# 99_flow_chart_excluded.R
# consort flow chart of numbers included
# Sep 2025
library(diagram)
library(dplyr)
library(stringr)

# for exclusion numbers
load('data/0_unprocessed.RData')

# number of XML files downloaded from PLOS
n_start = 386179
#
# exclusion reasons; just up to top ten
n_excluded = nrow(excluded)
reasons = group_by(excluded, reason) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(row = 1:n(),
         rown = ifelse(row > 10, 11, row),
         reason = ifelse(row > 10, 'Other non-peer reviewed\narticle', reason)) %>%
  group_by(rown, reason) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = str_squish(format(n, big.mark=',')))

#
n_data = nrow(data)
n_data + n_excluded
if(n_data + n_excluded != n_start - count_corrections){cat('Number error\n')}

# missing country
load('data/1_processed.RData')
n_miss = sum(is.na(data$country))
n_analysis = nrow(data) - n_miss

# labels
l1 = paste('XML files download\nfrom PLOS site (n=', format(n_start, big.mark=','), ')', sep='') # 
l2 = paste('XML file name included\n`correction` (n=', format(count_corrections, big.mark=','), ')', sep='') # 
l3 = paste('XML files processed\n(n=', format(n_start-count_corrections, big.mark=','), ')', sep='')
l4 = paste('Excluded (n=', format(n_excluded, big.mark=','), ')\n', 
           '- ', slice(reasons, 1)%>%pull(reason), ' (n=', slice(reasons, 1) %>%pull(n), ')\n',
           '- ', slice(reasons, 2)%>%pull(reason), ' (n=', slice(reasons, 2) %>%pull(n), ')\n',
           '- ', slice(reasons, 3)%>%pull(reason), ' (n=', slice(reasons, 3) %>%pull(n), ')\n',
           '- ', slice(reasons, 4)%>%pull(reason), ' (n=', slice(reasons, 4) %>%pull(n), ')\n',
           '- ', slice(reasons, 5)%>%pull(reason), ' (n=', slice(reasons, 5) %>%pull(n), ')\n',
           '- ', slice(reasons, 6)%>%pull(reason), ' (n=', slice(reasons, 6) %>%pull(n), ')\n',
           '- ', slice(reasons, 7)%>%pull(reason), ' (n=', slice(reasons, 7) %>%pull(n), ')\n',
           '- ', slice(reasons, 8)%>%pull(reason), ' (n=', slice(reasons, 8) %>%pull(n), ')\n',
           '- ', slice(reasons, 9)%>%pull(reason), ' (n=', slice(reasons, 9) %>%pull(n), ')\n',
           '- ', slice(reasons, 10)%>%pull(reason), ' (n=', slice(reasons, 10) %>%pull(n), ')\n',
           '- ', slice(reasons, 11)%>%pull(reason), ' (n=', slice(reasons, 11) %>%pull(n), ')',
           sep='')
l5 = paste('Remaining articles\n(n=', format(n_data, big.mark=','), ')', sep='')
l6 = paste('Missing country\n(n=', n_miss,')', sep='')
l7 = paste('Analysis data\n(n=', format(n_analysis, big.mark=','), ')', sep='')
null = '' # for arrow placements

labels = c(l1, l2, l3, l4, l5, l6, l7, null, null, null)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.94	white	square	0.24	0.17
2	0.75	0.82	white	square	0.19	0.2
3	0.25	0.7	white	square	0.24	0.17
4	0.75	0.5	white	square	0.96	0.2
5	0.25	0.3	white	square	0.24	0.17
6	0.75	0.2	white	square	0.17	0.2
7	0.25	0.1	white	square	0.24	0.17
8	0.25	0.82	transparent	square	0	0
9	0.25	0.5	transparent	square	0	0
10	0.25	0.2	transparent	square	0	0')
pos = as.matrix(subset(frame, select=c(x, y)))
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[3, 1] = "' '"
M[5, 3] = "' '"
M[7, 5] = "' '"
M[2, 8] = "' '"
M[4, 9] = "' '"
M[6, 10] = "' '"

jpeg('figures/99_consort_flow.jpg', width=7, height=6.5, units='in', res=500, quality=100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
dev.off()
