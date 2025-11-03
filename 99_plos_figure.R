# 99_plos_figure.R
# plot the peer review process at PLOS in a diagram
# Nov 2025
library(stringr)
library(diagram)

l1 = "Authors submit a paper to a PLOS journal"
l2 = "Selected papers are sent for review"
l3 = 'Authors respond to peer review comments, potentially with multiple rounds'
l4 = 'Authors of accepted papers consent or not to open peer review'
# wrap text by consistent amount
char_wrap = 20
l1 = str_wrap(l1, width = char_wrap)
l2 = str_wrap(l2, width = char_wrap)
l3 = str_wrap(l3, width = char_wrap)
l4 = str_wrap(l4, width = char_wrap)
#
labels = c(l1, l2, l3, l4)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=TRUE, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.75	white	square	0.66	0.18
2	0.75	0.75	white	square	0.66	0.18
3	0.75	0.25	white	square	0.66	0.18
4	0.25	0.25	white	square	0.66	0.18
')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = nrow(frame), ncol = nrow(frame), byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 2] = "' '"
M[4, 3] = "' '"

## make figure 
jpeg('figures/99_plos_diagram.jpg', width=5, height=3, units='in', res=500, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, arr.pos = 0.5,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop)
dev.off()

