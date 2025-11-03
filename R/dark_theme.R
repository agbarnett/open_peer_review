# dark_theme.R
# dark theme for ggplot used for slides
# October 2025

dark.theme = theme(panel.background = element_rect(fill = 'black'),
      legend.title = element_text(colour = 'white'),
      legend.text = element_text(colour = 'white'),
      text = element_text(colour = 'white'), # does not change geom_text
      axis.text.x = element_text(colour = 'white'),
      axis.title.x = element_text(colour = 'white'),
      axis.text.y = element_text(colour = 'white'),
      axis.title.y = element_text(colour = 'white'),
      axis.line.x = element_line(colour='white'),
      axis.line.y = element_line(colour='white'),
      plot.background = element_rect(fill='black', colour='transparent'), # transparent removes lines around plot
      legend.background = element_rect(fill = 'black', colour='transparent'),
      panel.grid.major = element_line(linewidth=0.2)) # narrower lines even for major

