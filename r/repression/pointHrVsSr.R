## This scripts plots hard against soft repression to demonstrate
## their asymmetric relationship.
## --- Preliminaries --------------------------------------------
rep <- read.csv(file.path(pathData, 'repressionIndex.csv'))
rep <- within(rep, {                              ## Rescale vars
  sr.index.mean <- sr.index.mean / 10
  hr.index.mean <- hr.index.mean / 10
  }
)
set.seed(6543)                                ## reproduce jitter

## --- proceed to plotting --------------------------------------
library('gridExtra')
p <- ggplot(
  data = rep,
  aes(x = sr.index.mean, y = hr.index.mean), 
) +
geom_jitter(
  position = position_jitter(width = .1, height = .1),
  alpha = .4, size = 4
) +
geom_smooth(                     ## tested spans seq(.4, .7, .05)
                           ## lower spans only create small bumps
  method = 'loess', span = .7, 
  linetype = 'solid', colour = 'black'
) +
geom_abline(                                          ## bisector
  intercept = 0, slope = 1, linetype = 'longdash'
) +
labs(                                             ## plot theming
  x = 'Soft repression',
  y = 'Hard repression'
) +
scale_x_continuous(breaks = seq(0, 10, 2)) +
scale_y_continuous(breaks = seq(0, 10, 2)) +
theme_bw(base_size = 24) +
theme(                             
  axis.ticks = element_line(size = 1),
  axis.ticks.length = unit(.9, units = 'lines'),
  axis.text.x = element_text(vjust = 1),
  axis.title.y = element_text(vjust = .2),
  panel.grid.major = element_line(colour = "grey80"),
  panel.grid.minor = element_line(colour = "grey88"),
  panel.border = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0) +.1, unit = "lines") ## crop marg
)

ggsave(p, file = file.path(pathOut, 'pointHrVsSr.png'), 
  dpi = 700, width = 14, height = 14/1.618, family = "serif"
)
## --- finishing ------------------------------------------------
detach(package:gridExtra)
rm(p)
## END