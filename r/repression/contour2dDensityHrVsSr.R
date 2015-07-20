## This scripts plots hard against soft repression 
## using a bivariate density contour map.

## --- Preliminaries ---------------------------------------
rep <- read.csv(file.path(pathData, 'repressionIndex.csv'))
rep <- within(rep, {                         ## Rescale vars
  sr.index.mean <- sr.index.mean / 10
  hr.index.mean <- hr.index.mean / 10
  }
)
library('RColorBrewer')
rf <- colorRampPalette(brewer.pal(9,'Greys')) # define color
r <- rf(24)                 # interpolate colors in 24 steps


library('MASS')
pdta <- subset(                         ## drop missing data
  rep, !is.na(sr.index.mean + hr.index.mean)
)
k <- with(pdta,                ## estimate density, 200 bins
  kde2d(sr.index.mean, hr.index.mean, n = 200)
)
smoothed.dta <- with(pdta,                       ## smoother
  loess.smooth(
    x = sr.index.mean, y = hr.index.mean, span = .7
  )
)

## --- generate plot ---------------------------------------
png(
  file = file.path(pathOut, 'density2dSmooth.png'),
  width = 7, height = 7/1.618, units = 'in', res = 600,
  family = 'Times'
)
par(mar = c(4, 4, 0, 0)+.1)

with(pdta, 
  plot(                                        ## empty plot
    x = sr.index.mean, y = hr.index.mean, 
    type = 'n', bty = 'n',
    xaxt = 'n', yaxt = 'n',
    xlab = 'Soft Repression', ylab = 'Hard Repression'
  )
)
image(k, col = r, add = TRUE)                 ## contour map
grid(lty = 3, lwd = .5, col = 'black')         ## grid lines
abline(a = 0, b = 1, lty = 2)                    ## bisector
with(smoothed.dta, lines(x = x, y = y))
axis(                                              ## x-axis
  side = 1, at = seq(0, 10, 2), las = 1,
  col = 'white', col.ticks = 'black'
)
axis(                                              ## y-axis
  side = 2, at = seq(0, 10, 2), las = 1,
  col = 'white', col.ticks = 'black'
)
dev.off()

## -- clean workspace --------------------------------------
rm(smoothed.dta, pdta, rf, r, rep)
detach(package:MASS)