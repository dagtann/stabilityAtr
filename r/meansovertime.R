## === Preliminaries =======================================
set.seed(6543)

## === Load data ===========================================
leg <- read.csv(file.path(pathData, 'legitimationIndex.csv'))
rep <- read.csv(file.path(pathData, 'repressionIndex.csv'))

## === Draw plots function =================================
drawPlots <- function() {
## --- Subset data -----------------------------------------
  require(reshape)
  # df <- leg
  # df$indicator <- df$hr.index.mean / 10
  # df$indicator <- df[, "legit"] * 10 
  df <- subset(df, 
    subset = year >= 1980,                    ## Limit years
    select = c("cowcode2", "year", "indicator")
  )
  df.all <- df               ## df.all as data for histogram
  df <- within(df, {             ## count countries p.y. !NA
    count <- ave(indicator, year, 
      FUN = function(x){sum(!is.na(x))}
    )
    n <- ave(cowcode2, year, 
      FUN = function(x){length(unique(x))}
    )
  }
  )
  
  exclude <- names(df) %in% "cowcode2" 
  df <- df[!exclude]                ## drop cowcode2 from df
  rm(exclude)
  
  df <- melt(df, id.vars  = c("year", "count", "n"))  
  df <- cast(df,           ## give mean and sd for each year
    year + count + n ~ variable,
    fun.aggregate = c(mean, sd),
    na.rm = TRUE
  )
  names(df)[4:5] <- c("m", "sd")  ## just give cleaner names
  df <- within(df,                    ## give .95 multiplier
    mult <- qt(p = .975, df = count-1)       ## alpha = .05, 
                                   ## t-dist with count-1 DF
  )
  detach(package:reshape)
  
  ## --- Make plot ----------------------------------------------
  require('ggplot2')
  require('ggthemes')
  require('grid')
  require('scales')
  
  p <- ggplot(                   ## p <- main over time plot
    aes(x = year, y = m), data = df
  ) +
  geom_linerange(              ## add t-confidence intervals
    aes(
      ymin = m - mult*sd/sqrt(count), 
      ymax = m + mult*sd/sqrt(count) 
    )
  ) +
  geom_point(aes(size = n)) +       ## add point & size by n
  geom_jitter(
    data = df.all, 
    aes(x = year, y = indicator, size = NULL), 
    position = position_jitter(width = .1, height = .1),
    size = 4, alpha = .2
  ) +
  scale_size_continuous(   ## and adjust size scale & labels
    range  = c(4, 7), 
    breaks = c(70, 80, 90, 100, 110),
    labels = c(
      "70"  = expression(NULL%~~%"70"), 
      "80"  = expression(NULL%~~%"80"), 
      "90"  = expression(NULL%~~%"90"), 
      "100" = expression(NULL%~~%"100"),
      "110" = expression(NULL%~~%"110")
    )
  ) +
  guides(                ## set size legend marker to circle
    size = guide_legend(override.aes = list(shape = 19))
  ) +
  scale_y_continuous(    ## adjust y-scale ticks by range of
    limits = c(-.2, 10.2),
    breaks = seq(0, 10, 1)
      ##Original code, commented out by D.T., 24.02.2014
      # limits = c(round(range(df$m)[1], 0)-1, ## country year means
      #   round(range(df$m)[2], 0)+1),
      # breaks = seq(round(range(df$m)[1], 0)-1, 
      #  round(range(df$m)[2], 0)+1, 1)
    ) +
    scale_x_continuous(              ## adjust x-scale ticks
      limits = c(1979.8, 2010.2) , 
      breaks = seq(1980, 2010, 5)
    ) +
    labs(                                       ## set plot labels (take label from top)
      x = "",
      y = "",
      ## Original code, commented out by D.T., 24.02.2014
      ## y = paste( label, "(mean)", sep=" "),
      size = "N"
    ) 
  
  pt <- p +                                ## give p a theme
    theme_bw(base_size = 48) +
    theme(                             
      legend.justification = c(0, 1), 
      legend.position = c(0.01, .99),
      legend.direction = "horizontal",
      legend.margin = unit(-0.6, "cm"),
      legend.key = element_blank(),
      legend.background = element_rect(fill=alpha('blue', 0)),
      axis.ticks = element_line(size = 1),
      axis.ticks.length = unit(.9, units = 'lines'),
      axis.text.x = element_text(vjust = 1),
      axis.title.y = element_text(vjust = .2),
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_line(colour = "grey88"),
      panel.border = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0) +.1, unit = "lines")
    )
  
  h <- ggplot(
    data = df.all, aes(x = factor(0), y = indicator)
  ) + 
  geom_tufteboxplot(outlier.size = 4, fatten = 8) +
  scale_x_discrete(breaks = 0, label = "") +
  scale_y_continuous(              ## adjust y-scale ticks
    limits = c(-.2, 10.2), breaks = c(0:10), 
    label = rep("", 11)
  ) +
  labs(x = "", y = "")
  
  ht <- h +
  theme_bw(base_size = 48) +
  theme(                     ## finishing touches to theme
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = NA),
    axis.ticks = element_blank(),      
    axis.ticks.length = unit(.9, units = 'lines'),
    plot.margin = unit(c(0, -20, 0, -20), units = "lines")
  )
  
  require(gridExtra)
  png( 
    file = file.path(pathOut, paste0('meanv2_', todo[i], '.png')), 
    width = 2048, height = 1265.76,
    units = "px",
    family = "serif"
  )
  
  print(
    grid.arrange(pt, ht, 
      ncol=2, nrow=1, widths=c(1, .02)
    )
  )
  dev.off()
  
  detach(package:gridExtra)
  detach(package:grid)
  detach(package:ggthemes)
  detach(package:ggplot2)
  
  rm(pt, ht, p, h )
}

## === Set loop   ==============================================
todo <- c( "hr" , "sr", "ss" )
for(i in 1:length(todo)) {
  if ( todo[i] == "hr" ) {
    label    <- "Hard Repression"               ## Plot notes
    df       <- rep                             ## Original data
    df$indicator <- df[, "hr.index.mean"] / 10  ## Indicator to choose
  }
  if ( todo[i] == "sr" ) {
    label    <- "Soft Repression"
    df       <- rep
    df$indicator <- df[, "sr.index.mean"] / 10 
  }
  if ( todo[i] == "ss" ) {
    label    <- "Specific Support"
    df       <- leg
    df$indicator <- df[, "legit"] * 10 
  }
  drawPlots()                                   ## Call the drawPlots function
}
rm(i)
## END