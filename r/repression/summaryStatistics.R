## === Summary statistics for repression concept chapter ========
## === load data ================================================
#rep <- read.csv("./data/repressionIndex.csv")
names(rep)

sumStat <- function(x, ...) {
  out <- cbind(
    min = min(x, ...), max = max(x, ...), mean(x, ...), 
    N = sum(is.na(x) == FALSE)
  )
  out <- round(out, digits = 2)
  return(out)
}

apply(rep[, 
  c("ciri.assn", "cld.freass", "ciri.speech", "cld.freexp",
    "ciri.elecsd", "p.parcomp")], 2,
  sumStat,
  na.rm = TRUE
)

apply(
  rep[, 
    c("std.ciri.assn", "std.cld.freass", "std.ciri.speech", 
      "std.cld.freexp", "std.ciri.elecsd", "std.p.parcomp", 
      "sr.index.mean"
    )
  ], 2,
  sumStat,
  na.rm = TRUE
)

apply(
  rep[, 
    c("ciri.physint", "gd.pts")
  ], 2,
  sumStat,
  na.rm = TRUE
)

apply(
  rep[, 
    c("std.ciri.physint", "std.gd.pts", "hr.index.mean")
  ], 2,
  sumStat,
  na.rm = TRUE
)
mean(rep$gd.pts-1, na.rm = TRUE)

library(Hmisc)
rcorr(
  as.matrix(rep[, c("std.ciri.assn", "std.cld.freass", 
    "std.ciri.speech", "std.cld.freexp", "std.ciri.elecsd", 
    "std.p.parcomp", "sr.index.mean", "std.ciri.physint", 
    "std.gd.pts", "hr.index.mean")]
  ),
  type = "spearman",
)
detach(package:Hmisc)
detach(package:grid)
detach(package:lattice)
detach(package:survival)
detach(package:splines)
detach(package:Formula)

rm(list = ls())