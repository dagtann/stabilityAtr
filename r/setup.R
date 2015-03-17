## --- Preliminaries --------------------------------------------
rm(ls = ls())
if( Sys.info()[['user']] == 'dag' ){
  pathData = '/home/dag/Dropbox/data/StabilityAtr/data';
  pathOut = '/home/dag/Dropbox/data/StabilityAtr/out';
  pathCode = '/home/dag/gitreps/stabilityAtr/r';
  options(help_type = 'html')
}

packs <- c('MASS', 'ggplot2', 'ggthemes')
invisible(lapply(packs, library, character.only = TRUE))