## --- Preliminaries --------------------------------------------
rm(list = ls())
if( Sys.info()[['user']] == 'dag' ){
  pathData = '/Users/dag/Dropbox/stabilityAtr/data';
  pathOut = '/Users/dag/Dropbox/stabilityAtr/out';
  pathCode = '/Users/dag/gitreps/stabilityAtr/r';
  options(help_type = 'html')
}

packs <- c('MASS', 'ggplot2', 'ggthemes')
invisible(lapply(packs, library, character.only = TRUE))