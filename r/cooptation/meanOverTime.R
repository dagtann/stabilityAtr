## === This script generates annual means for cooptation ===
load(file.path(pathData, 'CJSD Data 2015-03-31.RData'))
str(auto.cp)
## --- select index of cooptation --------------------------
dta.ico <- select(auto.cp, cowcode, year, ico.centre)
dta.ico <- mutate(dta.ico, 
  cabb = countrycode::countrycode(
    sourcevar = cowcode, 
    origin = 'cown', destination = 'iso3c',
    warn = TRUE
  )
)

## --- annual mean graph -----------------------------------
p1 <- ggplot(data = dta.ico, aes(y = ico.centre, x = year))
# p1 + stat_summary(fun.y = mean())()


## --- panel plot: examine random sample of countries ------
set.seed(5458)
cow.codes <- distinct(dta.ico, cowcode)[, 'cowcode']
length(cow.codes)                    ## 136 unique countries
sample.cow <- base::sample(
  cow.codes, size = 27, replace = FALSE
) 
ggplot(
  data = subset(dta.ico, cowcode %in% sample.cow),
  aes(x = year, y = ico.centre)
) +
geom_point() +
geom_smooth(method = 'lm', span = .7) +
facet_wrap(~ cabb)