library(lmomIDF)
library(lubridate)
library(RMAWGEN)
library(dplyr)
library(lmomPi)
##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.samlmu.R")
##
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:10,time) 
out <- annual.agg.samlmu(y)
lmrd(out)

####

#### https://stackoverflow.com/questions/33025385/fitted-gev-df-parameters-different-results-for-different-packages-lmomrfa-lm
####
####
####
#### https://rdrr.io/cran/lmom/man/lmrd.html




