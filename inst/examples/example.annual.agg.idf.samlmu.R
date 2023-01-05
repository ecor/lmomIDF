rm(list=ls())

##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.samlmu.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.pel.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.qua.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.cdf.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.idf.samlmu.R")
##



library(lmomRFA)
library(RMAWGEN)
data(trentino)


time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:5,time)
out <- annual.agg.idf.samlmu(y) 

###




###