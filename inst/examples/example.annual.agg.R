library(lmomIDF)
library(lubridate)
library(RMAWGEN)
library(dplyr)
##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
##
data(trentino)
##which(is.na(PRECIPITATION$B8570))
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570
out <- annual.agg(x,time)
out2 <- yearly.agg(x,time)
