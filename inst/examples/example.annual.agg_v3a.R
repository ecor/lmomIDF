library(RMAWGEN)
###library(lmomIDF)
library(profvis)
library(dplyr)
library(stringr)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg.R", echo=TRUE)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/df2vec.R", echo=TRUE)

data(trentino)
###
prof_file <- "/home/ecor/local/rpackages/jrc/lmomIDF/inst/examples/example.agg.profile.out"
Rprof(filename=prof_file)
###
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

ccs <- system.time({
  outvs <- annual.agg(x,time,return_vector=TRUE,dd=c(1,4),order_time=FALSE,speed_up=TRUE)
})
ccd <- system.time({
  outvd <- annual.agg(x,time,return_vector=TRUE,dd=c(1,4),order_time=FALSE,speed_up=FALSE)
})

(outvs-outvd)