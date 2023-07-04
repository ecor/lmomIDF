library(RMAWGEN)
###library(lmomIDF)
library(profvis)
library(dplyr)
library(stringr)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg.R", echo=TRUE)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/df2vec.R", echo=TRUE)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg_only.array.R", echo=TRUE)
data(trentino)
###
prof_file <- "/home/ecor/local/rpackages/jrc/lmomIDF/inst/examples/example.agg.profile.out"
Rprof(filename=prof_file)
###
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

pp <- Rprof({
  outv <- annual.agg(x,time,return_vector=TRUE,dd=c(1,4),order_time=FALSE,speed_up=TRUE)
},interval=0.005)
Rprof(NULL)
summaryRprof(prof_file)

###

out3 <- outxxx

vv <- paste(out3$dd,out3$index,sep="_")
out22 <- tapply(X=out3$value,INDEX=vv,FUN=max,na.rm=TRUE)