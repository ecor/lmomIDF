library(RMAWGEN)
library(lmomIDF)
##source("/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg_only.array.R", echo=TRUE)
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

ccv <- system.time({
  outv <- annual.agg(x,time,return_vector=TRUE,dd=c(1,4))
})
