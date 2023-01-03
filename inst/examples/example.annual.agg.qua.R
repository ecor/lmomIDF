library(lmomIDF)
library(lubridate)
library(RMAWGEN)
library(dplyr)
library(lmomPi)
library(ggplot2)
##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.samlmu.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.pel.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.qua.R")
##
set.seed(123)
##
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:20,time) 
lmom <- annual.agg.samlmu(y)
## TO CONTUE
y <- annual.agg(x,dd=1:5,time) 
lmom <- annual.agg.samlmu(y)
lmrd(lmom)
z <- annual.agg.pel(distrib="gpa",x=y,lmom=lmom)

out <- annual.agg.qua(para=z)
##
set.seed(100)
f <- unique(year(time)) %>% length() %>% runif()
out2 <- annual.agg.qua(f=f,para=z)

##