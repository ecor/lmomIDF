rm(list=ls())
###
###

library(lmomIDF)
library(RMAWGEN)
library(RColorBrewer)
###
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.qua.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.pel.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.idf.samlmu.R")
###
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:5,time)
out <- annual.agg.idf.samlmu(y)

out_pel <- annual.agg.pel(distrib="gpa",x=y,lmom=out)
set.seed(560)
f=c(0.5,0.8,0.9,0.95,0.98,0.99,0.999,0.9999)

out_qua <- annual.agg.qua(para=out_pel,f=f,remove_distrib_from_boxplot=TRUE)

####
library(GSODR)
library(GSODR)
library(magrittr)
library(data.table)
library(dplyr)

set.seed(123)

years <- 1937:2020
gsod <- get_GSOD(years=years,station="623180-99999") ##ALEXANDRIA INTL EG 623180-99999
prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>%
  mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d")) 

dds <- range(prec$YEARMODA)
## See GSODR documentation
yymmdds <- seq(from=dds[1],to=dds[2],by="day")
prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)
y <- annual.agg(x=prec$PRCP,dd=1:5,time=prec$YEARMODA)
### y$aggr cannot be -Inf or +Inf
y$aggr[y$aggr==-Inf] <- NA
lmom0 <- annual.agg.samlmu(y)
lmrd(lmom0)
lmom <- annual.agg.idf.samlmu(y)


z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
out_qua_gsod <- annual.agg.qua(f=f,para=z,remove_distrib_from_boxplot=TRUE)
attr(out_qua_gsod,"ddf")


