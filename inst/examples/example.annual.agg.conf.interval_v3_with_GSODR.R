rm(list=ls())

library(lmomIDF)
library(RMAWGEN)
library(boot)
library(ggplot2)
library(GSODR)
source('/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg.cdf.R')
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg..conf.interval.R", echo=TRUE)
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570
#x <- PRECIPITATION$T0099
#x[1000] <- 500
#x[2000] <- 600
#x[3000] <- 100000
y <- annual.agg(x,dd=1,time) ## duration: 1 day 
##
y$aggr[y$aggr<0] <- NA
y <- y[!is.na(y$aggr),]

##

lmom <- annual.agg.samlmu(y)


z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
####
out <- annual.agg.conf.interval(para=z) ##,extreme.f = NULL) ##,boot.ci.type="perc")
## Add Return Period column 
out$rt <- 1/(1-out$f)
out$rt_min <- 1/(1-out$f_high)
out$rt_max <- 1/(1-out$f_low)
out$rt_jj <- 1/(1-out$jj)
## ggplot plot 
gg <- ggplot()+geom_point(aes(x=rt_jj,y=aggr_obs),data=out)
gg <- gg+geom_line(aes(x=rt,y=aggr_obs),data=out)+theme_bw()
gg <- gg+geom_ribbon(aes(x=rt,ymax=aggr_high,ymin=aggr_low),data=out,fill="grey",alpha=0.5)
gg <- gg+scale_x_log10()
gg <- gg+xlab("Return Period [yr]")+ylab("Precipitation [mm]")
##
