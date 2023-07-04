rm(list=ls())

library(lmomIDF)
library(RMAWGEN)
library(boot)
library(ggplot2)
source("/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg.qua.conf.R", echo=TRUE)

data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:3,time) 
lmom <- annual.agg.samlmu(y)

z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
f=c(0.25,0.5,0.75,0.9,0.99,0.995,0.999) ##,0.9995,0.99999,0.999995,0.999999)
boot.ci.type=c("norm","basic", "stud", "perc")
boot.ci.type="perc"
out <- annual.agg.qua.conf(f=f,para=z,boot.ci.type=boot.ci.type,R=999999)

## ggplot plot 
gg <- ggplot()+geom_point(aes(x=aggr,y=aggr_obs),data=out$qua)+geom_abline()+theme_bw()
gg <- gg+geom_ribbon(aes(x=aggr,ymax=aggr_high,ymin=aggr_low),data=out$qua,fill="grey",alpha=0.5)

##
