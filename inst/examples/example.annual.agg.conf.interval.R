rm(list=ls())

library(lmomIDF)
library(RMAWGEN)
library(boot)
library(ggplot2)
source('/home/ecor/local/rpackages/jrc/lmomIDF/R/annual.agg.cdf.R')
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg..conf.interval.R", echo=TRUE)
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570
#x[1000] <- 500
#x[2000] <- 600
#x[3000] <- 100000
y <- annual.agg(x,dd=1,time)


lmom <- annual.agg.samlmu(y)


z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
####
out <- annual.agg.conf.interval(para=z) ##,boot.ci.type="perc")
## TO DO fdf <- annual.agg.cdf(y=aggr,para=z)
# stop("HERE")
# 
# f=c(0.25,0.5,0.6,0.75,0.9,0.99,0.995,0.999) ##,0.9995,0.99999,0.999995,0.999999)
# plotting_position_f <- function(x){x/(length(x)+1)}
# plotting_position <- y %>% filter(dd==1) %>% select(aggr) %>% extract2(1) %>% sort() %>% rank() %>% plotting_position_f()
# f <- plotting_position
# #f <- 0.999
# boot.ci.type=c("norm","basic", "stud", "perc")
# boot.ci.type="perc"
# out <- annual.agg.qua.conf(f=f,para=z,boot.ci.type=boot.ci.type,R=9999)
out$rt <- 1/(1-out$f)
out$rt_min <- 1/(1-out$f_high)
out$rt_max <- 1/(1-out$f_low)
out$rt_jj <- 1/(1-out$jj)
## ggplot plot 
gg <- ggplot()+geom_line(aes(x=rt,y=aggr_obs),data=out)+theme_bw()
gg <- gg+geom_ribbon(aes(x=rt,ymax=aggr_high,ymin=aggr_low),data=out,fill="grey",alpha=0.5)
gg <- gg+geom_point(aes(x=rt_jj,y=aggr_obs),data=out)
gg <- gg+scale_x_log10()
##
gf <- ggplot()+geom_point(aes(x=aggr,y=jj),data=out)+theme_bw()
gf<- gf+geom_line(aes(x=aggr,y=f),data=out)
gf <- gf+geom_ribbon(aes(x=aggr,ymax=f_high,ymin=f_low),data=out,fill="grey",alpha=0.5)
### RETURN PERIOD 


grt <- ggplot()+geom_point(aes(x=aggr,y=rt_jj),data=out)+theme_bw()+xlab("Precipitation Intensity [mm/day]")+ylab("Return Periods [years]")
grt <- grt+geom_line(aes(x=aggr,y=rt),data=out)
grt <- grt+geom_ribbon(aes(x=aggr,ymax=rt_max,ymin=rt_min),data=out,fill="grey",alpha=0.5)
grt <- grt+scale_y_log10()