
library(RMAWGEN)
data(trentino)

source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.cdf.R", echo=TRUE)

time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:20,time) 
lmom <- annual.agg.samlmu(y)
## TO CONTUE
y <- annual.agg(x,dd=1:5,time) 
lmom <- annual.agg.samlmu(y)
lmrd(lmom)

xval <- y %>% filter(dd==1) %>% select(aggr) %>% extract2(1)
z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
########ff <- annual.agg.qua(f=c(0.5,0.9),para=z)



out <- annual.agg.cdf(x="sample",para=z)




###out[out$f %in% ff$f,names(ff)]
##ff