## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=TRUE,
  warning = FALSE
)
###knitr::opts_chunk$set(echo = TRUE)

## ----gsod---------------------------------------------------------------------

library(GSODR)
library(dplyr)
library(magrittr)
years <- 1937:2020
gsod_rds <- "/home/ecor/local/rpackages/jrc/lmomIDF/inst/ext_data/gsod_623180-99999.rds"
cond <- file.exists(gsod_rds) 
if (!cond) {
  gsod <- get_GSOD(years=years,station="623180-99999") ##ALEXANDRIA INTL EG 623180-99999
  saveRDS(gsod,file=gsod_rds)  
} else {
  gsod <- readRDS(file=gsod_rds)
}

#####
#####

prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>%
 mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d")) 

knitr::kable(rbind(head(prec),tail(prec)))

### Dataset preparation 
## insertion of rows so that all instants are equidistant (i.e. there is a constant time step) 
## inserting any NA values of precipitation intensity


dds <- range(prec$YEARMODA)
## See GSODR documentation
yymmdds <- seq(from=dds[1],to=dds[2],by="day")
prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)




## ----gsod_zoo, fig.width=7, paged.print=FALSE---------------------------------

library(zoo)
library(dygraphs)

time <- prec$YEARMODA
precz <- prec$PRCP %>% as.zoo()
index(precz) <- time
main="DAILY PRECIPITATION Vs TIME"
ylab="precipitation [mm/hr]"
xlab="time"

dd <- dygraph(precz,main=main,ylab=ylab,xlab=xlab) %>% dyRangeSelector()
dd



## ----ann.max,fig.width=7, paged.print=FALSE-----------------------------------

library(lmomIDF)
library(lubridate)


ann.maxima <- annual.agg(x=prec$PRCP,time=prec$YEARMODA,aggr.fun=max,dd=1:5)
### values cannot be -Inf or +Inf
ann.maxima$aggr[ann.maxima$aggr==-Inf] <- NA
###
str(ann.maxima)
###
ann.maxima.z <- ann.maxima %>% reshape2::dcast(index ~ dd)
time <- as.numeric(ann.maxima.z$index)
ann.maxima.z <- ann.maxima.z %>% select(-index) %>% as.zoo()
time1 <- as.Date("1980-01-01")
year(time1) <- time
index(ann.maxima.z) <- time1 


main="MAXIMUM PRECIPITATION INTENSITY Vs TIME"
ylab="precipitation intensity [mm/hr]"
xlab="time"

dda <- dygraph(ann.maxima.z,main=main,ylab=ylab,xlab=xlab) %>% dyRangeSelector()
dda

## ----ann.max.samlum.1981.2010,fig.width=7, paged.print=FALSE------------------

x <- ann.maxima %>% filter(as.numeric(index) %in% 1980:2010) 
lmom <- annual.agg.samlmu(x)

head(lmom)


## ----ann.max.pel.1981.2010,fig.width=7, paged.print=FALSE---------------------

para <- annual.agg.pel(distrib="gev",x=x,lmom=lmom)

para$D001
attr(para,"lmom")



## ----ann.max.qua.1981.2010,fig.width=7, paged.print=FALSE---------------------


lmom_idf_1981_2010 <- annual.agg.idf.samlmu(x,lmom=attr(para,"lmom"))
####
summary(attr(lmom_idf_1981_2010,"fit"))
####
attr(lmom_idf_1981_2010,"gg")


## ----ann.max.idf.pel.1981.2010,fig.width=7, paged.print=FALSE-----------------
para_idf_1981_2010 <- annual.agg.pel(distrib="gev",x=x,lmom=lmom_idf_1981_2010)

para_idf_1981_2010$D001
attr(para_idf_1981_2010,"lmom")
 

## ----ann.max.idf.qua.1981.2010,fig.width=7, paged.print=FALSE-----------------

set.seed(560)
rt <- c(2,5,10,20,50,100,200,500)
f  <- 1-1/rt
##f=c(0.5,0.8,0.9,0.95,0.98,0.99,0.999,0.9999)
out_qua_1981_2010 <- annual.agg.qua(f=f,para=para_idf_1981_2010,remove_distrib_from_boxplot=TRUE)
attr(out_qua_1981_2010,"idf")
 

## ----ann.max.idf.qua.depth.1981.2010,fig.width=7, paged.print=FALSE-----------

attr(out_qua_1981_2010,"ddf")
 

## ----ann.max.samlum.1991.2020,fig.width=7, paged.print=FALSE------------------

x <- ann.maxima %>% filter(as.numeric(index) %in% 1980:2020) 
lmom <- annual.agg.samlmu(x)

head(lmom)


## ----ann.max.pel.1991.2020,fig.width=7, paged.print=FALSE---------------------

para <- annual.agg.pel(distrib="gev",x=x,lmom=lmom)

para$D001
attr(para,"lmom")



## ----ann.max.qua.1991.2020,fig.width=7, paged.print=FALSE---------------------


lmom_idf_1991_2020 <- annual.agg.idf.samlmu(x,lmom=attr(para,"lmom"))
####
summary(attr(lmom_idf_1991_2020,"fit"))
####
attr(lmom_idf_1991_2020,"gg")


## ----ann.max.idf.pel.1991.2020,fig.width=7, paged.print=FALSE-----------------
para_idf_1991_2020 <- annual.agg.pel(distrib="gev",x=x,lmom=lmom_idf_1991_2020)

para_idf_1991_2020$D001
attr(para_idf_1991_2020,"lmom")
 

## ----ann.max.idf.qua.1991.2020,fig.width=7, paged.print=FALSE-----------------

set.seed(560)
rt <- c(2,5,10,20,50,100,200,500)
f  <- 1-1/rt
##f=c(0.5,0.8,0.9,0.95,0.98,0.99,0.999,0.9999)
out_qua_1991_2020 <- annual.agg.qua(f=f,para=para_idf_1991_2020,remove_distrib_from_boxplot=TRUE)
attr(out_qua_1991_2020,"idf")
 

## ----ann.max.idf.qua.depth.1991.2020,fig.width=7, paged.print=FALSE-----------

attr(out_qua_1991_2020,"ddf")
 

## ----qqplot.idf,fig.width=7, paged.print=FALSE--------------------------------

qua_1991_2020 <- out_qua_1991_2020
qua_1981_2010 <- out_qua_1981_2010

attr(out_qua_1981_2010,"n_idf")
attr(out_qua_1991_2020,"n_idf")

qua_1981_2010$a <- qua_1981_2010$aggr/(qua_1981_2010$dd^attr(out_qua_1981_2010,"n_idf"))
qua_1991_2020$a <- qua_1991_2020$aggr/(qua_1991_2020$dd^attr(out_qua_1991_2020,"n_idf"))
#####
qua_1981_2010 <- qua_1981_2010 %>% select(-aggr)
qua_1991_2020 <- qua_1991_2020 %>% select(-aggr)
#####

names(qua_1981_2010)[names(qua_1981_2010)=="a"] <- "a_1981_2010"
names(qua_1991_2020)[names(qua_1991_2020)=="a"] <- "a_1991_2020"
#####


# > ggplot()+geom_point(data=qua_both,aes(x=aggr_1981_2010,y=aggr_1991_2010))
# Error in FUN(X[[i]], ...) : object 'aggr_1991_2010' not found
# > ggplot()+geom_point(data=qua_both,aes(x=aggr_1981_2010,y=aggr_1991_2020))
# > ggplot()+geom_point(data=qua_both,aes(x=aggr_1981_2010,y=aggr_1991_2020,group=dd))
# > ggplot()+geom_point(data=qua_both,aes(x=aggr_1981_2010,y=aggr_1991_2020,group=dd,fill=dd))
# > ggplot()+geom_point(data=qua_both,aes(x=aggr_1981_2010,y=aggr_1991_2020,group=dd,color=factor(dd)))
# > 
#####
qua_both <- full_join(qua_1981_2010,qua_1991_2020)

#####
library(ggplot2)
library(RColorBrewer)

gg <- ggplot()+geom_point(data=qua_both,aes(x=a_1981_2010,y=a_1991_2020,color=factor(f)))
gg <- gg+xlab("Quantiles 1981-2010")+ylab("Quantiles 1991-2020")+theme_bw()+geom_abline()
###
col1 <- brewer.pal(name="YlGnBu",n=length(unique(qua_both$f))) %>% col2rgb()
col2 <- brewer.pal(name="YlOrRd",n=length(unique(qua_both$f))) %>% rev() %>% col2rgb()
coll <- (col1*0.3+col2*0.7)/255
##coll[,] <- as.integer(coll[,]) 
colz <- rgb(red=coll["red",],green=coll["green",],blue=coll["blue",],maxColorValue = 1)

##YlOrRd
gg <- gg+scale_color_manual(name="Probability",values=colz)
gg

## ----generateBibliography,echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE,print=FALSE,results="hide"----

require(knitcitations)
cleanbib()
options(citation_format="pandoc")
read.bibtex(file = "bibliography.bib")



