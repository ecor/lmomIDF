## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=TRUE,
  warning = FALSE
)
###knitr::opts_chunk$set(echo = TRUE)

## ----chirps,fig.width=7-------------------------------------------------------

## remotes::install_github("ropensci/chirps")
## 
library(chirps)
library(sf)
library(magrittr)
library(terra)
library(stringr)
##
##

xmin <- 29
ymin <- 25
xmax <- 37
ymax <- 32

r <- rast(xmin=xmin, xmax=xmax, 
          ymin=ymin, ymax=ymax,crs="epsg:4326",resolution=1)

r[] <- 0
mapview::mapview(raster::raster(r),alpha=0.2)
###plet(r,alpha=0.2,tiles="Streets")

years <- 1982:2021
chirps_filenames <- "/home/ecor/local/data/climate/jrc/lmomIDF_example_ext_data/chirps_test_area/chirps_%d.grd" %>% sprintf(years)
cond0 <- FALSE ## current set to be FALSE please set TRUE to trigger possible CHIRPS data download

for (year in years) {

  dates <- c("%d-01-01","%d-12-31") %>% sprintf(year,year)
  filename <- chirps_filenames[years==year]
  cond <- !file.exists(filename) | cond0
  if (cond) {
    ss <- system.time({
  ## chirps R package Version 0.1.5
  
      chirps <- get_chirps(r,dates=dates,server="CHC")
  
      chirps[chirps<0] <- NA
  
      terra::time(chirps) <- seq(from=as.Date(dates[1]),to=as.Date(dates[2]),by=1)
  
    
  
      writeRaster(chirps,filename=filename,overwrite=TRUE) 
    })
  }
}

## PUT A MAP HERE

## ----chirps.max,fig.width=7,fig.height=8--------------------------------------

prec_data <- rast(chirps_filenames)

annual.agg.wrap <- function(x,time,...,numeric_dd= FALSE,aggr.name="aggr",dd.name="dd", index.name = "index") { 
  out1 <- annual.agg(x,time,aggr.name=aggr.name,dd.name=dd.name,index.name=index.name,numeric_dd=numeric_dd,...)
  out1 <- as.data.frame(out1)
  out <- out1[,aggr.name]
  names(out) <- paste(out1[,dd.name],out1[,index.name],sep="_")
  return(out)
  }
cond1 <- FALSE ## current set to be FALSE please set TRUE to trigger possible CHIRPS data download
aggr_filename <- "/home/ecor/local/rpackages/jrc/lmomIDF/inst/ext_data/chirps_test_area_aggr/chirps_annual_maxima_v01.grd"

cond <- !file.exists(aggr_filename) | cond1
if (cond) {
##
  ss <- system.time({
    out0aggr <- app(prec_data,fun=annual.agg.wrap,time=time(prec_data),filename=aggr_filename,overwrite=TRUE)

  })
} else {
  
  
  out0aggr <- rast(aggr_filename)
}
###
out0aggr[out0aggr<0] <- NA
time(out0aggr,tstep="years") <- str_split(names(out0aggr),"_") %>% sapply(FUN=function(x){x[2]}) %>% as.numeric()
units(out0aggr) <- "mm day -1"
varnames(out0aggr) <- "Rainfall Intensity"
##varnames(out0aggr) <-  str_split(names(out0aggr),"_") %>% sapply(FUN=function(x){x[1]})
###
plotting_years <- c(1985,1995,2005,2015)
nn <- names(out0aggr)
ll <- unlist(lapply(nn,FUN=function(x,y=plotting_years){return(x[any(str_detect(x,as.character(y)))])}))
###

library(RColorBrewer)
library(tidyterra)
library(ggplot2)
col1 <- brewer.pal(name="YlGnBu",n=10) %>% col2rgb()
col2 <- brewer.pal(name="YlOrRd",n=10) %>% rev() %>% col2rgb()
coll <- (col1*0.3+col2*0.7)/255
##coll[,] <- as.integer(coll[,]) 
colz <- rgb(red=coll["red",],green=coll["green",],blue=coll["blue",],maxColorValue = 1) %>% rev()
#plot(out0aggr[[ll]],col=colz)
## https://bookdown.org/igisc/EnvDataSci/spatial-data-and-maps.html

aggr_to_plot <- out0aggr[[ll]]
##https://cran.r-project.org/web/packages/tidyterra/vignettes/tidyterra.html

##https://cran.r-project.org/web/packages/tidyterra/vignettes/tidyterra.html
nrow <- length(unique(time(aggr_to_plot)))
mm <- matrix(names(aggr_to_plot),nrow=nrow) %>% t() 
gg <- ggplot() +geom_spatraster(data = aggr_to_plot[[mm]])+facet_wrap(~lyr,nrow=nrow)+theme_bw()
##gg <- ggplot() +geom_spatraster(data = aggr_to_plot,aes(group=lyr))+facet_grid(lyr ~ .time)+theme_bw()
gg


## ----plet,fig.width=7,fig.height=10-------------------------------------------
## plet(x, 1:2, tiles=c("Streets", "Esri.WorldImagery", "OpenTopoMap")[3], collapse=FALSE) |> lines(v, lwd=2, col="blue")
library(leaflet)

plet(out0aggr,1:nlyr(out0aggr), tiles=c("Streets", "Esri.WorldImagery", "OpenTopoMap")[2], collapse=FALSE,shared=TRUE,legend="bottomleft") %>% addScaleBar() #%>% lines(v, lwd=2, col="blue") 


## ----ann.max.samlum.1981.2010,fig.width=7, paged.print=FALSE,eval=FALSE-------
#  
#  library(lmomIDF)
#  source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.idf.samlmu.R")
#  library(RMAWGEN)
#  data(trentino)
#  time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#  PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#  x <- PRECIPITATION$B8570
#  vv <- out0aggr[123454667][1,]
#  
#  annual.agg.idf.samlmu.vector <- function(xv=vv,nv=names(xv),...) {
#  
#    oo2 <- data.frame(aggr=as.numeric(xv))
#    oo2$dd <- str_split(nv,"_") %>% sapply(FUN=function(x){x[1]})  %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>%     as.numeric()
#    oo2$index <- str_split(nv,"_") %>% sapply(FUN=function(x){x[2]})  %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>%     as.numeric()
#    ###
#    return(oo2)
#  }
#  y <- annual.agg(x,dd=1:5,time)
#  
#  out <- annual.agg.idf.samlmu(y,use_ggplot=FALSE,like_lmom=FALSE)
#  
#  ###prec_lmom <- app(out0aggr,fun=annual.agg.idf.samlmu,time=time(prec_data),filename=aggr_filename,overwrite=TRUE)
#  

## ----generateBibliography,echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE,print=FALSE,results="hide"----

require(knitcitations)
cleanbib()
options(citation_format="pandoc")
read.bibtex(file = "bibliography.bib")



