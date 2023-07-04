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
## view with leaflet:: see ..

years <- 1982:2021
chirps_filenames <- "/home/ecor/local/data/climate/jrc/lmomIDF_example_ext_data/chirps_test_area/chirps_%d.grd" %>% sprintf(years)
cond0 <- FALSE ## current set to be FALSE please set TRUE not to trigger possible CHIRPS data download

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
    ##out0aggr <- app(prec_data,fun=annual.agg.wrap,time=time(prec_data),filename=aggr_filename,overwrite=TRUE)
    out0aggr <- app(prec_data,fun=annual.agg,time=time(prec_data),return_vector=TRUE,filename=aggr_filename,overwrite=TRUE)
  })
} else {
  
  
  out0aggr <- rast(aggr_filename)
}
###
out0aggr[out0aggr<0] <- NA ## Mandatory
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
aggr_to_plot[aggr_to_plot>400] <- NA
##https://cran.r-project.org/web/packages/tidyterra/vignettes/tidyterra.html

##https://cran.r-project.org/web/packages/tidyterra/vignettes/tidyterra.html
nrow <- length(unique(time(aggr_to_plot)))
mm <- matrix(names(aggr_to_plot),nrow=nrow) %>% t() 
gg <- ggplot()+geom_spatraster(data = aggr_to_plot[[mm]])+facet_wrap(~lyr,nrow=nrow)+theme_bw()
##gg <- gg + scale_fill_continuous(type = "viridis")



ff <- function(x,colors=colz,values=aggr_to_plot[],probs=(0:10)/100,...) {
  x <- x[]
  df <- data.frame(prob=probs)
  df$color <- colorRampPalette(colors)(nrow(df))
  df$value <- quantile(values,probs=df$prob,type=3,na.rm=TRUE) ## sse help(quantile)
   ###summary(quantile(probs=ecdf(values_)(values_),values_,type=3)-values_)
  qq <- ecdf(values)(x)
  
  out <- colorRamp(colors,...)(qq)
  outt <<- out
 out2 <- rgb(red=out[,1],green=out[,2],blue=out[,3],maxColorValue=255)
  
  
  return(out2)
}

##gg <- gg + scale_fill_continuous(type =  ff)

#gg <- gg + scale_fill_continuous(type =  colorRampPalette(colz)(10))
xvv <- 0:300
gg <- gg+scale_fill_gradientn(colors=ff(0:300),name="Precipitation Intensity [mm/day]")

#gg <- gg+scale_fill_gradientn(palette=colorRampPalette(colz))
##gg <- ggplot() +geom_spatraster(data = aggr_to_plot,aes(group=lyr))+facet_grid(lyr ~ .time)+theme_bw()
##gg <- gg+scale_fill_gradient(low="blue", high="red")
## http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
gg


## ----plet,fig.width=7,fig.height=10-------------------------------------------
## plet(x, 1:2, tiles=c("Streets", "Esri.WorldImagery", "OpenTopoMap")[3], collapse=FALSE) |> lines(v, lwd=2, col="blue")
library(leaflet)
out1aggr  <- out0aggr
out1aggr[out1aggr>400] <- NA
plet(out1aggr,1:nlyr(out0aggr), tiles=c("Streets", "Esri.WorldImagery", "OpenTopoMap")[2], collapse=FALSE,shared=TRUE,legend="bottomleft") %>% addScaleBar() #%>% lines(v, lwd=2, col="blue")  #%>% lines(v, lwd=2, col="blue") 


## ----ann.max.samlum.1981.2010,fig.width=7, paged.print=FALSE,eval=TRUE--------

library(lmomIDF)
library(terra)

lmom_filename <-  "/home/ecor/local/rpackages/jrc/lmomIDF/inst/ext_data/chirps_test_area_aggr/chirps_annual_maxima_lmom_v01.grd"
cond_lmom <- FALSE
cond_lmom <- !file.exists(lmom_filename) | cond_lmom
##cond_lmom <- TRUE
if (cond_lmom) {
  lmom_map <- app(out0aggr,fun=annual.agg.idf.samlmu,like_lmom=FALSE,use_ggplot=FALSE,filename=lmom_filename,overwrite=TRUE,nmom=5)
} else {
  
  
  lmom_map <- rast(lmom_filename)

}

## CHECK L-MOMENT VALIDITY with "lmomco" package
library(lmomco)
lmom_valid <- app(lmom_map,fun=function(x){are.lmom.valid(vec2lmom(x[1:5]))})
## check lmom_map[(lmom_valid!=1) & !(is.na(lmom_map[[1]])) & !(is.na(lmom_map[[2]]))]
## correction: 
#lmom_map[(lmom_valid!=1) & !(is.na(lmom_map[[1]])) & !(is.na(lmom_map[[2]]))] <- NA
lmom_map[(lmom_valid!=1)] <- NA
####

gl  <- ggplot()+geom_spatraster(data = lmom_map[[c("l_1","l_2")]])+facet_wrap(~lyr)+theme_bw()
gl

## ----ann.max.samlum.1981.2010_t3_t4,fig.width=7, paged.print=FALSE,eval=TRUE----
gt3  <- ggplot()+geom_spatraster(data = lmom_map[[c("t_3","t_4")]])+facet_wrap(~lyr)+theme_bw()
gt3

## ----ann.max.samlmu.1981.2010_n_idf,fig.width=7, paged.print=FALSE,eval=TRUE----

sp_n_idf <- lmom_map[[c("n_idf")]]
sp_p_idf <- lmom_map[[c("p_idf")]]
sp_n_ddf <- lmom_map[[c("n_ddf")]]
sp_p_ddf <- lmom_map[[c("p_ddf")]]   

sp2_n_idf <- sp_n_idf       
sp2_n_idf[sp_p_ddf>0.1] <- -1              
gn <- ggplot()+geom_spatraster(data = sp2_n_idf)+facet_wrap(~lyr)+theme_bw()
gn



## ----ann.max.samlmu.1981.2010_p_ddf,fig.width=7, paged.print=FALSE,eval=TRUE----

     
gpvd <- ggplot()+geom_spatraster(data = sp_p_ddf)+facet_wrap(~lyr)+theme_bw()
gpvd



## ----ann.max.samlmu.1981.2010_gev_param,fig.width=7, paged.print=FALSE,eval=TRUE----


library(raster)
library(rasterList) ## RasterList >= 0.5.15
library(stringr)

distrib="gev"


ddd <- sapply(str_split(names(out0aggr),"_"),function(w){w[1]}) %>% unique() %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>% as.numeric()
lmom_rl <- stack(lmom_map) %>% rasterList(FUN=function(x,dd=ddd,pv=0.1,dd.name="dd"){ 
    n_idf <- x["n_idf"]
    n_idf[x["p_ddf"]>pv] <- (-1)
    out <- data.frame(dd=dd) 
    names(out)[names(out)=="dd"] <- dd.name
    for (it in c("l_1","l_2")) {
      
      out[,it] <- x[it]*out[,dd.name]^n_idf
      
    } 
    for (it in c("t_3","t_4")) {
      
      
      out[,it] <- x[it]
    }  
    
    return(out)
    
  })


gev_para <- RasterListApply(x=rasterList(stack(out0aggr)),lmom=lmom_rl,FUN=annual.agg.pel,distrib="gev") 

gev_pvalue <- stack(gev_para,FUN=function(x,dd_formatter="D%03d",dd.name="dd") {
  ou2 <- attr(x,"lmom")
  if (!is.data.frame(ou2)) {
    ou3 <- NULL
  } else {
    ou3 <- ou2$p.value
    names(ou3) <- sprintf(dd_formatter,ou2[,dd.name])
  }
  return(ou3)
  }) %>% rast()

gpv_gev <- ggplot()+geom_spatraster(data = gev_pvalue)+facet_wrap(~lyr)+theme_bw()
gpv_gev




## ----ann.max.samlum.1981.2010_gev_param_test_passed,fig.width=7, paged.print=FALSE,eval=TRUE----


gev_pvalue_passed <- gev_pvalue>0.1

gpv_gev_passed <- ggplot()+geom_spatraster(data = gev_pvalue_passed)+facet_wrap(~lyr)+theme_bw()
gpv_gev_passed



## ----ann.max.samlum.1981.2010_gev_param_test_passed2,fig.width=7, paged.print=FALSE,eval=TRUE----


gev_pvalue_passed2 <- min(gev_pvalue)>0.1
gev_gev_passed2 <- ggplot()+geom_spatraster(data = gev_pvalue_passed2)+theme_bw()
gev_gev_passed2



## ----ann.max.samlum.1981.2010_qua,fig.width=7, ,fig.height=10,paged.print=FALSE,eval=TRUE----




library(raster)
rt <- c(2,5,10,20,50,100,200)


f <- 1-1/rt ##c(0.5,0.9)
gev_qua_rl <- rasterList(gev_para,FUN=annual.agg.qua,f=f,use_ggplot=FALSE,possible_return_null=TRUE) ###    (f=f,para=z,use_gglopt=FALSE)
gev_qua <- stack(gev_qua_rl,FUN=df2vec,index.name="f") %>% rast()

####
gev_qua[!gev_pvalue_passed2] <- NA
####

nrow <- length(f)
mm <- matrix(names(gev_qua),nrow=nrow) %>% t() 
ggqua <- ggplot()+geom_spatraster(data = gev_qua[[mm]])+facet_wrap(~lyr,nrow=nrow)+theme_bw()
##xvv <- 0:300
##gg <- gg+scale_fill_gradientn(colors=ff(0:300),name="Precipitation Intensity [mm/day]")
ggqua




## ----ann.max.samlum.1981.2010_qua_plet,fig.width=7, ,fig.height=10,paged.print=FALSE,eval=TRUE----

library(leaflet)



gev1_qua  <- gev_qua
gev1_qua[gev1_qua>400] <- NA

ff2 <- function(x,colors=colz,values=gev1_qua[],probs=(0:10)/100,...) {
  x <- x[]
  df <- data.frame(prob=probs)
  df$color <- colorRampPalette(colors)(nrow(df))
  df$value <- quantile(values,probs=df$prob,type=3,na.rm=TRUE) ## sse help(quantile)
   ###summary(quantile(probs=ecdf(values_)(values_),values_,type=3)-values_)
  qq <- ecdf(values)(x)
  
  out <- colorRamp(colors,...)(qq)
  outt <<- out
 out2 <- rgb(red=out[,1],green=out[,2],blue=out[,3],maxColorValue=255)
  
  
  return(out2)
}







plet(gev1_qua,1:nlyr(gev1_qua), tiles=c("Streets", "Esri.WorldImagery", "OpenTopoMap")[2], collapse=FALSE,shared=TRUE,legend="bottomleft") ##,col=ff2) %>% addScaleBar() #%>% lines(v, lwd=2, col="blue") 



## ----generateBibliography,echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE,print=FALSE,results="hide"----

require(knitcitations)
cleanbib()
options(citation_format="pandoc")
read.bibtex(file = "bibliography.bib")



