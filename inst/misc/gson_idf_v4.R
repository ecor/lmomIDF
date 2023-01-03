rm(list=ls())
library(IDF)
library(GSODR)
library(sf)
library(dplyr)
library(magrittr)
library(lmomRFA)
###
set.seed(122)
inventory_eg <- get_inventory() 
inventory_eg <- inventory_eg %>% filter(!is.na(LON) & !is.na(LAT)) %>% st_as_sf(coords=c("LON","LAT"),crs=4326)
###
#mapview::mapview(inventory_eg)
## 624140-99999 

#STNID 	624140-99999 
#NAME 	ASWAN INTL 
#CTRY 	EG 
#STATE 	 
#BEGIN 	19370103 
#END 	20201122 
#COUNTRY_NAME 	EGYPT

years <- 1937:2020


gsod <- get_GSOD(years=years,station="623180-99999")
             ##      "640050-99999")  ### MBANDAKA / CONGO 
                   ##"642110-99999") ## KINSHASA / CONGO
                 ## ASWAN: 624140-99999") 
##ALEXANDRIA INTL EG 623180-99999
##############
prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>% mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d")) 



# PRCP_ATTRIBUTES -
#   A = 1 report of 6-hour precipitation amount;
#   B = Summation of 2 reports of 6-hour precipitation amount;
#   C = Summation of 3 reports of 6-hour precipitation amount;
#   D = Summation of 4 reports of 6-hour precipitation amount;
#   E = 1 report of 12-hour precipitation amount;
#   F = Summation of 2 reports of 12-hour precipitation amount;
#   G = 1 report of 24-hour precipitation amount;
#   H = Station reported ‘0’ as the amount for the day (e.g. from 6-hour reports), but also reported at least one occurrence of precipitation in hourly observations–this could indicate a trace occurred, but should be considered as incomplete data for the day;
#   I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations–it’s still possible that precipitation occurred but was not reported;
dd <- range(prec$YEARMODA)
yymmdds <- seq(from=dd[1],to=dd[2],by="day")
prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)
####
##ddays <- c(1,2,5) ##3,4,5,10,20)
##names(ddays) <- "PRCP_D%03d" %>% sprintf(ddays) 

y <- annual.agg(x=prec$PRCP,dd=1:5,time=prec$YEARMODA)
### y$aggr cannot be -Inf or +Inf
y$aggr[y$aggr==-Inf] <- NA
lmom <- annual.agg.samlmu(y)
lmrd(lmom)
z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
out <- annual.agg.qua(f=c(1:49)/50,para=z)
