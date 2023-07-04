library(RGENERATEPREC)
library(lubridate)
library(lmomPi)
library(ggplot2)
library(reshape2)
set.seed(1245)

data(trentino)

year_min <- 1961
year_max <- 1990

origin <- paste(year_min,1,1,sep="-")
end <- paste(year_max,12,31,sep="-")

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

prec_mes <- PRECIPITATION[period,]
Tx_mes <- TEMPERATURE_MAX[period_temp,]
Tn_mes <- TEMPERATURE_MIN[period_temp,]
accepted <- array(TRUE,length(names(prec_mes)))
names(accepted) <- names(prec_mes)
for (it in names(prec_mes)) {
  acc <- TRUE
  acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
  acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
  accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
  
}

valmin <- 1.0
prec_mes <- prec_mes[,accepted]


Tx_mes <- Tx_mes[,accepted]
Tn_mes <- Tn_mes[,accepted]
station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
prec_occurrence_mes <- prec_mes[,station]>=valmin
prec_gen1_occ <- prec_occurrence_mes ## TO BE MODIFIED !!! 

station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]

precamount <- PrecipitationAmountModel(prec_mes,station=station,origin=origin)

val <- predict(precamount)

prec_gen1 <- generate(precamount)  
it1 <- station[8] ## STATION 

## MODIFIFY_PROBABILITY_DISTRUUTION 


## See RGENERATEPREC vignette section "Modifying Precipitation Distribution (e.g. Precipitation Futture Projection)"
## https://cran.r-project.org/web/packages/RGENERATEPREC/vignettes/precipitation_stochastic_generation_v8.html
origin <- paste(prec_mes$year[1],prec_mes$month[1],prec_mes$day[1],sep="-")
df <- data.frame(obs=prec_mes[,it1],gen=prec_gen1[,it1])
## INSERT ORIGIN HERE
df$date <- as.Date(origin)+lubridate::days(1:nrow(df))-1
df$month <- factor(month(df$date))
df$season <- "none"
df$season[df$month %in% c(12,2,1)] <- "1.DJF"
df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
df$season[df$month %in% c(9,10,11)] <-  "4.SON"

dfp <- df[df$obs>valmin,] 

color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]

gdens <- ggplot(data=dfp)+geom_density(aes(x=obs,color=month,group=month),trim=TRUE)+facet_grid(season ~ .)+theme_bw()
gdens <- gdens+scale_color_manual(values = color_month)

show(gdens)

library(lmom)

prec_val_m <- dfp$obs %>% split(dfp$month) 
lmoms <- prec_val_m %>% lapply(FUN=samlmu) 

params <- lmoms %>% lapply(FUN=pelln3,bound=valmin) 
kstest <- mapply(para=params,FUN=ks.test,x=prec_val_m,y="cdf",distrib="ln3",SIMPLIFY=FALSE) 

kstest

modify_lmoments <- function(x){x[2] <- x[2]*1.3; return(x)}
paraml <- function(x,valmin) {
  ip <- which(x>=valmin)
  x <- x[ip] 
  out <- list()
  lmomx <- samlmu(x)
  out$obs <- pelln3(lmom=lmomx,bound=valmin)
  lmomx_m <- modify_lmoments(lmomx)
  out$mod  <- pelln3(lmom=lmomx_m,bound=valmin)
  return(out)
}

modify_distribution <- function(x,paraml,valmin){
  
  out <- x
  ip <- which(x>=valmin)
  out[ip] <- x[ip] %>% cdfln3(para=paraml$obs) %>% qualn3(para=paraml$mod)
  return(out)
  
}


para_monthly <- df$obs %>% split(df$month) %>% lapply(FUN=paraml,valmin=valmin)

df$obs_mod <- df$obs
for (mo in unique(df$month))  {
  im <- which(df$month==mo)
  
  df$obs_mod[im] <- modify_distribution(x=df$obs[im],paraml=para_monthly[[mo]],valmin=valmin)
}
color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
gg <- ggplot()+geom_line(data=df,mapping=aes(x=obs,y=obs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
gg <- gg+scale_color_manual(values = color_month)
show(gg)


###
###

prec_mod <- as.data.frame(df$obs_mod) 
names(prec_mod) <- it1
model1_amount_mod <- PrecipitationAmountModel(prec_mod,station=it1,origin=origin)
prec_gen1_mod <- generate(model1_amount_mod,newdata=prec_gen1_occ[,it1])

str(prec_gen1_mod)
#> 'data.frame':    10957 obs. of  1 variable:
#>  $ T0083: num  0 3.96 2.19 1.05 0 ...
df$gen_mod <- prec_gen1_mod[,it1]

qqplot__ <- function(df,i=1) {
  df <- as.list(df)
  o <- list()
  nn <- names(df)
  jv <- (1:length(df))[-i]
  for (j in jv) {
    ot <- qqplot(df[[i]],df[[j]],plot.it=FALSE)
    ot <- as.data.frame(ot)
    names(ot) <- names(df)[c(i,j)]
    o[[j]] <- ot
  }
  ref <- o[[jv[1]]][,1]
  for (j in jv[-1]) {
    ref2 <-  o[[j]][,1]
    cond <- all(ref==ref2)
    if (cond==FALSE) stop("qqplot__ error")
    
    
  }
  o <- do.call(args=o[jv],what=cbind)
  o <- o[,nn] %>% melt(id=1)
  
  
  return(o)
}

nnn <- c("obs_mod","gen_mod","obs")

qqdf <- split(df[,nnn],f=df$season) %>% lapply(FUN=qqplot__) %>% melt(id=1:3)
names(qqdf)[names(qqdf)=="L1"] <- "season"
g <- ggplot(data=qqdf)+geom_point(aes(x=obs_mod,y=value,group=variable,color=variable))+theme_bw()+geom_abline()+facet_grid(. ~ season)+xlab("modified secenario through observated values [mm]")+ylab("generated / observated [mm]")
show(g)




#######


stop("HERE")


month <- adddate(as.data.frame(residuals(precamount$T0090)),origin=origin)$month
#####plot(month,residuals(precamount$T0090))
plot(factor(month),residuals(precamount$T0090))

qqplot(prec_mes$T0083,prec_gen$T0083)
abline(0,1)


## SINGLE STATION



precamount_single <- PrecipitationAmountModel(prec_mes,station=station,origin=origin)

val_single <- predict(precamount_single)

prec_gen_single <- generate(precamount_single)  



month <- adddate(as.data.frame(residuals(precamount_single[[station[1]]])),origin=origin)$month
plot(factor(month),residuals(precamount_single[[station[1]]]))


### Comparison (Q-Q plot) between multi and single sites. 

qqplot(prec_mes$T0083,prec_gen$T0083,col=1)
abline(0,1)
points(sort(prec_mes$T0083),sort(prec_gen_single$T0083),pch=2,col=2)
legend("bottomright",pch=c(1,2),col=c(1,2),legend=c("Multi Sites","Single Site"))







abline(0,1)
