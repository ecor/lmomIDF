library(lmomIDF)
library(lubridate)
library(RMAWGEN)
library(dplyr)
library(lmomPi)
##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.samlmu.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.pel.R")
##
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:365,time) 
y <- annual.agg(x,dd=1:6,time) 
lmom <- annual.agg.samlmu(y)
lmrd(lmom)

####

#### https://stackoverflow.com/questions/33025385/fitted-gev-df-parameters-different-results-for-different-packages-lmomrfa-lm
####
####
####
#### https://rdrr.io/cran/lmom/man/lmrd.html

out <- annual.agg.pel(distrib="gpa",x=y,lmom=lmom)
###
library(ggplot2)
gg <- ggplot()+geom_point(aes(x=dd,y=l_1),data=lmom)+theme_bw()
gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=lmom)+theme_bw()
gg <- ggplot()+geom_point(aes(x=dd,y=l_2/l_1),data=lmom)+theme_bw()
gg <- ggplot()+geom_point(aes(x=dd,y=t_3),data=lmom)+theme_bw()
gg <- ggplot()+geom_point(aes(x=dd,y=t_4),data=lmom)+theme_bw()
###
gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=lmom)+geom_smooth(aes(x=log(dd),y=log(l_1)),formula= y ~ x,method = "lm",col = "blue",data=lmom)+theme_bw()

gg <- gg+labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                   "Intercept =",signif(fit$coef[[1]],5 ),
                   " Slope =",signif(fit$coef[[2]], 5),
                   " P =",signif(summary(fit)$coef[2,4], 5)))


####
####




## https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

ggplotRegression <- function (fit,col="blue") {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = col) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
  
fit1 <- lm(log(iris$Sepal.Length) ~ Petal.Width^2, data = iris)
ggplotRegression(fit1)
fit2 <- lm(log(lmom$l_1) ~ log(lmom$dd)) 
ggplotRegression(fit2)
##,data=lmom)
## https://www.youtube.com/watch?v=UtM3wum6bo0


