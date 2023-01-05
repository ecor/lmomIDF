rm(list=ls())
library(lmomIDF)
library(lubridate)
library(RMAWGEN)
library(dplyr)
library(lmomPi)
##
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.R")
source("~/local/rpackages/jrc/lmomIDF/R/annual.agg.samlmu.R")
##
data(trentino)
time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
x <- PRECIPITATION$B8570

y <- annual.agg(x,dd=1:5,time) 
##y$aggr <- y$aggr*y$dd
nmom=3
outr <- annual.agg.samlmu(y)
lmrd(outr)

### summary(lm(log(outr$t_4) ~ log(outr$dd)))
out <- annual.agg.samlmu(y,ratios=FALSE,nmom=nmom)

#####
log_dd <- log(out$dd)
fits <-list()
######


##summary(fit)
library(ggplot2)
##
ggs <- list()
##
for (i in 1:nmom) {
  
  l_dd <- log(out$dd)
  l_lm <- log(out[,sprintf("l_%d",i)])
  fit <- lm(l_lm ~ l_dd)
  fits[[i]] <- fit
  gg <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1]))+
   geom_point()+stat_smooth(method = "lm", col = "blue") +     
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))
##gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=out)+theme_bw()
  gg <- gg+theme_bw()
  ggs[[i]] <- gg

}

out2 <- out
dd.name <- "dd"
for (i in 1:nrow(out)) out2[i,names(out)!=dd.name] <- out[i,names(out)!=dd.name]/out[1,names(out)!=dd.name]
out2m <- melt(out2,id=dd.name)
l_dd <- log(out2m$dd)
l_lm <- log(out2m$value)
fit <- lm(l_lm ~ l_dd)
gg <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1]))+
  geom_point()+stat_smooth(method = "lm", col = "blue") +     
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))
##gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=out)+theme_bw()
gg <- gg+theme_bw()




stop("HERE")
n_idf <- fits[[1]]$coef[[2]]

xval <- y$aggr/y$dd^n_idf
new_lmom <- samlmu(xval,ratios=TRUE)


### RATO WITH ALL DIMANSIONAL L_MOMENTS : 

###out <- annual.agg.samlmu(y)
####for (i in 1:nrow(out)) out2[i,1:4] <- out[i,1:4]/out[1,1:4]


####

#### https://stackoverflow.com/questions/33025385/fitted-gev-df-parameters-different-results-for-different-packages-lmomrfa-lm
####
####
####
#### https://rdrr.io/cran/lmom/man/lmrd.html




