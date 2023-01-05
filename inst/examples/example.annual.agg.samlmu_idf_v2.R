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
out <- annual.agg.samlmu(y)
lmrd(out)

log_l_1 <- log(out$l_1)
log_t <- log(out$l_2/out$l_1)
log_l_2 <- log(out$l_2)
log_t3 <- log(out$t_3)
log_l_3 <- log(out$t_3*out$l_2)
log_t4<- log(out$t_4)
log_l_4 <- log(out$t_3*out$l_2)
#####
log_dd <- log(out$dd)
fits <-list()
fits[[1]] <- lm(log_l_1 ~ log_dd)
fits[[2]] <- lm(log_t ~ log_dd)
##fits[[2]] <- lm(log_l_2 ~ log_dd)
fits[[3]] <- lm(log_t3 ~ log_dd)
fits[[4]] <- lm(log_t4 ~ log_dd)

##summary(fit)
library(ggplot2)
##
ggs <- list()
for (i in 1:length(fits)) {
  fit <- fits[[i]]
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




