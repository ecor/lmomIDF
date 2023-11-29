NULL
#'Confidence Interval 
#'
#' Bootstrap confidence interval for  Probability / Frequency / Return Period Extimation (for event of a specific duration). Function experimental.
#'
#' 
#' @param x sample or \code{x} argument. See \code{\link{annual.agg.cdf}}
#' @param para object returned by \code{\link{annual.agg.pel}}.
#' @param aggr.name,dd.name column names. see \code{\link{annual.agg.samlmu}} or \code{\link{annual.agg.pel}}.
#' @param dd_formatter see \code{\link{annual.agg.cdf}}
#' @param f.name column name for the probabilities/frequencies. Default is \code{"f"}.
#' @param jj.name column name for plotting position
#' @param probability_distribution_attrname see \code{\link{qua}}
#' @param boot.ci.type,conf see \code{type,conf} in \code{\link{boot.ci}}
#' @param R see \code{R} in \code{\link{boot}}
#' @param extreme.f last point parameter (documention to be completed)
#' @param ... further arguments. 
#'
#'
#' @seealso \code{\link{annual.agg.idf.samlmu}},\code{\link{annual.agg.qua}}
#' 
#' @importFrom stringr str_detect
#' @importFrom boot boot boot.ci 
#'
#' @export
#' @examples 
#' 
#' library(RMAWGEN)
#' library(ggplot2)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#'                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#' y <- annual.agg(x,dd=1,time) ## duration: 1 day 
#' ##
#' y$aggr[y$aggr<0] <- NA
#' y <- y[!is.na(y$aggr),]
#'
#' ##
#'
#' lmom <- annual.agg.samlmu(y)
#'
#'
#' z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
#' ####
#' out <- annual.agg.conf.interval(para=z) ##,extreme.f = NULL) ##,boot.ci.type="perc")
## Add Return Period column 
#' out$rt <- 1/(1-out$f)
#' out$rt_min <- 1/(1-out$f_high)
#' out$rt_max <- 1/(1-out$f_low)
#' out$rt_jj <- 1/(1-out$jj)
#' ## ggplot plot 
#' gg <- ggplot()+geom_point(aes(x=rt_jj,y=aggr_obs),data=out)
#' gg <- gg+geom_line(aes(x=rt,y=aggr_obs),data=out)+theme_bw()
#' gg <- gg+geom_ribbon(aes(x=rt,ymax=aggr_high,ymin=aggr_low),data=out,fill="grey",alpha=0.5)
#' gg <- gg+scale_x_log10()
#' gg <- gg+xlab("Return Period [yr]")+ylab("Precipitation [mm]")
#' ##


annual.agg.conf.interval  <- function(x="sample",para,f.name="f",aggr.name=NA,dd.name=NA,jj.name="jj",dd_formatter=NA,R=9999,
                                 boot.ci.type=c("perc","norm","basic", "stud"),conf=0.95,probability_distribution_attrname = "probability_distrib",extreme.f=c(0.99,0.995),...)
                                 
                                 ####use_ggplot=TRUE,xlab="duration",ylab="value",fill=c("#ca0020","#0571b0","#bababa","#bababb","#bababc"),color_idf=brewer.pal(name="YlOrRd",n=length(f)), nrun=5,n_idf=NULL,remove_distrib_from_boxplot=FALSE,idf_curve=TRUE,possible_return_null=FALSE,...)
  
{
  if (is.null(dd.name)) dd.name <- NA
  if (is.null(aggr.name)) aggr.name <- NA
  if (is.null(dd_formatter)) dd_fprmatter <- NA
  ### 
  if (is.na(dd.name)) dd.name <- attr(para,"dd.name")
  if (is.na(aggr.name)) aggr.name <- attr(para,"aggr.name")
  if (is.na(dd_formatter)) n_idf <- attr(para,"dd_formatter")
  ###
  ## only a 1 
  dd_formatter <- attr(para,"dd_formatter")
  dd <- sapply(para,FUN=attr,dd.name)
  names(dd) <- sprintf(dd_formatter,dd)
  
  
  ######
  ######
  ######
  ######
  if (x[1]=="sample") {
   ## para <- para
    x <- attr(para,"x")[,aggr.name] ## EC 20230531 20230626
    is_sample <- array(TRUE,length(x))
    nsample <- length(x)
    if (length(extreme.f)>0){
    xe <- unlist(qua(f=extreme.f,para=para,probability_distribution_attrname=probability_distribution_attrname)) ###,f.name=f.name,aggr.name=aggr.name,dd.name=dd.name,probability_distribution_attrname=probability_distribution_attrname,...)
    x<- c(x,xe)
    is_sample <- c(is_sample,array(FALSE,length(xe)))
    }  
  } else {
    
    is_sapmle <- array(FALSE,length(x))
    nsample <- length(x)
  }
  
 
  out <- annual.agg.cdf(x=x,para=para,f.name=f.name,aggr.name=aggr.name,dd.name=dd.name,probability_distribution_attrname=probability_distribution_attrname,dd_formatter=dd_formatter,...)
  min_dd <- min(out[,dd.name],na.rm=FALSE)
  min_dd_sn <- sprintf(dd_formatter,min_dd)
  out <- out[out[,dd.name]==min_dd,]
  out$is_sample <- is_sample
  out <- out[order(out[,aggr.name]),]
  out <- out[order(out$is_sample),]
  out[,jj.name] <- as.numeric(NA)
  out[which(out$is_sample),jj.name] <- (1:nsample)/(nsample+1)
  ####
  ####
  ####
  
  xdata <- out[which(out$is_sample),aggr.name] ## THE SAMPLE OF PARS 
   ## out$data[out$data[,dd.name]==dd,aggr.name]
  
  ####
  distrib <- attr(para[[min_dd_sn]],probability_distribution_attrname)
  mle=list(para=para[[min_dd_sn]])
### TO REMOVE EC 20230629  statistic <- function(data,x=out[,f.name],mle=mle) {quantile(probs=x,x=data)}
###
###
###
  statistic <- function(data,x=out[,f.name],distrib="gev") {
    lmom <- lmom::samlmu(x=data,nmom=5,sort.data=TRUE,ratios=TRUE)
    param <- lmomPi::pel(distrib=distrib,lmom=lmom)
    o <- qua(para=param,f=out[,f.name],distrib=distrib)
  }  
  
  ran.gen <- function(data,mle=list(para=para[[min_dd_sn]]),...) { ###  ran.gen <- function(data,mle=list(para=para[[dd_sn]]),...) {
        nn <- nrow(data)
        if (length(nn)==0) nn <- NA
        if (is.na(nn)) nn <- length(data)
        qdata <- runif(nn)
        out_rg <- qua(f=qdata,para=mle$para)
        out_rg[out_rg<0] <- 0
        return(out_rg)
  }
  #out$qua2 <- out$qua %>% group_by(dd) %>% mutate(aggr_q=qqff(f=f,dd=dd),) %>% ungroup()
  boot_ <- boot(data=xdata,statistic = statistic,R=R,sim="parametric",ran.gen=ran.gen,mle=mle,distrib=distrib,...)
 
  #BOOTSTRAP HERE
  boot.ci.type <- boot.ci.type[1]
  ####oo <<- boot_
  boot.ci_ <- lapply(FUN=boot.ci,boot.out=boot_,type=boot.ci.type,conf=conf,X=1:length(boot_$t0))  ##index(boot_$t0))
  ####
  boot.ci.2_ <- lapply(X=boot.ci_,FUN=function(x,type=boot.ci.type) {(x[str_detect(names(x),type)][[1]] %>% rev())[c(2,1)]}) %>% do.call(what="rbind") %>% as.data.frame()
  names(boot.ci.2_) <- paste(aggr.name,c("low","high"),sep="_")
 ## boot.ci.2_[paste0aggr.name,c("low","high"),sep="_")
  if (boot.ci.type=="norm") {
    
    out[,paste0(aggr.name,"_obs")] <- (boot.ci.2_[,1]+boot.ci.2_[,2])/2
  } else {  
    out[,paste0(aggr.name,"_obs")] <- statistic(data=xdata)
  } 
  out <- cbind(out,boot.ci.2_)
  out[,paste(f.name,c("low"),sep="_")] <- out[,paste(aggr.name,c("low"),sep="_")] %>% cdf(para=mle$para)
  out[,paste(f.name,c("high"),sep="_")] <- out[,paste(aggr.name,c("high"),sep="_")] %>% cdf(para=mle$para)
   ####
  
  ####
    
    ###
  return(out) 
}



### 'Yearly*" counterpart
NULL
#'
#'
#' @rdname annual.agg.conf.interval
#' @export
#' 
#' 
yearly.agg.conf.interval <- function(...) {
  
  out <- annual.agg.conf.interval(...)
  return(out)
}












