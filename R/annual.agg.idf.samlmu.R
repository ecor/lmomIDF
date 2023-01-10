NULL
#' Sample L-Moments of annual maxima or minima values 
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param lmom object returned by \code{\link{annual.agg.samlmu}} or \code{NULL}. 
#' @param aggr.name,dd.name optional column mane for \code{x}. See function usage.
#' @param nn_moms column names of \code{lmom} whose l-moments  are scaled with duration event. 
#' @param use_ggplot logical. if it is \code{TRUE} (default) regression plot is created. See function result descriptions. 
#' @param col argument for \code{ggplot2} functions. It is used if \code{use_ggplot==TRUE}. 
#' @param like_lmom logical. If it is \code{TRUE}, function returns a data frame formatted like \code{lmom} to be used for \code{\link{annual.agg.pel}}, otherwise it returns the outcome of \code{\link{samlmu}} .
#' @param ... further arguments for \code{\link{samlmu}}
#'
#' @export
#' 
#' @importFrom stats glm
#' @importFrom ggplot2 aes_string geom_point labs stat_smooth
#' @return a data frame containing the L-moment of rainfall intensity for each duration category. Plus attributes, see function code. 
#' 
#' @seealso \code{\link{samlmu}]} , \code{\link{annual.agg.samlmu}}
#' @examples 
#' @examples 
#' 
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#' PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' y <- annual.agg(x,dd=1:5,time)
#' out <- annual.agg.idf.samlmu(y)
#' 
#' out_pel <- annual.agg.pel(distrib="gev",x=y,lmom=out)
#' set.seed(560)
#' out_qua <- annual.agg.qua(para=out_pel)
#' attr(out_qua,"boxplot")
#'
#' 
#' 
annual.agg.idf.samlmu <- function(x,lmom=NULL,aggr.name="aggr",dd.name="dd",nn_moms=c("l_1","l_2"),use_ggplot=TRUE,col="blue",like_lmom=TRUE,...) {
  
  x <- as.data.frame(x)
  if  (is.null(lmom)) {
    
    lmom <- annual.agg.samlmu(x=x,aggr.name=aggr.name,dd.name=dd.name,...)
  }
  
  ## INSERT POWER-LAW FORMULA 
  out <- lmom[,c(nn_moms,dd.name)] %>% reshape2::melt(id=dd.name)
  ######
  out$log_dd <- log(out$dd)
  out$log_lm <- log(out$value)
  names(out)[names(out)=="variable"] <- "lm_order"
  dd <- out$dd
  out <- out[,c("log_lm","lm_order","log_dd")] 
  
  fit <- glm(out)
  
  if (use_ggplot) {
  gg <- ggplot(fit$model, aes_string(x = "log_dd", y = "log_lm",group="lm_order"))+
    geom_point()+stat_smooth(method = "glm", col = col) +
    theme_bw()+
     labs(title = paste("log(lm) ~ log(dd)", 
                        "Slope =",signif(fit$coef["log_dd"], 5),
                        "P =",signif(summary(fit)$coef["log_dd",4], 5)))
  
  } else {
    gg <- NULL
  }
  n_idf <- as.numeric(fit$coef["log_dd"])
  
  xval <- x[,aggr.name]/x[,dd.name]^n_idf
  out0 <- samlmu(xval,...)
  if (like_lmom) {
    ddd <- unique(dd)
    out0 <- t(out0)
    out0 <- as.data.frame(out0)[rep(1,length(ddd)),]
    out0[,dd.name] <- ddd
    out0[,nn_moms] <- out0[,nn_moms]*out0[,dd.name]^(n_idf)
  }
  
  attr(out0,"lmom") <- lmom
  attr(out0,"nn_moms") <- nn_moms
  attr(out0,"fit") <- fit
  attr(out0,"gg") <- gg
  attr(out0,"n_idf") <- n_idf
  
  ##str(out)
  
  
  
  return(out0)
}
NULL
#'
#' @rdname annual.agg.idf.samlmu
#' @export
#' 
yearly.agg.idf.samlmu <- function(x,...) {
  
  out <- annual.agg.idf.samlmu(x,...)
  return(out)
}