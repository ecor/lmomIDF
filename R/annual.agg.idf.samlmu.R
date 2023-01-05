NULL
#' Sample L-Moments of annual maxima or minima values 
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param lmom object returned by \code{\link{annual.agg.samlmu}} or \code{NULL}. 
#' @param aggr.name,dd.name optional column mane for \code{x}. See function usage.
#' @param ... further arguments for \code{\link{samlmu}}
#'
#' @export
#' 
#'
#' 
#' @return a data frame containing the L-moment of rainfall intensity for each duration category. LOREM IPSUM
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
#' y <- annual.agg(x,dd=1:10,time)
#' out <- annual.agg.idf.samlmu(y)
#' 
#' 
#' 
annual.agg.idf.samlmu <- function(x,lmom=NULL,aggr.name="aggr",dd.name="dd",nn_moms=c("l_1","l_2"),use_ggplot=TRUE,col="blue",...) {
  
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
  n_idf <- fit$coef["log_dd"]
  
  xval <- x[,aggr.name]/x[,dd.name]^n_idf
  out0 <- samlmu(xval,...)
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