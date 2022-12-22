NULL
#' Sample L-Moments of annual maxima or minima values 
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param aggr.name,dd.name optional column mane for \code{x}. See function usage.
#' @param ... further arguments for \code{\link{samlmu}}
#'
#' @export
#' 
#' @importFrom lmom samlmu
#' @examples 
#' 
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#' PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' y <- annual.agg(x,dd=1:10,time)
#' out <- annual.agg.samlmu(y)
#'
#' lmrd(out)
#'
#' summary(lm(log(l_1) ~ log(dd),data=out))
#'
annual.agg.samlmu <- function(x,aggr.name="aggr",dd.name="dd",...) {
  
  x <- as.data.frame(x)
  u <- unique(x[,dd.name])
  out <- x[,aggr.name] %>% split(x[,dd.name]) %>% lapply(FUN=samlmu,...)
  out <- out %>% as.data.frame() %>% t() %>% as.data.frame()
  out[,dd.name] <- u
  
  return(out)
}
NULL
#'
#' @rdname annual.agg.samlmu
#' @export
#' 
yearly.agg.samlmu <- function(x,...) {
  
  out <- annual.agg.samlmu(x,...)
  return(out)
}