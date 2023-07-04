NULL
#'
#' Mirror of \code{\link{cdf}}: probability distribution fitting with L-Moments
#'
#' @param x vector of variable values (pe.g. precipitation intensity).
#' @param para object returned by \code{\link{annual.agg.pel}}.
#' @param aggr.name,dd.name column names. see \code{\link{annual.agg.samlmu}} or \code{\link{annual.agg.pel}}.
#' @param f.name column name for the probabilities/frequancies. Default is \code{"f"}.
#' @param dd_formatter string formatter for duration in the function value (currently not used).
#' @param is_depth logical. Default is \code{FALSE}. It must be set \code{TRUE} in case \code{x} refers to precipitation depth. 
#' @param ... further arguments. 
#'
#'
#' @return a data frame containg values, probabilities/frequencies and duration values
#' @export
#' @examples 
#'
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#'                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' y <- annual.agg(x,dd=1:20,time) 
#' lmom <- annual.agg.samlmu(y)
#' ## TO CONTUE
#' y <- annual.agg(x,dd=1:5,time) 
#' lmom <- annual.agg.samlmu(y)
#' lmrd(lmom)
#' z <- annual.agg.pel(distrib="gpa",x=y,lmom=lmom)
#' ff <- annual.agg.qua(f=c(0.5,0.9),para=z)
#' out <- annual.agg.cdf(x=ff$aggr,para=z)
#'
#' out[out$f %in% ff$f,names(ff)]
#' ff


annual.agg.cdf <- function(x,para,f.name="f",aggr.name=NA,dd.name=NA,is_depth=FALSE,dd_formatter=NA,...)
{
  
 
  if (is.null(aggr.name)) aggr.name <- NA
  if (is.null(dd.name)) dd.name <- NA
 ## if (is.null(dd_formatter)) dd_formatter <- NA
  
  ###
  if (is.na(aggr.name)) aggr.name <- attr(para,"aggr.name")
  if (is.na(dd.name)) dd.name <- attr(para,"dd.name")
  if (is.null(dd_formatter)) dd_formatter <- attr(para,"dd_formatter")
 # dd <- attr(para,"lmom")[,dd.name]
  #names(dd) <- sprintf(dd_formatter,dd)
  #str(x)
  if (x[1]=="sample") x <- attr(para,"x")[,aggr.name] ## EC 20230531 20230626

  out <- para %>% lapply(FUN=function(zz,xx,is_ddepth,ddd.name,...){
        ddd <- as.numeric(attr(zz,ddd.name))
        if (is_ddepth) xx <- xx/ddd    
        ff <- cdf(para=zz,x=xx,...)
        o <- data.frame(x=xx,f=ff,dd=ddd)
        return(o)
  },x=x,is_ddepth=is_depth,ddd.name=dd.name,...) %>% do.call(what="rbind")
  
  names(out)[names(out)=="x"] <- aggr.name
  names(out)[names(out)=="f"] <- f.name
  names(out)[names(out)=="dd"] <- dd.name
  return(out) 
}



### 'Yearly*" counterpart
NULL
#'
#'
#' @rdname annual.agg.cdf
#' @export
#' 
#' 
yearly.agg.cdf <- function(...) {
  
  out <- annual.agg.cdf(...)
  return(out)
}












