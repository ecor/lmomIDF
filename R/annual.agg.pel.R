NULL
#' Mirror of \code{\link{pel...}}: probability distribution fitting with L-Moments
#' 
#' @param distrib character string incating the probability distribution to fit 
#' @param lmom object returned by \code{\link{annual.agg.samlmu}} or \code{\link{yearly.agg.samlmu}}
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param aggr.name,dd.name optional column names for \code{x} and/or \code{lmom}. See function usage.
#' @param dd_formatter string formatter for duration in the function value
#' @param alternative,exact arguments for \code{\link{ks.test}}
#' @param ... further arguments  
#' @importFrom stats ks.test
#' @importFrom lmomPi cdf pel_lmom
#'
#' @export
#' @examples 
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#'                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' y <- annual.agg(x,dd=1:20,time) 
#' lmom <- annual.agg.samlmu(y)
#' lmrd(lmom)


annual.agg.pel <- function(distrib=c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3","nor","pe3","wak","wei")[3],
                  x,lmom=lmom,dd.name="dd",dd_formatter="D%03d",aggr.name="aggr",
                          alternative = c("two.sided", "less", "greater"),
                          exact = NULL,
                  ...         
) {
  
  ##lwhich(names(lmom)==dd.name)
  distrib <- distrib[1]
  dds <- lmom[,dd.name]
  names(dds) <- sprintf(dd_formatter,dds)
  
  
  out <- apply(lmom,MARGIN=1,FUN=pel_lmom,distrib=distrib,simplify=FALSE)
  names(out) <- names(dds)
  ###

  
  ####
  x <- as.data.frame(x)
  for(it in names(dds)) {
    xval <- x[which(x[,dd.name]==dds[it]),aggr.name]
    attr(out[[it]],dd.name) <- dds[it]
    attr(out[[it]],"ks.test") <- ks.test(x=xval,y=cdf,para=out[[it]],alternative = alternative,exact=exact)
  }  

  lmom$p.value <- lapply(out,attr,which="ks.test") %>% sapply(FUN=function(t){t$p.value})
  attr(out,"lmom") <- lmom
  attr(out,"x") <- x
  attr(out,"dd.name") <- dd.name
  attr(out,"dd_formatter") <- dd_formatter
  attr(out,"aggr.name") <- aggr.name
  
  ###
  
  ###  
  ###  
  ### 
  return(out) 
}


NULL

#' @rdname annual.agg.pel
#' @export
#' 
#' 
annual.agg.pel_x <- function(x,...) {
	
	out <- annual.agg.pel(...,x=x)
	return(out)
}
NULL
#' @rdname annual.agg.pel
#' @export
#' 
#' 
annual.agg.pel_lmom <- function(lmom,...) {
  
  out <- annual.agg.pel(...,lmom=lmom)
  return(out)
}

### 'Yearly*" counterpart
NULL
#' @rdname annual.agg.pel
#' @export
#' 
#' 
yearly.agg.pel <- function(...) {
  
  out <- annual.agg.pel(...)
  return(out)
}

NULL

#' @rdname annual.agg.pel
#' @export
#' 
#' 
yearly.agg.pel_x <- function(x,...) {
  
  out <- annual.agg.pel_x(x,...)
  return(out)
}
NULL
#' @rdname annual.agg.pel
#' @export
#' 
#' 
yearly.agg.pel_lmom <- function(lmom,...) {
  
  out <- annual.agg.pel_lmom(lmom,...)
  return(out)
}










