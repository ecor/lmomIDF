NULL
#'Generic function for \code{\link{pel...}}: probability distribution fitting with L-Moments
#' 
#' @param distrib character string incating the probability distribution to fit 
#' @param lmom object returned by \code{\link{annual.agg.samlmu}} or \code{\link{yearly.agg.samlmu}}
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#'
#'
#' @example 
#' 
#' ## TO DO
#' 

annual.agg.pel <- function(distrib=c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3","nor","pe3","wak","wei")[3],
                  x=x,lmom=lmom,dd.name="dd",dd_formatter="D%03d",aggr.name="aggr",
                          alternative = c("two.sided", "less", "greater"),
                          exact = NULL,
                  ...         
) {
  
  ##lwhich(names(lmom)==dd.name)
  distrib <- distrib[1]
  dds <- out[,dd.name]
  names(dds) <- sprintf(dd_formatter,dds)
  
  
  out <- apply(out,MARGIN=1,FUN=pel_lmom,distrib=distrib,simplify=FALSE)
  names(out) <- names(dds)
  ###

  
  ####
  x <- as.data.frame(x)
  for(it in names(dds)) {
    xval <- x[which(x[,dd.name]==dds[it]),aggr.name]
    attr(out[[it]],"ks.test") <- ks.test(x=xval,y=cdf,para=out[[it]],alternative = alternative,exact=exact)
  }  

  
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
  
  out <- annual.agg.pel(...,lmom=x)
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










