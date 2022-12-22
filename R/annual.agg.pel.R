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
#' 
#' 

annual.agg.pel <- function(distrib=c("exp","gam","gev","glo","gpa","gno","gum","kap","ln3","nor","pe3","wak","wei"),
                  x=x,lmom=lmom         
) {
  
  out <- NULL
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










