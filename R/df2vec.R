NULL
#' Data frame (table) to vector 
#'
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param aggr.name,dd.name,index.name optional column mane for \code{x}. See function usage.
#' @param dd_formatter string formatter for duration in the function value
#'
#' @export
#' @seealso \code{\link{vec2df}}
#' @examples 
#'
#' ## TO DO
#' #' 
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#' PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' out <- annual.agg(x,time)
#' out2 <- df2vec(out)
#' out3 <- vec2df(out2)
#' 
df2vec <- function(x,aggr.name="aggr",index.name="index",dd.name="dd",dd_formatter="D%03d") {
  
  if (is.null(x)) return(NULL)
  x <- as.data.frame(x)
  
  if (is.numeric(x[,dd.name])) x[,dd.name] <- sprintf(dd_formatter,x[,dd.name])
  out <- x[,aggr.name]
  names(out) <- paste(x[,dd.name],x[,index.name],sep="_")
  return(out)
}

