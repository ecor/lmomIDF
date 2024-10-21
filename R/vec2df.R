NULL
#' Vector to Data frame (table) or Tibble
#'
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}}
#' @param nn names of \code{x}
#' @param aggr.name,dd.name,index.name optional column mane for \code{x}. See function usage.
#' @param dd_formatter string formatter for duration in the function value
#' @param numeric_dd logical. Default is \code{TRUE}.
#' @param numeric_index logical. Default is \code{FALSE}.
#' @param return_tibble logical 
#' @export
#' 
#' @importFrom stringr str_split
#' @importFrom dplyr as_tibble
#' @seealso \code{\link{df2vec}}
#' 
#' 
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
vec2df <- function(x,nn=names(x),aggr.name="aggr",index.name="index",dd.name="dd",dd_formatter="D%03d",numeric_dd=TRUE,numeric_index=FALSE,return_tibble=TRUE) {
  
  if (is.null(x)) return(NULL)
  if (length(x)!=length(nn)) stop("Mismatch between x and nn lengths")
  ##out <- data.frame(aggr=as.numeric(x))
  out <- data.frame(aggr=x) ## EC 20241019
  out$dd <- str_split(nn,"_") %>% sapply(FUN=function(x){x[1]})  
  if (numeric_dd) out$dd <- out$dd %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>%     as.numeric()
  out$index <- str_split(nn,"_") %>% sapply(FUN=function(x){paste(x[-1],collapse="_")}) ## EC20241019sapply(FUN=function(x){x[2]})  
  if (numeric_index) out$index <- out$dd %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>%     as.numeric()

  names(out)[names(out)=="aggr"] <- aggr.name
  names(out)[names(out)=="dd"]   <- dd.name
  names(out)[names(out)=="index"]   <- index.name
  
  out <- out[,c(dd.name,index.name,aggr.name)]
  if (return_tibble) out <- as_tibble(out)
  
  return(out)
}

