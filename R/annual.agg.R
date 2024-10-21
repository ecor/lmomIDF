NULL
#' Annual (maxima or minima) (default) aggregation
#' 
#' 
#' @param x vector or time series
#' @param time vector of time index (e.g. \code{Date} or \code{POSIXct})
#' @param index index aggregation vector. Default is \code{as.character(lubridate::year(time))} (e.g every year)
#' @param aggr.fun atomic aggregation function (e.g. \code{\link{max}} or \code{\link{min}}), accepting \code{na.rm} argument.
#' @param na.rm argument for \code{aggr.fun}
#' @param dd duration for the moving window. More than 1 duration can be assigned.
#' @param dd_formatter string formatter for duration in the function value
#' @param numeric_dd logical. Default is \code{TRUE}.
#' @param numeric_index logical. Default is \code{FALSE}.

#' @param aggr.name,dd.name,index.name optional column names of function results. See function usage.
#' @param return_vector logical. If \code{TRUE} function return a vector through \code{\link{df2vec}}. Default is \code{FALSE}.
#' @param order_time logical argument, if it \code{TRUE} \code{x} is ordered following increasing \code{data} 
#' @param speed_up logical argument 
#' @param filter,method,sides,... further arguments for \code{stats::\link[stats]{filter}}
#'
#' @seealso \code{stats::\link[stats]{filter}},\code{\link{max}},\code{\link{min}}
#' 
#' @return a data frame or similar with index (e.g. year or year-month), duration (column name: \code{"dd"})  
#' and the aggregated value  (column name: \code{"aggr"})
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom reshape2 melt
#' @importFrom dplyr .data group_by summarize ungroup
#' @importFrom lubridate year
#' @examples 
#' 
#' library(RMAWGEN)
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#' PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' out <- annual.agg(x,time)
#' out2 <- yearly.agg(x,time)
#' 
#' 
#' outv <- annual.agg(x,time,return_vector=TRUE)
#' outv1 <- vec2df(outv)
#' 
#' if (!identical(out,outv1)) stop("Something went wrong!")
#' 
#' library(lmom)
#' library(dplyr)
#' outp <- out %>% filter(dd==2)
#' evplot(outp$aggr)
#' 

#' 
#' 
#' outm <- annual.agg(x,time,index=sprintf("%04d_%02d",lubridate::year(time),
#' lubridate::month(time)))
#' outmd <- annual.agg(x,time,index=sprintf("%04d_%02d",lubridate::year(time),
#' lubridate::month(time)),aggr.fun=function(l,...) {which.max(l)})
#'
#' ### intesity greater or equal to 10 mm per day. 
#' funthres <- function(r,valmin=10,...) {length(which(r>=valmin))}
#' out_funthres <- annual.agg(x,time,index=sprintf("%04d_%02d",lubridate::year(time),
#' lubridate::month(time)),aggr.fun=funthres)
#' outv_funthres <- annual.agg(x,time,index=sprintf("%04d_%02d",lubridate::year(time),
#' lubridate::month(time)),aggr.fun=funthres,return_vector=TRUE)
#' outv_funthres <- monthly.agg(x,time,aggr.fun=funthres,return_vector=TRUE)
#' out2_funthres <- vec2df(outv_funthres)
#'
#' if (!identical(out_funthres,out2_funthres)) stop("Something went wrong!")
#'


# @param format argument for \code{\link{as.character}} used to create \code{index} from \code{time}. DEPREC
#format="%Y"

annual.agg <- function(x,time,index=as.character(lubridate::year(time)),dd=c(1,2,5),aggr.fun="max",na.rm=TRUE,
                       dd_formatter="D%03d",numeric_dd=TRUE,numeric_index=FALSE,aggr.name="aggr",dd.name="dd",index.name="index",filter=lapply(dd,function(dd){rep(1,dd)/dd}),method="convolution",sides=1,return_vector=FALSE,order_time=TRUE,speed_up=FALSE,...) {
  
  cond1 <- all(diff(time)==diff(time)[1])
  if (!cond1) {
    stop("time must have constant intervals")
  }
  
  out <- NULL
  ##
  time <- sort(time)
  if (order_time) x <- x[order(time)]
  index <- index[order(time)]
  
  ##
  if (!is.list(filter)) filter <- list(filter)
  if (length(dd)==length(filter)) names(filter) <- sprintf(dd_formatter,dd)
  out <- lapply(FUN=stats::filter,x=x,filter,method=method,sides=sides,...) 
  out <- lapply(out,FUN=as.numeric) 
  if (speed_up) {
    out <- out %>% lapply(FUN=tapply,get(aggr.fun),INDEX=index) 
    out <- unlist(out)
    names(out) <- str_replace_all(names(out),"[.]","_")
    return(out)
  }  
  out <- as.data.frame(out)
  
  
  out$time <- time
  out$index <- index ## as.character(out$time,format=format)
  out <- reshape2::melt(out,id=c("time","index"))
  names(out)[names(out)=="variable"] <- "dd"
 

  ######
  ######
  if (is.character(aggr.fun)) aggr.fun <- get(aggr.fun)
  #out <- 
  #####
  ######
  out <- out %>% group_by(.data$dd,.data$index) %>% summarize(aggr=aggr.fun(.data$value,na.rm=na.rm)) %>% ungroup()
  if (numeric_dd) out$dd <- as.character(out$dd) %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>% as.numeric()
  if (numeric_index) out$index <- as.character(out$index) %>% str_replace_all("[A-Z]","") %>% str_replace_all("[a-z]","") %>% as.numeric()
  ###
  iaggr  <- which(names(out)=="aggr")
  idd    <- which(names(out)=="dd")
  iindex <- which(names(out)=="index")
  ###
  names(out)[iaggr] <- aggr.name
  names(out)[idd] <- dd.name
  names(out)[iindex] <- index.name
  ###
  if (return_vector) out <- df2vec(out,aggr.name=aggr.name,index.name=index.name,dd.name=dd.name,dd_formatter=dd_formatter)
  ###
  return(out)
}

NULL
#'
#' @rdname annual.agg
#' @export
#' 
yearly.agg <- function(x,time,...) {
  
  out <- annual.agg(x,time,...)
  return(out)
}


NULL
#'
#' @rdname annual.agg
#' @export
#' 
monthly.agg <- function(x,time,index=sprintf("%04d_%02d",lubridate::year(time),lubridate::month(time)),...) {
  
  out <- annual.agg(x,time,index=index,...)
  return(out)
}


