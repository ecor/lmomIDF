NULL
#' Sample L-Moments of annual maxima or minima values 
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}} or a vector which will be processed through \code{\link{vec2df}}
#' @param aggr.name,dd.name optional column mane for \code{x}. See function usage.
#' @param dd_formatter argument used by \code{\link{vec2df}}
#' @param ... further arguments for \code{\link{samlmu}}
#'
#' @export
#' 
#' @importFrom lmom samlmu
#' 
#' @return a data frame containing the L-moment of rainfall intensity for each duration category. 
#' 
#' @seealso \code{\link{samlmu}} 
#' 
#' 
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
#' y1 <- df2vec(y)
#' out1 <- annual.agg.samlmu(y1)
#' 
#' if (max(abs(out1-out),na.rm=TRUE)>1e-8) stop("Something went wrong!") 
#' for (it in names(attributes(out))) {
#'
#'    if (!identical(attr(out,it),attr(out1,it))) {
#'       msg <- sprintf("Something went wrong in %s!",it)
#'       stop(msg)
#'    
#'    }
#'      
#'   
#' }
#' 
#' 
#' lmrd(out)
#'
#' log_l_1 <- log(out$l_1)
#' log_dd <- log(out$dd)
#' fit <- lm(log_l_1 ~ log_dd)
#' summary(fit)
#' library(ggplot2)
#' gg <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1]))+
#' geom_point()+stat_smooth(method = "lm", col = "blue") +     
#' labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#'                                                   "Intercept =",signif(fit$coef[[1]],5 ),
#'                                                   " Slope =",signif(fit$coef[[2]], 5),
#'                                                   " P =",signif(summary(fit)$coef[2,4], 5)))
#' ##gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=out)+theme_bw()
#' gg <- gg+theme_bw()
#' gg
#' 
#' ## Case of a GSOD dataset time series
#' \donttest{
#' 
#' library(GSODR)
#' library(magrittr)
#' library(data.table)
#' library(dplyr)
#' library(ggplot2)
#' set.seed(123)
#' 
#' years <- 1937:2020
#' gsod <- get_GSOD(years=years,station="623180-99999") ##ALEXANDRIA INTL EG 623180-99999
#' prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>%
#'  mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d")) 
#' 
#' dds <- range(prec$YEARMODA)
#' ## See GSODR documentation
#' yymmdds <- seq(from=dds[1],to=dds[2],by="day")
#' prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)
#' y <- annual.agg(x=prec$PRCP,dd=1:5,time=prec$YEARMODA)
#' ### y$aggr cannot be -Inf or +Inf
#' y$aggr[y$aggr==-Inf] <- NA
#' out <- annual.agg.samlmu(y)
#' lmrd(out)
#' log_l_1 <- log(out$l_1)
#' log_dd <- log(out$dd)
#' fit <- lm(log_l_1 ~ log_dd)
#' summary(fit)
#' 
#' ggg <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1]))+
#' geom_point()+stat_smooth(method = "lm", col = "blue") +     
#' labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#'                                                   "Intercept =",signif(fit$coef[[1]],5 ),
#'                                                   " Slope =",signif(fit$coef[[2]], 5),
#'                                                   " P =",signif(summary(fit)$coef[2,4], 5)))
#' ##gg <- ggplot()+geom_point(aes(x=log(dd),y=log(l_1)),data=out)+theme_bw()
#' ggg <- ggg+theme_bw()
#' ggg
#' 
#'
#'
#'
#' }
#'   
#'
annual.agg.samlmu <- function(x,aggr.name="aggr",dd.name="dd",dd_formatter="%03d",nn=names(x),...) {
  
  
  if (is.vector(x)) x <- vec2df(x,nn=nn,aggr.name=aggr.name,dd.name=dd.name,dd_formatter=dd_formatter)
  x <- as.data.frame(x)
  u <- unique(x[,dd.name])
  out <- x[,aggr.name] %>% split(x[,dd.name]) %>% lapply(FUN=samlmu,...)
  out <- out %>% as.data.frame() %>% t() %>% as.data.frame()
  out[,dd.name] <- u
  
  ####
  
  
  
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