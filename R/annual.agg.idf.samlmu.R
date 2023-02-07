NULL
#' Sample L-Moments of annual maxima or minima values 
#'
#' @param x object returned by \code{\link{annual.agg}} or \code{\link{yearly.agg}} or a vector which will be processed through \code{\link{vec2df}}
#' @param lmom object returned by \code{\link{annual.agg.samlmu}} or \code{NULL}. 
#' @param aggr.name,dd.name optional column mane for \code{x}. See function usage.
#' @param nn_moms column names of \code{lmom} whose l-moments  are scaled with duration event. 
#' @param use_ggplot logical. if it is \code{TRUE} (default) regression plot is created. See function result descriptions. 
#' @param col argument for \code{ggplot2} functions. It is used if \code{use_ggplot==TRUE}. 
#' @param like_lmom logical. If it is \code{TRUE}, function returns a data frame formatted like \code{lmom} to be used for \code{\link{annual.agg.pel}}, otherwise it returns the outcome of \code{\link{samlmu}} .
#' @param dd_formatter argument used by \code{\link{vec2df}}
#' @param nnx names of \code{x}
#' @param ... further arguments for \code{\link{samlmu}}
#'
#' @return a data frame containing the L-moment of rainfall intensity for each duration category. Plus attributes, see function code. 
#' 
#' @seealso \code{\link{samlmu}},\code{\link{annual.agg.samlmu}}
#' 
#' @importFrom stats glm
#' @importFrom ggplot2 aes_string geom_point labs stat_smooth
#'
#' @export
#' @examples
#' 
#' library(lmomIDF)
#' library(RMAWGEN)
#' 
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#'                         PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#' 
#' y <- annual.agg(x,dd=1:5,time)
#' out <- annual.agg.idf.samlmu(y)
#' y1 <- df2vec(y)
#' out1 <- annual.agg.idf.samlmu(y1)
#' 
#' y2 <- y1
#' y2[] <- NA
#' out2 <- annual.agg.idf.samlmu(y2)
#' 
#' if (max(abs(out1-out),na.rm=TRUE)>1e-8) stop("Something went wrong!") 
#' for (it in names(attributes(out))) {
#'    if (it=="gg") { ## TO CHECK
#'    } else if (is.list(attr(out,it))) for (itn in names(attr(out,it))) {
#'          if (!identical(attr(out,it)[[itn]],attr(out1,it)[[itn]]) & !(itn %in% c("model","family","terms"))) { ## TO CHECK
#'             msg <- sprintf("Something went wrong in %s  %s!",it,itn)
#'             stop(msg)
#'          }
#'    } else if (!identical(attr(out,it),attr(out1,it))) {
#'       msg <- sprintf("Something went wrong in %s!",it)
#'       stop(msg)
#'    
#'    }
#'      
#'   
#' }
#' 
#' 
#'
#' 
#' 
#' out_pel <- annual.agg.pel(distrib="gpa",x=y,lmom=out)
#' set.seed(560)
#' f=c(0.5,0.8,0.9,0.95,0.98,0.99,0.999,0.9999)
#' out_qua <- annual.agg.qua(para=out_pel,f=f,remove_distrib_from_boxplot=TRUE)
#' 
#' \donttest{
#' 
#' ####
#' library(GSODR)
#' library(dplyr)
#'
#' set.seed(123)
#'
#' years <- 1937:2020
#' gsod <- get_GSOD(years=years,station="623180-99999") ##ALEXANDRIA INTL EG 623180-99999
#' prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>%
#'   mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d"))
#'
#' dds <- range(prec$YEARMODA)
#' ## See GSODR documentation
#' yymmdds <- seq(from=dds[1],to=dds[2],by="day")
#' prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)
#' y <- annual.agg(x=prec$PRCP,dd=1:5,time=prec$YEARMODA)
#' ### y$aggr cannot be -Inf or +Inf
#' y$aggr[y$aggr==-Inf] <- NA
#' lmom0 <- annual.agg.samlmu(y)
#' lmrd(lmom0)
#' lmom <- annual.agg.idf.samlmu(y)
#'
#'
#' z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
#' out_qua_gsod <- annual.agg.qua(f=f,para=z,remove_distrib_from_boxplot=TRUE)
#' attr(out_qua_gsod,"idf")
#' attr(out_qua_gsod,"ddf")
#' }
#' 
#' 
#' 









annual.agg.idf.samlmu <- function(x,lmom=NULL,aggr.name="aggr",dd.name="dd",nn_moms=c("l_1","l_2"),use_ggplot=TRUE,col="blue",like_lmom=TRUE,dd_formatter="%03d",nnx=names(x),...) {

  if (is.vector(x)) x <- vec2df(x,nn=nnx,aggr.name=aggr.name,dd.name=dd.name,dd_formatter=dd_formatter)
  x <- as.data.frame(x)
  if  (is.null(lmom)) {

    lmom <- annual.agg.samlmu(x=x,aggr.name=aggr.name,dd.name=dd.name,...)
  }
  xx1 <<- x
  lmom1 <<- lmom
  ## INSERT POWER-LAW FORMULA
  out <- lmom[,c(nn_moms,dd.name)] %>% reshape2::melt(id=dd.name)
  ######
  out$log_dd <- log(out$dd)
  out$log_lm <- log(out$value)
  names(out)[names(out)=="variable"] <- "lm_order"
  dd <- out$dd
  out <- out[,c("log_lm","lm_order","log_dd")]
  out$log_lm[out$log_lm==-Inf] <- NaN 
  out$log_lm[out$log_lm==Inf] <- NaN
  
  if (is.na(all(out$log_lm))) {
    condna <- TRUE
  } else {
    condna <- FALSE
  }
  
  ## INSET CONSTRAINT 
  ## https://stats.stackexchange.com/questions/3143/linear-model-with-constraints
  outz1 <<- out
  if (condna) out$log_lm <- 0 
  outz2 <<- out
  fit <- glm(out)
  ###
  outd <- out
  outd$log_lm <- out$log_lm+out$log_dd
  fitd <- glm(outd)
  ####
  if (condna) {
    out$log_lm <- NA
    n_idf <- as.numeric(NA)
    p_idf <- as.numeric(NA)
    n_ddf <- as.numeric(NA)
    p_ddf <- as.numeric(NA)
    
  }  else {
    n_idf <- as.numeric(fit$coef["log_dd"])
    p_idf <- as.numeric(summary(fit)$coef["log_dd",4])
    n_ddf <- as.numeric(fitd$coef["log_dd"])
    p_ddf <- as.numeric(summary(fitd)$coef["log_dd",4])
  }
  
  if (use_ggplot & !condna) {
  gg <- ggplot(fit$model, aes_string(x = "log_dd", y = "log_lm",group="lm_order"))+
    geom_point()+stat_smooth(method = "glm", col = col) +
    theme_bw()+
     labs(title = paste("log(lm) ~ log(dd)",
                        "Slope =",signif(fit$coef["log_dd"], 5),
                        "P =",signif(summary(fit)$coef["log_dd",4], 5)))

  } else {
    gg <- NULL
  }


  xval <- x[,aggr.name]/x[,dd.name]^n_idf
  out0 <- samlmu(xval,...)
  if (like_lmom) {
    ddd <- unique(dd)
    out0 <- t(out0)
    out0 <- as.data.frame(out0)[rep(1,length(ddd)),]
    out0[,dd.name] <- ddd
    out0[,nn_moms] <- out0[,nn_moms]*out0[,dd.name]^(n_idf)
  } else {

    out0['n_idf'] <- n_idf
    out0['p_idf'] <- p_idf
    out0['n_ddf'] <- n_ddf
    out0['p_ddf'] <- p_ddf
  }

  attr(out0,"lmom") <- lmom
  attr(out0,"nn_moms") <- nn_moms
  attr(out0,"fit") <- fit
  attr(out0,"gg") <- gg
  attr(out0,"n_idf") <- n_idf
  attr(out0,"p_idf") <- p_idf
  attr(out0,"n_ddf") <- n_ddf
  attr(out0,"p_ddf") <- p_ddf
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