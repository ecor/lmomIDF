NULL
#'
#' Mirror of \code{\link{qua}}: probability distribution fitting with L-Moments
#'
#' @param f vector of probabilities/frequencies.
#' @param para object returned by \code{\link{annual.agg.pel}}.
#' @param aggr.name,dd.name column names. see \code{\link{annual.agg.samlmu}} or \code{\link{annual.agg.pel}}.
#' @param f.name column name for the probabilities/frequancies. Deafault is \code{"f"}.
#' @param use_ggplot logical. if it is \code{TRUE} (default) boxplots are created. See function result descriptions. 
#' @param xlab,ylab,fill,color_idf arguments for \code{ggplot2} functions. It is used if \code{use_ggplot==TRUE}. See \code{\link{geom_boxplot}},\code{\link{scale_fill_manual}},\code{\link{labs}}
#' @param nrun additional Monte-Carlo generations. It is used if \code{use_ggplot==TRUE}. 
#' @param n_idf exponemts (e.g. generally mamed \code{n}) of Intansity Diration Curve 
#' @param remove_distrib_from_boxplot logical. It is used if \code{use_ggplot==TRUE}.  Default see usage. If it \code{TRUE} distribution quantiles are removed from box plots.  
#' @param idf_curve logical. It is used if \code{use_ggplot==TRUE}.  Default see usage. If it \code{TRUE}. IDF (intensity-durantion-frequancy) and DDF (depth-durantion-frequancy) curves are added to the respective boxplots 


#' @param ... further arguments. 
#'
#'
#' @importFrom stats runif
#' @importFrom ggplot2 ggplot geom_boxplot aes theme_bw scale_fill_manual xlab ylab geom_line scale_color_manual
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lmomPi qua
#' 
#' @seealso \code{\link{annual.agg.idf.samlmu}}
#' 
#' @export
#' @examples 
#' 
#' library(RMAWGEN)
#'
#' 
#' data(trentino)
#' time <- as.Date(sprintf("%04d-%02d-%02d",PRECIPITATION$year,
#'                        PRECIPITATION$month,PRECIPITATION$day),format="%Y-%m-%d")
#' x <- PRECIPITATION$B8570
#'
#' y <- annual.agg(x,dd=1:5,time) 
#' lmom <- annual.agg.samlmu(y)
#' lmrd(lmom)
#' z <- annual.agg.pel(distrib="gpa",x=y,lmom=lmom)
#'
#' out <- annual.agg.qua(para=z)
#'
#' \donttest{
#' 
#' library(GSODR)
#' library(magrittr)
#' library(data.table)
#' library(dplyr)
#' 
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
#' lmom <- annual.agg.samlmu(y)
#' lmrd(lmom)
#' z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
#' out <- annual.agg.qua(f=c(1:49)/50,para=z)
#' }

annual.agg.qua <- function(f=(0:10)/10,para,f.name="f",aggr.name=NA,dd.name=NA,use_ggplot=TRUE,xlab="duration",ylab="value",fill=c("#ca0020","#0571b0","#bababa","#bababb","#bababc"),color_idf=brewer.pal(name="YlOrRd",n=length(f)), nrun=5,n_idf=NULL,remove_distrib_from_boxplot=FALSE,idf_curve=TRUE,...)
  
{
  
  
  out <- lapply(para,FUN=qua,f=f) %>% lapply(data.frame,f=f) %>% reshape2::melt(id="f") ##%>% reshape2::melt()
  ###
  ###
  
  ###
 ## out0 <- 
  ###
  ## EC 20230114
  if (length(color_idf)<length(f)) color_idf[length(color_idf):length(f)] <- color_idf[length(color_idf)]
  ## END EC 20230114
      
      
  if (is.null(dd.name)) dd.name <- NA
  if (is.null(aggr.name)) aggr.name <- NA
  if (is.null(n_idf)) n_idf <- NA
  ### 
  if (is.na(dd.name)) dd.name <- attr(para,"dd.name")
  if (is.na(aggr.name)) aggr.name <- attr(para,"aggr.name")
  if (is.na(n_idf)) n_idf <- attr(para,"n_idf")
  ###
  dd <- attr(para,"lmom")[,dd.name]
  names(dd) <- sprintf(attr(para,"dd_formatter"),dd)
  
  out[,dd.name] <- dd[out$L1]
  out <- out[c("f",dd.name,"value")]
  names(out) <- c(f.name,dd.name,aggr.name)
  
  
 ## out
  if (use_ggplot) {
    ####
    ####
    zzrl <- list()
    nyr <- nrow(attr(para,"x")[attr(para,"x")[,dd.name]==attr(para,"x")[1,dd.name],])
    
    if (nrun>0) for (i in 1:nrun) {
      fzz <- runif(nyr)
      zzr <- lapply(para,FUN=qua,f=fzz) %>% lapply(data.frame,f=fzz) %>% reshape2::melt(id="f") 
      ##dd <- attr(para,"lmom")[,dd.name]
      ##names(dd) <- sprintf(attr(para,"dd_formatter"),dd)
     
      zzr[,dd.name] <- dd[zzr$L1]
      zzr <- zzr[,c("f",dd.name,"value")]
      names(zzr) <- c(f.name,dd.name,aggr.name)
      zzr$group <- i
      zzrl <- rbind(zzr,zzrl)
      
    }
    
    
    nzeros <- ceiling(log10(i))
    nzeros[nzeros<=2] <- 2
    formatter <- paste0("RUN%0",nzeros,"d")
    zzrl$group <- sprintf(formatter,zzrl$group)
    
    
    zz <- attr(para,"x")
    zz <- zz[,c(dd.name,aggr.name)]
    zz$group <- "obs"
    if (!remove_distrib_from_boxplot) {
      zz1 <- out[,c(dd.name,aggr.name)]
      zz1$group <- "model"
     ###
      zz <- rbind(zz,zz1)
    } else {
      
      fill <- fill[-1]
      
    }
    zz <- rbind(zz,zzrl[,names(zz)])
    dd1 <- zz[,dd.name]
    aggr1 <- zz[,aggr.name]
    group1 <- zz$group
    fff <- fill[length(fill)]
    ngroups <- length(unique(group1))
    nfills <- length(fill)
    if (ngroups>=nfills) fill[nfills:ngroups] <- fill[nfills]
    fill <- fill[1:ngroups]
    gg <- ggplot()+geom_boxplot(aes(x=factor(dd1),y=aggr1,fill=group1),data=NULL)+theme_bw()
    gg <- gg+xlab(xlab)+ylab(ylab)+scale_fill_manual(values=fill,name="samples")
    if (idf_curve) {
      gg <- gg+geom_line(aes(x=out[,dd.name],y=out[,aggr.name],group=factor(out[,f.name]),col=factor(out[,f.name])))
      gg <- gg+scale_color_manual(values=color_idf,name="frequency")
      
    }
    ###
    attr(out,"boxplot") <- gg
    ###
    aggr1_depth <- aggr1*dd1
    gg_depth <- ggplot()+geom_boxplot(aes(x=factor(dd1),y=aggr1_depth,fill=group1),data=NULL)+theme_bw()
    gg_depth <- gg_depth+ylab(paste(ylab,"(depth)"))+xlab(xlab)+scale_fill_manual(values=fill,name="samples")
    if (idf_curve) {
      gg_depth <- gg_depth+geom_line(aes(x=out[,dd.name],y=out[,aggr.name]*out[,dd.name],group=factor(out[,f.name]),col=factor(out[,f.name])))
      gg_depth <- gg_depth+scale_color_manual(values=color_idf,name="frequency")
      
    }
    
    #####
    
    attr(out,"boxplot_depth") <- gg_depth
    #####
    
    i_idf <- which(group1=="obs")
    gg_idf <- ggplot()+geom_boxplot(aes(x=factor(dd1[i_idf]),y=aggr1[i_idf],fill=group1[i_idf]),data=NULL)+theme_bw()
    gg_idf <- gg_idf+xlab(xlab)+ylab(ylab)+scale_fill_manual(values=fill,name="samples") 
    gg_idf <- gg_idf+geom_line(aes(x=out[,dd.name],y=out[,aggr.name],group=factor(out[,f.name]),col=factor(out[,f.name])))
    gg_idf <- gg_idf+scale_color_manual(values=color_idf,name="frequency")
    attr(out,"idf") <- gg_idf
    
    gg_ddf <- ggplot()+geom_boxplot(aes(x=factor(dd1[i_idf]),y=aggr1[i_idf]*dd1[i_idf],fill=group1[i_idf]),data=NULL)+theme_bw()
    gg_ddf <- gg_ddf+ylab(paste(ylab,"(depth)"))+xlab(xlab)+scale_fill_manual(values=fill,name="samples") 
    gg_ddf <- gg_ddf+geom_line(aes(x=out[,dd.name],y=out[,aggr.name]*out[,dd.name],group=factor(out[,f.name]),col=factor(out[,f.name])))
    gg_ddf <- gg_ddf+scale_color_manual(values=color_idf,name="frequency")
    attr(out,"ddf") <- gg_ddf
    
    
    
    
  }
  return(out) 
}



### 'Yearly*" counterpart
NULL
#'
#'
#' @rdname annual.agg.qua
#' @export
#' 
#' 
yearly.agg.qua <- function(...) {
  
  out <- annual.agg.qua(...)
  return(out)
}












