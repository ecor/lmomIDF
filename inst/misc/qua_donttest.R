#' \donttest{
#' ## USING GSOD DATA
#' library(GSODR)
#' years <- 1937:2020
#'
#'
#' gsod <- get_GSOD(years=years,station="623180-99999")
#' ##ALEXANDRIA INTL EG 623180-99999
#' prec <- gsod %>% select(YEARMODA,PRCP,PRCP_ATTRIBUTES) %>% mutate(YEARMODA=as.Date(YEARMODA,format="%Y-%m-%d")) 
#'
#' # PRCP_ATTRIBUTES 
#' #   A = 1 report of 6-hour precipitation amount;
#' #   B = Summation of 2 reports of 6-hour precipitation amount;
#' #   C = Summation of 3 reports of 6-hour precipitation amount;
#' #   D = Summation of 4 reports of 6-hour precipitation amount;
#' #   E = 1 report of 12-hour precipitation amount;
#' #   F = Summation of 2 reports of 12-hour precipitation amount;
#' #   G = 1 report of 24-hour precipitation amount;
#' #   H = Station reported '0' as the amount for the day (e.g. from 6-hour reports), but also reported at least one occurrence of precipitation in hourly observations–this could indicate a trace occurred, but should be considered as incomplete data for the day;
#' #   I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations–it's still possible that precipitation occurred but was not reported;
#' dds <- range(prec$YEARMODA)
#' yymmdds <- seq(from=dds[1],to=dds[2],by="day")
#' prec <- data.table::data.table(YEARMODA=yymmdds) %>% full_join(prec)
#'
#' y <- annual.agg(x=prec$PRCP,dd=1:5,time=prec$YEARMODA)
#' ### y$aggr cannot be -Inf or +Inf
#' y$aggr[y$aggr==-Inf] <- NA
#' lmom <- annual.agg.samlmu(y)
#' lmrd(lmom)
#' z <- annual.agg.pel(distrib="gev",x=y,lmom=lmom)
#' out <- annual.agg.qua(f=c(1:49)/50,para=z)
#' }
