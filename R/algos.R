
#### FOR EACH ALGORITHM, IT'S MANDATORY TO CREATE APPROPRIATE NAMES FOR PREDICTIONS
#### IE  "pred.name_of_algo.mean" AND SO ON

### implementation of facebook's prophet

#' Make a prediction with prophet algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe for "next year" with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.prophet(my.ts)

my.prophet <- function(prepedTS,n_pred)
{
  mod.prophet <- prepedTS$obj.df %>%
    dplyr::select(ds=dates,y=val) %>%
    prophet::prophet(weekly.seasonality = F,daily.seasonality = F,yearly.seasonality = T)
  prev.prophet <- prophet::make_future_dataframe(mod.prophet,periods = n_pred,
                                                 freq = prepedTS$freq.alpha) %>%
    predict(mod.prophet,.) %>%
    dplyr::mutate(dates=lubridate::as_date(ds)) %>%
    dplyr::select(dates,prev.prophet.mean=yhat,prev.prophet.inf=yhat_lower,prev.prophet.sup=yhat_upper) %>%
    dplyr::filter(dates>max(prepedTS$obj.df$dates))
  return(prev.prophet)
}

### implementation of SARIMA

#' Make a prediction with SARIMA algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.sarima(my.ts)
#'
my.sarima <- function(prepedTS,n_pred)
{
  prev.arima <- forecast::auto.arima(prepedTS$obj.ts,seasonal = T,D=1) %>%
    forecast::forecast(h=n_pred)
  dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  prev.arima <- data.frame(dates=lubridate::as_date(dates),prev.sarima.mean=as.numeric(prev.arima$mean),
                           prev.sarima.inf=as.numeric(prev.arima$lower[,2]),
                           prev.sarima.sup=as.numeric(prev.arima$upper[,2]))
  return(prev.arima)
}

### implementation of ets estimator => exponential smoothing

#' Make a prediction with ETS algorithm for one year after last oberved point
#' TBATS differ from BATS in the way it models the seasonality
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.ets(my.ts)
#'

my.ets <- function(prepedTS,n_pred)
{
  prev.ets <- forecast::ets(prepedTS$obj.ts) %>%
    forecast::forecast(h=n_pred)
  dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  prev.ets <- data.frame(dates=lubridate::as_date(dates),prev.ets.mean=as.numeric(prev.ets$mean),
                         prev.ets.inf=as.numeric(prev.ets$lower[,2]),prev.ets.sup=as.numeric(prev.ets$upper[,2]))
  return(prev.ets)
}

#' Make a prediction with TBATS algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.tbats(my.ts)
#'

my.tbats <- function(prepedTS,n_pred)
{
  prev.tbats <- forecast::tbats(prepedTS$obj.ts) %>%
    forecast::forecast(h=n_pred)
  dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  prev.tbats <- data.frame(dates=lubridate::as_date(dates),prev.tbats.mean=as.numeric(prev.tbats$mean),
                           prev.tbats.inf=as.numeric(prev.tbats$lower[,2]),prev.tbats.sup=as.numeric(prev.tbats$upper[,2]))
  return(prev.tbats)
}

#' Make a prediction with BATS algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.bats(my.ts)
#'

my.bats <- function(prepedTS,n_pred)
{
    prev.bats <- forecast::bats(prepedTS$obj.ts,seasonal.periods = prepedTS$freq.num) %>%
      forecast::forecast(h=n_pred)

  dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  prev.bats <- data.frame(dates=lubridate::as_date(dates),prev.bats.mean=as.numeric(prev.bats$mean),
                          prev.bats.inf=as.numeric(prev.bats$lower[,2]),prev.bats.sup=as.numeric(prev.bats$upper[,2]))
  return(prev.bats)
}

#' Make a prediction with STLM algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.stlm(my.ts)
#'

my.stlm <- function(prepedTS,n_pred)
{
  prev.stlm <- forecast::stlm(prepedTS$obj.ts) %>%
    forecast::forecast(h=n_pred)
  dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  prev.stlm <- data.frame(dates=lubridate::as_date(dates),prev.stlm.mean=as.numeric(prev.stlm$mean),
                          prev.stlm.inf=as.numeric(prev.stlm$lower[,2]),prev.stlm.sup=as.numeric(prev.stlm$upper[,2]))
  return(prev.stlm)
}


#' Make a prediction only upon previous year data. The prediction is built with last known smoothed evolution, previous year's
#' seasonal components and the last yearly cumulated value. No confidence interval is provided with this method
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series).
#' Note that this algorithm cannot predict further than one year
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.shortterm(my.ts)

my.shortterm <- function(prepedTS,n_pred,smooth_window=2)
{
  season_comp <- round(min(prepedTS$freq.num))
  dat <- prepedTS$obj.df %>%
    dplyr::mutate(fake_year = floor((dplyr::row_number())/min(prepedTS$freq.num))) ## create variable for aggregation for specific periodicity
  ## Compute "season" component
  agg_years <-dplyr::group_by(dat,fake_year) %>%
    dplyr::summarise(usage_year=sum(val))

  year_cumulate <- dplyr::mutate(dat,cum_year = RcppRoll::roll_sumr(val,season_comp))
  last_year <- year_cumulate$cum_year[nrow(year_cumulate)]

  season <- dplyr::left_join(dat,agg_years,by="fake_year") %>%
    dplyr::mutate(season=val/usage_year,
                  season = ifelse( !(is.na(season) | is.nan(season)| is.infinite(season)),season,0)) %>%
    dplyr::select(dates,val,season)

  evols <- season %>%
    dplyr::mutate(cum_year = RcppRoll::roll_sumr(val,season_comp,na.rm=T),
                  evol = RcppRoll::roll_sumr(val,smooth_window,na.rm=T) / dplyr::lag(RcppRoll::roll_sumr(val,smooth_window,na.rm=T),season_comp),
                  evol = ifelse( !(is.na(evol) | is.nan(evol) | is.infinite(evol)),evol,0) ) %>%
    dplyr::filter(dplyr::row_number()==dplyr::n()) %>%
    dplyr::select(-dates,-season,-val)

  calc <- season %>%
    dplyr::filter(dates>max(dates)-lubridate::years(1)) %>%
    base::cbind(evols) %>%
    dplyr::mutate(prev.shortterm.mean = last_year*evol*season,prev.shortterm.inf=NA,prev.shortterm.sup=NA,
                  dates = dates+lubridate::years(1)) %>%
    dplyr::select(dates,prev.shortterm.mean,prev.shortterm.inf,prev.shortterm.sup)

  calc <- calc[1:floor(n_pred),]
  calc$dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  return(calc)
}
