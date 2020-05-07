
#### FOR EACH ALGORITHM, IT'S MANDATORY TO CREATE APPROPRIATE NAMES FOR PREDICTIONS
#### IE  "pred.name_of_algo.mean" AND SO ON

### implementation of facebook's prophet

#' Fit prophet algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe for "next year" with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.prophet(my.ts,n_pred=12)

my.prophet <- function(prepedTS,n_pred)
{
  . <- NULL
  if (prepedTS$freq.alpha=="day"){ws=T;ds=F;ys=T} else{ws=F;ds=F;ys=T}
  mod.prophet <- prepedTS$obj.df %>%
    dplyr::select(ds=.data$dates,y=.data$val) %>%
    prophet::prophet(weekly.seasonality = ws,daily.seasonality = ds,yearly.seasonality = ys)
  prev.prophet <- prophet::make_future_dataframe(mod.prophet,periods = n_pred,
                                                 freq = prepedTS$freq.alpha) %>%
    predict(mod.prophet,.) %>%
    dplyr::mutate(dates=lubridate::as_date(.data$ds)) %>%
    dplyr::select(.data$dates,prev.prophet.mean=.data$yhat,prev.prophet.inf=.data$yhat_lower,
                  prev.prophet.sup=.data$yhat_upper) %>%
    dplyr::filter(.data$dates>max(prepedTS$obj.df$dates))
  return(prev.prophet)
}

### implementation of SARIMA

#' Fit SARIMA algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#'  library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' \dontrun{
#' my.sarima(my.ts,n_pred=12)
#' }
#'
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

#' Fit ETS algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#'  library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.ets(my.ts,n_pred=12)
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

#' Fit TBATS algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.tbats(my.ts,n_pred=12)
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

#' Fit BATS algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.bats(my.ts,n_pred=12)
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

#' Fit STLM algorithm and make the prediction
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.stlm(my.ts,n_pred=12)
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


#' Fit short term algorithm and make the prediction
#'
#' @details this algorithm uses data of the last year and makes the prediction
#' taking into account the seasonality and the evolution of the previous periods' evolution
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series).
#' Note that this algorithm cannot predict further than one year
#' @param smooth_window Int specifying the number of periods to consider for computing the evolution rate that will be applied for the forecast
#' @return A dataframe with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"week",complete = 0)
#' my.shortterm(my.ts,n_pred=12)

my.shortterm <- function(prepedTS,n_pred,smooth_window=2)
{
  season_comp <- round(min(prepedTS$freq.num))
  adjust_fake <- season_comp-nrow(prepedTS$obj.df)%%season_comp-1

  dat <- prepedTS$obj.df %>%
    dplyr::mutate(fake_year =  (dplyr::row_number()+adjust_fake)%/%season_comp) ## create variable for aggregation for specific periodicity  ## Compute "season" component
  agg_years <-dplyr::group_by(dat,.data$fake_year) %>%
    dplyr::summarise(usage_year=sum(.data$val))

  year_cumulate <- dplyr::mutate(dat,cum_year = RcppRoll::roll_sumr(.data$val,season_comp))
  last_year <- year_cumulate$cum_year[nrow(year_cumulate)]

  season <- dplyr::left_join(dat,agg_years,by="fake_year") %>%
    dplyr::mutate(season=.data$val/.data$usage_year,
                  season = ifelse( !(is.na(season) | is.nan(season)| is.infinite(season)),season,0)) %>%
    dplyr::select(.data$dates,.data$val,.data$season)

  evols <- season %>%
    dplyr::mutate(cum_year = RcppRoll::roll_sumr(.data$val,season_comp,na.rm=T),
                  evol = RcppRoll::roll_sumr(.data$val,smooth_window,na.rm=T) /
                    dplyr::lag(RcppRoll::roll_sumr(.data$val,smooth_window,na.rm=T),season_comp),
                  evol = ifelse( !(is.na(.data$evol) | is.nan(.data$evol) | is.infinite(.data$evol)),.data$evol,0) ) %>%
    dplyr::filter(dplyr::row_number()==dplyr::n()) %>%
    dplyr::select(-.data$dates,-.data$season,-.data$val)

  calc <- season %>%
    dplyr::filter(.data$dates>max(.data$dates)-lubridate::years(1)) %>%
    base::cbind(evols) %>%
    dplyr::mutate(prev.shortterm.mean = last_year*.data$evol*.data$season,prev.shortterm.inf=NA,prev.shortterm.sup=NA,
                  dates = .data$dates+lubridate::years(1)) %>%
    dplyr::select(.data$dates,.data$prev.shortterm.mean,.data$prev.shortterm.inf,.data$prev.shortterm.sup)

  calc <- calc[1:floor(n_pred),]
  calc$dates <- seq(max(prepedTS$obj.df$dates),by=prepedTS$freq.alpha,length.out = n_pred+1)[-1]
  return(calc)
}
