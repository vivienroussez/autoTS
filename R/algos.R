
#### FOR EACH ALGORITHM, IT'S MANDATORY TO CREATE APPROPRIATE NAMES FOR PREDICTIONS
#### IE  "pred.name_of_algo.mean" AND SO ON

### implementation of facebook's prophet

#' Make a prediction with prophet algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @return A dataframe for "next year" with 4 columns : date, average prediction, upper and lower 95% confidence interval bounds
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' my.prophet(my.ts)

my.prophet <- function(prepedTS)
{
  mod.prophet <- prepedTS$obj.df %>%
    dplyr::select(ds=dates,y=val) %>%
    prophet::prophet(weekly.seasonality = F,daily.seasonality = F,yearly.seasonality = T)
  prev.prophet <- prophet::make_future_dataframe(mod.prophet,periods = prepedTS$freq.num,
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
my.sarima <- function(prepedTS)
{
  prev.arima <- forecast::auto.arima(prepedTS$obj.ts,seasonal = T,D = 1) %>%
    forecast::forecast(h=prepedTS$freq.num)
  dates <- time(prev.arima$mean) %>% as.numeric() %>%
    lubridate::date_decimal() %>% lubridate::round_date(prepedTS$freq.alpha)
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

my.ets <- function(prepedTS)
{
  prev.ets <- forecast::ets(prepedTS$obj.ts) %>%
    forecast::forecast(h=prepedTS$freq.num)
  dates <- time(prev.ets$mean) %>% as.numeric() %>%
    lubridate::date_decimal() %>% lubridate::round_date(prepedTS$freq.alpha)
  prev.ets <- data.frame(dates=lubridate::as_date(dates),prev.ets.mean=as.numeric(prev.ets$mean),
                         prev.ets.inf=as.numeric(prev.ets$lower[,2]),prev.ets.sup=as.numeric(prev.ets$upper[,2]))
  return(prev.ets)
}

#' Make a prediction with TBATS algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
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

my.tbats <- function(prepedTS)
{
  prev.tbats <- forecast::tbats(prepedTS$obj.ts) %>%
    forecast::forecast(h=prepedTS$freq.num)
  dates <- time(prev.tbats$mean) %>% as.numeric() %>%
    lubridate::date_decimal() %>% lubridate::round_date(prepedTS$freq.alpha)
  prev.tbats <- data.frame(dates=lubridate::as_date(dates),prev.tbats.mean=as.numeric(prev.tbats$mean),
                           prev.tbats.inf=as.numeric(prev.tbats$lower[,2]),prev.tbats.sup=as.numeric(prev.tbats$upper[,2]))
  return(prev.tbats)
}

#' Make a prediction with BATS algorithm for one year after last oberved point
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
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

my.bats <- function(prepedTS)
{
  prev.bats <- forecast::bats(prepedTS$obj.ts) %>%
    forecast::forecast(h=prepedTS$freq.num)
  dates <- time(prev.bats$mean) %>% as.numeric() %>%
    lubridate::date_decimal() %>% lubridate::round_date(prepedTS$freq.alpha)
  prev.bats <- data.frame(dates=lubridate::as_date(dates),prev.bats.mean=as.numeric(prev.bats$mean),
                          prev.bats.inf=as.numeric(prev.bats$lower[,2]),prev.bats.sup=as.numeric(prev.bats$upper[,2]))
  return(prev.bats)
}


