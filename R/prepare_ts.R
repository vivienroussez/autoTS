# library(dplyr)
# library(tidyr)
# library(lubridate)

#' Determines the decimal frequency of a time series from
#'
#' @param freq.alpha A character string that indicates the frequency of the time series ("week", "month", "quarter", "day").
#' @return The decimal version of the frequency (useful for the forecast package functions).
#' @importFrom magrittr %>%
#' @examples getFrequency("week")
#' @export

getFrequency <- function(freq.alpha) ## get numerical frequency from alphanumerical
{
  if (freq.alpha=="month") {
    ff=12} else if(freq.alpha=="day") {
      ff=c(365.25,7) } else if(freq.alpha=="week") { # 2 seasonalities for daily series (weekly, yearly)
        ff=365.25/7 } else if(freq.alpha=="quarter") {
          ff=4} else stop("freq.alpha not implemented")
  return(ff)
}

#' Creates additional dates and values when NA where removed and the TS is not complete
#'
#' @param dates A vector of dates that can be parsed by lubridate
#' @param values A vector of same size as \code{dates}
#' @param freq A chacracter string that indicates the frequency of the time series ("week", "month", "quarter", "day").
#' @param complete A numerical value (or NA) to fill the missing data points
#' @return A dataframe with 2 columns : date and val, with additional rows
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' complete.ts(dates,values,"month",complete = 0)

complete.ts <- function(dates,values,freq,complete=0) ### creates explicit 0 or NA values for dates not appearing
{
  if (length(dates) != length(values)) stop("Dates and values' lengths differ")
  else{
    # gather in DF
    df <- data.frame(dates=lubridate::as_date(dates),val=values)
    # Complete the holes with zeros (or NA...)
    dd <- seq(min(df$dates),max(df$dates),by=freq) %>%
      data.frame(dates=.) %>%
      dplyr::left_join(df,by="dates") %>%
      dplyr::mutate(val=tidyr::replace_na(val,0))
    return(dd)
  }
}

#' Creates a list with the time series in a dataframe and a ts object, and the frequency stored
#' in decimal and litteral values. The result is meant to be put in the prophet or forecast functions
#'
#' @param dates A vector of dates that can be parsed by lubridate
#' @param values A vector of same size as \code{dates}
#' @param freq A chacracter string that indicates the frequency of the time series ("week", "month", "quarter", "day").
#' @param complete A numerical value (or NA) to fill the missing data points
#' @return A list containing : a dataframe, a ts vector for the time series, and 2 scalars for its frequency
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' library(ggplot2)
#' dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"quarter")
#' values <- rnorm(length(dates))
#' my.ts <- prepare.ts(dates,values,"month",complete = 0)
#' plot(my.ts$obj.ts)
#' ggplot(my.ts$obj.df,aes(dates,val)) + geom_line()

prepare.ts <- function(dates,values,freq,complete=0) ### prepare object ready to use by all algorithms
{
  # get a complete dataframe
  dd <- complete.ts(dates,values,freq,complete=0)

  # create a ts object for the forecast package out of this completed TS
  # if daily time series : for short ones (less than 2 years), frequency is set to 7 (ignoring yearly seasonality)
  # if greater, ts object is created with 2 seasonalities
  ff <- getFrequency(freq)
  if (freq=="day" & length(dd$val)/365 <2)  ff <- 7
  ts <- forecast::msts(dd$val,start=lubridate::decimal_date(min(dd$date)),seasonal.periods = ff,ts.frequency = max(ff))
  return(list(obj.ts=ts,obj.df=dd,freq.num=ff,freq.alpha=freq))
}


