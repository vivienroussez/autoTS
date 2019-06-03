
#' Make predictions with desired algorithms, combine them with observed data and gather everything
#' in one final dataframe. If you want to save time, skip the sarima algorithm which is much slower
#'
#' @param prepedTS A list created by the \code{prepare.ts()} function
#' @param algos A list containing the algorithms to be implemented
#' @param bagged A boolean indicating weather a "bagged" estimator (simple average of all algorithm) should be computed.
#' Using this option will overwrite the provided list of algorithms to implement them all
#' @return A dataframe containing : date, actual observed values, one column per used algorithm, and
#' a column indicating the type of measure (mean prediction, upper or lower bound of CI)
#' @export
#' @importFrom magrittr %>%
#' @example library(lubridate)
#' library(dplyr)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' prepare.ts(dates = dates,values = values,freq = "month") %>%
#'     my.predictions(algos=list("my.ets","my.prophet"))
#'
my.predictions <- function(prepedTS,
                           algos=list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm"),
                           bagged=F)
{

  if (!is_empty(grep("my.bagged",algos))) bagged <- T ## Check if best is bagged algo

  ### Test frequency for ets (doesn't run for freq higher than 24...)
  where_ets <- grep("ets",algos)
  if (prepedTS$freq.num>24 & !is_empty(where_ets)) {
    warning("Frequency too high to implement ETS ; skipping this algo")
    algos <- algos[-where_ets]
  }

  ### Implementing all algorithms if bagged estimator is requested
  if (bagged==T) algos <- list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm")

  algos <- lapply(algos,get)

  res <- lapply(algos,function(xx) xx(prepedTS)) %>%
    dplyr::bind_cols() %>%
    dplyr::select(dates, dplyr::starts_with("prev")) %>%
    tidyr::gather(key="var",value = "val",-dates) %>%
    tidyr::separate(var,into = c("pred","model","type"),sep="\\.") %>%
    dplyr::group_by(dates,type) %>%
    tidyr::spread(key = model,value = val) %>%
    dplyr::full_join(prepedTS$obj.df,by="dates") %>%
    dplyr::arrange(dates,type) %>%
    dplyr::rename(actual.value=val) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pred)

  if (bagged==T) ### Compute simple mean for bagged estimator
  {
    res$bagged <- dplyr::select(res,-dates,-type,-actual.value) %>%
      apply(MARGIN = 1,mean)
    res <-  dplyr::select(res,dates,type,actual.value,bagged)
  }
  return(res)
}

#' Implement selected algorithms of the package, train them without the last observed year
#' (or n_test number of points), make a prediction, and returns the best performing algorithm on this period.
#' If you want to save time, skip the SARIMA method.
#'
#' @param dates A vector of dates that can be parsed by lubridate
#' @param values A vector of same size as \code{dates}
#' @param freq A chacracter string that indicates the frequency of the time series ("week", "month", "quarter", "day").
#' @param complete A numerical value (or NA) to fill the missing data points
#' @param n_test number of data points to keep aside for the test (default : one year)
#' @param algos A list containing the algorithms to test
#' @param bagged A boolean indicating weather a "bagged" estimator (simple average of all algorithm) should be computed.
#' Using this option will overwrite the list of algorithms to implement them all
#' @param graph A boolean, if TRUE, comparison of algorithms is plotted
#' @export
#' @importFrom magrittr %>%
#' @return A list contraining a character string with the name of the best method,
#' a gg object with the comparison between algorithms and a dataframe with predictions of all tried algorithms
#' @example library(lubridate)
#' library(dplyr)
#' library(ggplot2)
#' dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
#' values <- rnorm(length(dates))
#' implement <- getBestModel(dates,values,freq = "month")
#' res <- prepare.ts(dates,values,freq = "month") %>%
#'   my.predictions(algos =list(implement$best))


getBestModel <- function(dates,values,
                         freq,complete=0,n_test=NA,graph=T,
                         algos=list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm"),
                         bagged=T)
{
  freq.num <- getFrequency(freq)

  if (is.na(n_test)) n_test <- freq.num # by default, test over the last observed year

  df <- complete.ts(dates,values,freq,complete=0)

  ## filter out the last year and create proper object for algos
  fin <- df$dates[1:(length(df$dates)-n_test)] %>% max()
  df_filter <- dplyr::filter(df,dates<=fin)

  prepedTS <- prepare.ts(df_filter$dates,df_filter$val,freq,complete)

  ### Test frequency for ets (doesn't run for freq higher than 24...)
  where_ets <- grep("ets",names(algos))
  if (prepedTS$freq.num>24 & !is_empty(where_ets)) {
    warning("Frequency too high to implement ETS ; skipping this algo")
    algos <- algos[-where_ets]
  }

  if (bagged==T) algos <- list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm")

  train <- my.predictions(prepedTS,algos) %>%
    dplyr::select(-actual.value) %>%
    dplyr::full_join(df,by="dates") %>%
    dplyr::rename(actual.value=val)
  ### Compute average for bagged model
  if (bagged==T)
  {
    train$bagged <- dplyr::select(train,-dates,-type,-actual.value) %>%
      apply(MARGIN = 1,mean)
  }

  errors <- dplyr::summarise_if(train,is.numeric,
                                function(xx) sqrt(mean((xx-train$actual.value)^2,na.rm = T))) %>%
    dplyr::select(-actual.value)

  best <- names(errors)[apply(errors,which.min,MARGIN = 1)] %>% paste("my",.,sep=".")

  ### Graph comparing actual values & algo predictions
  ddd <- dplyr::filter(train,type %in% c(NA,"mean")) %>%
    dplyr::select(-type) %>%
    tidyr::gather(key="algo",value=val,-dates)
  gg <- ggplot2::ggplot(ddd,ggplot2::aes(dates,val,color=algo)) + ggplot2::geom_line()
  if (graph==T)
  {
    print(gg)
  }

  return(list(best=best,graph.train=gg,res.train=train))
}
