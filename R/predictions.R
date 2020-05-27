
#' Make predictions with selected algorithms
#'
#' @description Fit selected algorithms, make the predictions and combine
#' the results along with observed data in one final dataframe.
#'
#' @param bestmod A list produced by the \code{getBestModel()} function (optional if \code{prepredTS} is provided)
#' @param prepedTS A list created by the \code{prepare.ts()} function (optional if \code{bestmod} provided)
#' @param algos A list containing the algorithms to be implemented. If \code{bestmod} is supplied, this value is ignored, and taken from the best model object
#' Using this option will overwrite the provided list of algorithms to implement them all
#' @param n_pred Int number of periods to forecast forward (eg n_pred = 12 will lead to one year of prediction for monthly time series)
#' @return A dataframe containing : date, actual observed values, one column per used algorithm, and
#' a column indicating the type of measure (mean prediction, upper or lower bound of CI)
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @examples
#' library(lubridate)
#' library(dplyr)
#' dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"quarter")
#' values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
#' ### Stand alone usage
#' prepare.ts(dates,values,"quarter") %>%
#'   my.predictions(prepedTS = .,algos = list("my.prophet","my.ets"))
#' ### Standard input with bestmodel
#' \donttest{
#' getBestModel(dates,values,freq = "quarter",n_test = 6) %>%
#'   my.predictions()
#'}
#'
my.predictions <- function(bestmod=NULL,prepedTS=NULL,
                           algos=list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm"),
                           n_pred=NA)
{
  only_bagged <- length(grep("bagged",c(algos,bestmod$best)) )>0
  if (is.null(bestmod) & algos[[1]]=="my.bagged") algos <- list("my.prophet","my.ets", "my.sarima",
                                                                "my.tbats","my.bats","my.stlm","my.shortterm")
  if (is.null(bestmod) & is.null(prepedTS)) stop("I need a prepared TS or a best model object !")
  if (!is.null(bestmod)){
    if (bestmod$best=="my.bagged") algos <- bestmod$algos else algos <-bestmod$best
    prepedTS <- bestmod$prepedTS
    if (is.na(n_pred)) n_pred <- round(bestmod$prepedTS$freq.num[1])
  }
  if (is.na(n_pred) & is.null(bestmod)) n_pred <- round(prepedTS$freq.num[1])

  ### Test frequency for ets (doesn't run for freq higher than 24...)
  where_ets <- grep("ets",algos)
  if (prepedTS$freq.num[1]>24 & !rlang::is_empty(where_ets)) {
    warning("Frequency too high to implement ETS ; skipping this algorithm")
    algos <- algos[-where_ets]
  }

  # Removing short term for predictions further than 1 year
  where_short <- grep("shortterm",algos)
  if (n_pred>prepedTS$freq.num[1] & !rlang::is_empty(where_short)) {
    warning("Predictions too far for short term algorithm, which has been skipped")
    algos <- algos[-where_short]
  }

  algos_apply <- lapply(algos,get)

  res <- lapply(algos_apply,function(xx) xx(prepedTS,n_pred)) %>%
    dplyr::bind_cols(.data) %>%
    dplyr::select(.data$dates, dplyr::starts_with("prev")) %>%
    tidyr::gather(key="var",value = "val",-.data$dates) %>%
    tidyr::separate(.data$var,into = c("pred","model","type"),sep="\\.") %>%
    dplyr::group_by(.data$dates,.data$type) %>%
    tidyr::spread(key = .data$model,value = .data$val) %>%
    dplyr::full_join(prepedTS$obj.df,by="dates") %>%
    dplyr::arrange(.data$dates,.data$type) %>%
    dplyr::rename(actual.value=.data$val) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$pred)
  ### Compute simple mean of elected algorithms for bagged estimator
  res$bagged <- dplyr::select(res,-.data$dates,-.data$type,-.data$actual.value) %>%
    apply(MARGIN = 1,mean)

  if (only_bagged){
    keeps <- "bagged"
  } else{
    if (length(algos)>1) {
      keeps <- c(stringr::str_remove_all(unlist(algos),"my."),"bagged")
    } else keeps <- stringr::str_remove_all(unlist(algos),"my.")
  }

  res <- dplyr::select_at(res,c("dates","type","actual.value",keeps))
  return(res)
}

#' Determine best algorithm
#'
#' @description Implement selected algorithms, train them without the last n observed data points
#' (or n_test number of points), and compares the results to reality to determine the best algorithm
#'
#' @param dates A vector of dates that can be parsed by lubridate
#' @param values A vector of same size as \code{dates}
#' @param freq A chacracter string that indicates the frequency of the time series ("week", "month", "quarter", "day").
#' @param complete A numerical value (or NA) to fill the missing data points
#' @param n_test number of data points to keep aside for the test (default : one year)
#' @param algos A list containing the algorithms (strings, with prefix "my.") to be tested
#' @param bagged A string. "auto" will use all available algoriths, skipping algos parameter. Else, specified algos  of the `algo` parameter will be used
#' @param graph A boolean, if TRUE, comparison of algorithms is plotted
#' @param metric.error a function to compute the error the each models. available functions : my.rmse and my.mae
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats predict
#' @return A list contraining a character string with the name of the best method,
#' a gg object with the comparison between algorithms and a dataframe with predictions of all tried algorithms,
#' a dtaframe containing the errors of each algorithms, the preparedTS object and the list of algorithms tested
#' @examples
#' library(autoTS)
#' dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"quarter")
#' values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
#' \donttest{
#' which.model <- getBestModel(dates,values,freq = "quarter",n_test = 4)
#' }
#' ### Custom set of algorithm (including for bagged estimator)
#' which.model <- getBestModel(dates,values,freq = "quarter",n_test = 4,
#'                             algos = list("my.prophet","my.ets"),bagged = "custom")
#' ### Use MAE instead of RMSE
#' which.model <- getBestModel(dates,values,freq = "quarter",n_test = 3,
#'                             algos = list("my.prophet","my.ets"),
#'                             bagged = "custom",metric.error = my.mae)
#'


getBestModel <- function(dates,values,
                         freq,complete=0,n_test=NA,graph=TRUE,
                         algos=list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm"),
                         bagged="auto",
                         metric.error = my.rmse)
{
  . <- NULL
  freq.num <- getFrequency(freq)
  if (is.na(n_test)) n_test <- freq.num[1] # by default, test over the last "seasonal period"

  ## if auto bagged model, all algos are implemented. Else, only specified algos are implemented (and used for bagged estimator)
  if (bagged=="auto") algos <- list("my.prophet","my.ets", "my.sarima","my.tbats","my.bats","my.stlm","my.shortterm")

  df <- complete.ts(dates,values,freq,complete=0)

  ## filter out the last year and create proper object for algos
  fin <- max(df$dates[1:(length(df$dates)-n_test)])
  df_filter <- dplyr::filter(df,dates<=fin)

  full.TS <- prepare.ts(df$dates,df$val,freq,complete)
  filtered.TS <- prepare.ts(df_filter$dates,df_filter$val,freq,complete)
  train <- my.predictions(prepedTS = filtered.TS,algos = algos,n_pred = n_test) %>%
    dplyr::select(-.data$actual.value) %>%
    dplyr::full_join(df,by="dates") %>%
    dplyr::rename(actual.value=.data$val)

  errors <- dplyr::filter(train,.data$type=="mean") %>%
    dplyr::summarise_if(is.numeric,list(~metric.error(.,.data$actual.value))) %>%
    dplyr::select(-.data$actual.value)

  best <- names(errors)[apply(errors,which.min,MARGIN = 1)] %>% paste("my",.,sep=".")

  ### Graph comparing actual values & algo predictions
  ddd <- dplyr::filter(train,.data$type %in% c(NA,"mean")) %>%
    dplyr::select(-.data$type) %>%
    tidyr::gather(key="algo",value="val",-.data$dates)
  gg <- ggplot2::ggplot(ddd,ggplot2::aes(.data$dates,.data$val,color=.data$algo)) +
    ggplot2::geom_line() + ggplot2::theme_minimal()
  if (graph==TRUE)
  {
    print(gg)
  }
  return(list(prepedTS=full.TS,best=best,train.errors=errors,res.train=train,algos=algos,graph.train=gg))
}
