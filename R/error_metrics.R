#' Custom (internal) function for RMSE (not found in basic packages)
#'
#' @param true num vector of actual values
#' @param predicted num vector of predicted values
#' @return Num value with RMSE

my.rmse <- function(true,predicted){
  return(sqrt(mean((predicted-true)^2,na.rm=T)))
}

#' Custom (internal) function for MAE (not found in basic packages)
#'
#' @param true num vector of actual values
#' @param predicted num vector of predicted values
#' @return Num value with MAE
#'
my.mae <-  function(true,predicted){
  return(mean(abs(predicted-true),na.rm=T))
}
