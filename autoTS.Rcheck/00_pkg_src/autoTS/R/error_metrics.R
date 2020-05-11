#' Custom (internal) function for RMSE
#'
#' @param true num vector of actual values
#' @param predicted num vector of predicted values
#' @return Num value with RMSE
#' @export

my.rmse <- function(true,predicted){
  return(sqrt(mean((predicted-true)^2,na.rm=T)))
}

#' Custom (internal) function for MAE
#'
#' @param true num vector of actual values
#' @param predicted num vector of predicted values
#' @return Num value with MAE
#' @export
#'
my.mae <-  function(true,predicted){
  return(mean(abs(predicted-true),na.rm=T))
}
