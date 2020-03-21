library(autoTS)
library(magrittr)

dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
# prepare.ts(dates,values,"day") %>% my.prophet(n_pred=12)

getBestModel(dates,values,freq = "month",n_test = 6) %>%
  my.predictions()

##########################
#### Monthly data ########
##########################

## Find best algo
which.model <- getBestModel(dates,values,freq = "month",n_test = 6)
## implement best algo
my.predictions(which.model) %>% View()
## Implement all algos anyway
my.predictions(prepedTS = which.model$prepedTS) %>% View()

## standalone usage
prepare.ts(dates,values,"month") %>%
  my.predictions(prepedTS = .,algos = list("my.prophet","my.ets"))

## force model to be bagged
which.model <- getBestModel(dates,values,freq = "month",n_test = 6)
which.model$best <- "my.bagged"
my.predictions(which.model) %>% View()

## force model to be anything else
which.model <- getBestModel(dates,values,freq = "month",n_test = 6)
which.model$best <- "my.prophet"
my.predictions(which.model) %>% View()

## custom set of algos for bagged
which.model <- getBestModel(dates,values,freq = "month",n_test = 6,algos = list("my.prophet","my.ets"),
                            bagged = "custom",metric.error = my.mae)
which.model$best <- "my.bagged"
my.predictions(which.model) %>% View()

#################################
### quarterly data ##############
#################################

dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"quarter")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
## Find best algo
which.model <- getBestModel(dates,values,freq = "quarter",n_test = 4)
## implement best algo
my.predictions(which.model) %>% View()

## Implement all algos anyway
my.predictions(prepedTS = which.model$prepedTS) %>% View()

## force model to be bagged
which.model <- getBestModel(dates,values,freq = "quarter",n_test = 4)
which.model$best <- "my.bagged"
my.predictions(which.model) %>% View()

## force model to be anything else
which.model <- getBestModel(dates,values,freq = "month",n_test = 6)
which.model$best <- "my.prophet"
my.predictions(which.model) %>% View


#################################
### daily data    ##############
#################################

dates <- seq(lubridate::as_date("2010-06-01"),lubridate::as_date("2010-12-31"),"day")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
## Find best algo
which.model <- getBestModel(dates,values,freq = "day",n_test = 4)
## implement best algo
my.predictions(which.model) %>% View()

## Implement all algos anyway
my.predictions(prepedTS = which.model$prepedTS) %>% View()

## force model to be bagged
which.model <- getBestModel(dates,values,freq = "week",n_test = 4)
which.model$best <- "my.bagged"
my.predictions(which.model) %>% View()
