library(autoTS)
library(magrittr)

dates <- seq(lubridate::as_date("2007-01-01"),lubridate::as_date("2010-12-31"),"day")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates))
# prepare.ts(dates,values,"day") %>% my.prophet(n_pred=12)

## Find best algo
which.model <- getBestModel(dates,values,freq = "month",n_test = 24)
## Implement it on full sample
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos = list(which.model$best),n_pred = 14)

dates <- seq(lubridate::as_date("2007-01-01"),lubridate::as_date("2010-12-31"),"week")
values <- 1:length(dates)/100 + rnorm(length(dates))
toto <- prepare.ts(dates,values,"week")
implement <- getBestModel(dates,values,freq = "week",bagged = T)
res <- autoTS::prepare.ts(dates,values,freq="week") %>%
  my.predictions("my.bagged")

prepedTS <- prepare.ts(dates,values,freq = "month")
my.predictions(prepedTS,n_pred = 14)
my.shortterm(prepedTS,n_pred = 14)


dates <- seq(lubridate::as_date("2010-06-01"),lubridate::as_date("2010-12-31"),"day")
values <- 10+ 1:length(dates)/100 + rnorm(length(dates))
toto <- prepare.ts(dates,values,"day")
toto <- getBestModel(dates,values,freq = "day",bagged = T,n_test = 15)

my.sarima(toto,14)
my.bats(toto,14)

forecast::auto.arima(toto$obj.ts,seasonal = T) %>% forecast::forecast(10) %>% plot()

prepedTS <- toto
n_pred <- 10
