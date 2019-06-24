library(autoTS)
library(magrittr)

dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 1:length(dates)/10 + rnorm(length(dates))
## Find best algo
which.model <- getBestModel(dates,values,freq = "month")
## Implement it on full sample
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos = list(which.model$best))

dates <- seq(lubridate::as_date("2007-01-01"),lubridate::as_date("2010-12-31"),"week")
values <- 1:length(dates)/100 + rnorm(length(dates))
toto <- prepare.ts(dates,values,"week")
implement <- getBestModel(dates,values,freq = "week",bagged = T)
res <- autoTS::prepare.ts(dates,values,freq="week") %>%
  my.predictions("my.bagged")
