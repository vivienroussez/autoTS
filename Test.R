library(autoTS)
library(magrittr)

dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 1:length(dates)/10 + rnorm(length(dates))
## Find best algo
which.model <- getBestModel(dates,values,freq = "month")
## Implement it on full sample
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos = list(which.model$best))

dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 1:length(dates)/10 + rnorm(length(dates))
tt <- autoTS::prepare.ts(dates,values,freq="month")

toto <- getBestModel(dates,values,freq = "month",bagged = T)
plotly::ggplotly(toto$graph.train)

res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos = list("my.bagged"))

toto$res.train$bagged <- dplyr::select(toto$res.train,-dates,-type,-actual.value) %>%
  apply(MARGIN = 1,mean)

  my.stlm(prepedTS)


dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- rnorm(length(dates))
implement <- getBestModel(dates,values,freq = "month")
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos =list(implement$best))
