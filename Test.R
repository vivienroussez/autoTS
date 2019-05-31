dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"week")
values <- 1:length(dates)/10 + rnorm(length(dates))
## Find best algo
which.model <- getBestModel(dates,values,freq = "week",algos = list(my.sarima))
## Implement it on full sample
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos =list(get(which.model)))

dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"week")
values <- 1:length(dates)/10 + rnorm(length(dates))
tt <- autoTS::prepare.ts(dates,values,freq="week")

toto <- getBestModel(dates,values,freq = "week")
plotly::ggplotly(toto$graph.train)

my.stlm(prepedTS)
