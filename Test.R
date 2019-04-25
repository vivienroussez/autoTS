library(dplyr)
# library(magrittr)
library(prophet)
# library(lubridate)

dat <- read.csv("~/Documents/A1/A1_roaming/R/Extract_data.csv")
ts <- filter(dat,RoamerType=="C" & RoamingPartner=="20801" & CallType=="V " & CallDirection=="A") %>%
  mutate(date=paste(substr(UsageMonth,1,4),"-",substr(UsageMonth,5,6),"-","01",sep="")) %>%
  select(date,usage=Usage) %>%
  filter(!(date %in% c("2018-11-01","2016-06-01")))

my.ts <- prepare.ts(dates = ts$date,values = ts$usage,freq = "month")

my.prophet(my.ts)
my.sarima(my.ts)
my.ets(my.ts)

toto <- my.predictions(my.ts)

implement <- getBestModel(dates,values,freq = "month")
res <- prepare.ts(dates = ts$date,values = ts$usage,freq = "month") %>%
  my.predictions(algos =list(get(implement)))

res <- prepare.ts(dates = ts$date,values = ts$usage,freq = "month") %>%
  my.predictions(algos =list(my.ets,my.prophet))

implement <- getBestModel(ts$date,ts$usage,freq = "month")
implement <- getBestModel(dates,values ,freq = "month")



#### For doc
library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"quarter")
values <- rnorm(length(dates))
complete.ts(dates,values,"month",complete = 0)

library(lubridate)
library(dplyr)
library(ggplot2)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"quarter")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"month",complete = 0)
plot(my.ts$obj.ts)
ggplot(my.ts$obj.df,aes(dates,val)) + geom_line()


library(lubridate)
library(dplyr)
library(ggplot2)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"quarter")
values <- rnorm(length(dates))
res <- prepare.ts(dates,values,"month",complete = 0) %>%
  my.predictions(algos =list(my.ets,my.prophet))
filter(res,type %in% c(NA,"mean")) %>%
  select(-type) %>%
  tidyr::gather(key="algo",value=val,-dates) %>%
  ggplot(aes(dates,val,color=algo)) + geom_line()

which.model <- getBestModel(dates,values,freq = "month",algos = list(my.sarima,my.ets))
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos =list(get(which.model)))

