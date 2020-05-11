pkgname <- "autoTS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "autoTS-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('autoTS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("complete.ts")
### * complete.ts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: complete.ts
### Title: Creates additional dates and values when NA where removed and
###   the TS is not complete
### Aliases: complete.ts

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
values <- rnorm(length(dates))
complete.ts(dates,values,"month",complete = 0)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("complete.ts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getBestModel")
### * getBestModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getBestModel
### Title: Determine best algorithm
### Aliases: getBestModel

### ** Examples

library(lubridate)
library(dplyr)
library(ggplot2)
dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
## Not run: 
##D which.model <- getBestModel(dates,values,freq = "month",n_test = 9)
## End(Not run)
### Custom set of algorithm (including for bagged estimator)
## Not run: 
##D which.model <- getBestModel(dates,values,freq = "month",n_test = 6,
##D                             algos = list("my.prophet","my.ets"),bagged = "custom")
##D                             
## End(Not run)
### Use MAE instead of RMSE
## Not run: 
##D which.model <- getBestModel(dates,values,freq = "month",n_test = 6,
##D                             algos = list("my.prophet","my.ets"),
##D                             bagged = "custom",metric.error = my.mae)
##D                             
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getBestModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getFrequency")
### * getFrequency

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getFrequency
### Title: Determines the decimal frequency of a time series from a
###   character string
### Aliases: getFrequency

### ** Examples

getFrequency("week")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getFrequency", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.bats")
### * my.bats

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.bats
### Title: Fit BATS algorithm and make the prediction
### Aliases: my.bats

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"week",complete = 0)
my.bats(my.ts,n_pred=12)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.bats", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.ets")
### * my.ets

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.ets
### Title: Fit ETS algorithm and make the prediction
### Aliases: my.ets

### ** Examples

 library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"month",complete = 0)
my.ets(my.ts,n_pred=12)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.ets", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.predictions")
### * my.predictions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.predictions
### Title: Make predictions with selected algorithms
### Aliases: my.predictions

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)
### Stand alone usage
## Not run: 
##D prepare.ts(dates,values,"month") %>%
##D   my.predictions(prepedTS = .,algos = list("my.prophet","my.ets"))
##D   
## End(Not run)
### Standard input with bestmodel
## Not run: 
##D getBestModel(dates,values,freq = "month",n_test = 6) %>%
##D   my.predictions()
##D   
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.predictions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.prophet")
### * my.prophet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.prophet
### Title: Fit prophet algorithm and make the prediction
### Aliases: my.prophet

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"month",complete = 0)
my.prophet(my.ts,n_pred=12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.prophet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.sarima")
### * my.sarima

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.sarima
### Title: Fit SARIMA algorithm and make the prediction
### Aliases: my.sarima

### ** Examples

 library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"month")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"month",complete = 0)
## Not run: 
##D my.sarima(my.ts,n_pred=12)
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.sarima", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.shortterm")
### * my.shortterm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.shortterm
### Title: Fit short term algorithm and make the prediction
### Aliases: my.shortterm

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"week",complete = 0)
my.shortterm(my.ts,n_pred=12)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.shortterm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.stlm")
### * my.stlm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.stlm
### Title: Fit STLM algorithm and make the prediction
### Aliases: my.stlm

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"week",complete = 0)
my.stlm(my.ts,n_pred=12)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.stlm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("my.tbats")
### * my.tbats

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: my.tbats
### Title: Fit TBATS algorithm and make the prediction
### Aliases: my.tbats

### ** Examples

library(lubridate)
library(dplyr)
dates <- seq(as_date("2000-01-01"),as_date("2010-12-31"),"week")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"week",complete = 0)
my.tbats(my.ts,n_pred=12)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("my.tbats", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare.ts")
### * prepare.ts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare.ts
### Title: Format 2 vectors in a proper object usable by all algorithms
### Aliases: prepare.ts

### ** Examples

library(lubridate)
library(dplyr)
library(ggplot2)
dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"quarter")
values <- rnorm(length(dates))
my.ts <- prepare.ts(dates,values,"month",complete = 0)
plot(my.ts$obj.ts)
ggplot(my.ts$obj.df,aes(dates,val)) + geom_line()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare.ts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
