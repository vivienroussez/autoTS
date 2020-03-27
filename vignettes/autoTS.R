## -----------------------------------------------------------------------------
knitr::opts_chunk$set(warning = F,message = F,fig.width = 8,fig.height = 5)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
library(autoTS)

## -----------------------------------------------------------------------------
dat <- read.csv("Data/namq_10_gdp_1_Data.csv")
str(dat)
head(dat)

## -----------------------------------------------------------------------------
dat <- mutate(dat,dates=yq(as.character(TIME)),
              values = as.numeric(stringr::str_remove(Value," "))) %>% 
  filter(year(dates)>=2000 & 
           S_ADJ=="Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)" &
           UNIT == "Current prices, million euro")

filter(dat,GEO %in% c("France","Austria")) %>% 
  ggplot(aes(dates,values,color=GEO)) + geom_line() + theme_minimal() +
  labs(title="GDP of (completely) random countries")

## -----------------------------------------------------------------------------
ex1 <- filter(dat,GEO=="France") 
preparedTS <- prepare.ts(ex1$dates,ex1$values,"quarter")

## What is in this new object ?
str(preparedTS)
plot.ts(preparedTS$obj.ts)
ggplot(preparedTS$obj.df,aes(dates,val)) + geom_line() + theme_minimal()



## -----------------------------------------------------------------------------
## What is the best model for prediction ?
best.algo <- getBestModel(ex1$dates,ex1$values,"quarter",graph = F)
names(best.algo)
print(paste("The best algorithm is",best.algo$best))
best.algo$graph.train


## -----------------------------------------------------------------------------
## Build the predictions
final.pred <- my.predictions(bestmod = best.algo)
tail(final.pred,24)
ggplot(final.pred) + geom_line(aes(dates,actual.value),color="black") + 
  geom_line(aes_string("dates",stringr::str_remove(best.algo$best,"my."),linetype="type"),color="red") +
  theme_minimal() 

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyr))
dat.wide <- select(dat,GEO,dates,values) %>% 
  group_by(dates) %>% 
  spread(key = "GEO",value = "values")
head(dat.wide)

## -----------------------------------------------------------------------------
library(doParallel)
pipeline <- function(dates,values)
{
  pred <- getBestModel(dates,values,"quarter",graph = F)  %>%
    my.predictions()
  return(pred)
}
doMC::registerDoMC(parallel::detectCores()-1) # parallel backend (for UNIX)

system.time({
  res <- foreach(ii=2:ncol(dat.wide),.packages = c("dplyr","autoTS")) %dopar%
  pipeline(dat.wide$dates,pull(dat.wide,ii))
})
names(res) <- colnames(dat.wide)[-1]
str(res)

## -----------------------------------------------------------------------------
sapply(res,function(xx) colnames(select(xx,-dates,-type,-actual.value)) ) %>% table()
sapply(res,function(xx) colnames(select(xx,-dates,-type,-actual.value)) )

