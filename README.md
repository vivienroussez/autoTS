# Package autoTS

## Introduction

This R package is meant to provide a high-level interface to make **automated** predictions for **univariate** time series. The purpose is to avoid to deal with the different classes required by the different libraries (ts objects, data frames, matrices...) and to get fast results for large amount of time series.

As of version 0.1, it is possible to deal with daily, weekly, monthly or quarterly time series.

In order to be as generic as possible, the input required by this package is :

- a vector of dates, such that [lubridate](https://lubridate.tidyverse.org/) can parse it
- a vector of the same size as the previous, contraining the values for each datetime

It implements the following algorithms (see below for details) :

- SARIMA
- Prophet
- ETS (implements several exponential smoothing models)

It provides a function `getbestModel` which trains every available algorithm, and compare the predictions of each of them on the last year (or n last observations) with the actual data (which is excluded from the training data).
The function `my.predictions` provides automatic prediction for the selected algorithms one year ahead of the last know date.

## Usage

Who is performing best on a random walk  with drift ??

```{r}
library(autoTS)
library(magrittr)

## Generate dummy data
dates <- seq(lubridate::as_date("2000-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 1:length(dates)/10 + rnorm(length(dates))
## Find best algo
which.model <- getBestModel(dates,values,freq = "month",algos = list(my.sarima,my.ets,my.prophet))
## Implement it on full sample
res <- prepare.ts(dates,values,freq = "month") %>%
  my.predictions(algos =list(get(which.model)))
```

## Warnings

This package has been developped with very standard and arbitrary default values for the parameters of each algorithm, which cannot be relevant for every use case. **Users are invited to report bugs and are very welcome to do pull request in order to make it more flexible as well**.

## To-Do

- Licencing
- Implement LSTM
- Implement random forest 
- Add parameters to tweak algorithms
- Make prediction
- Cross validate on the beginning year to evaluate wether keeping most recent data helps improve the models
- Add more possible frequencies 

## Available algorithms

### (S)ARIMA : (Seasonal) AutoRegressive Integrated Moving Average. 
The general form of such a model is :
$$ (I-B)^d \Phi(B)X_t = \Psi(B)\epsilon_t$$ 

Where $X_t$ is the time serie, B the lag operator, $\Phi$ and $\Psi$ are polynomes in powers of B with respective degrees p and d, and $\epsilon_t$ is a white noise. In other words, we try to model the differenciated serie $(I-B)^d X_t$ as a function of its p past values and a moving average of a white noise of order q.
The function auto.arima tries to find the best values of p, d and q in order to minimize the AIC criterion.

**Scalability** : this algorithm is the slowest to train.

### Facebook's prophet

The idea of this algorithm is to decompose (seasonality, trend,...) the time series and then predict each component
$$ X_t = g(t) + s(t) + h(t) + \epsilon(t) $$
Where g is the trend, s the seasonal and h holiday (has to be provided by the user, which has not been done yet) components. These components are estimated using GAM models. The detailed paper can be found [there](https://peerj.com/preprints/3190/) 

**Scalability** : faster than ARIMA but somewhat long to train

### Exponential smoothing (ETS)

The idea of this algorithm is to decompose (seasonality, trend,...) the time series and then predict each component with a weighted average of the past. The weights decline exponentially in time. The ets function of the `forecast` package tries different functionnal specifications and keeps the most effective. [More details](https://robjhyndman.com/talks/RevolutionR/6-ETS.pdf) 

**Scalability** : this model is very fast to train

