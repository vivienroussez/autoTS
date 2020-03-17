# Package autoTS v0.9

## Introduction

This R package is meant to provide a high-level interface to make **automated** predictions for **univariate** time series. The purpose is to avoid to deal with the different classes required by the different libraries (ts objects, data frames, matrices...) and to get fast results for large amount of time series. The final results are included in tidy dataframes.

As of version 0.8, it is possible to deal with daily, weekly, monthly or quarterly time series.

In order to be as generic as possible, the inputs required by most functions of this package are :

- a vector of dates, such that [lubridate](https://lubridate.tidyverse.org/) can parse it
- a vector of the same size as the previous, contraining the values for each datetime
- a character string indicating the frequency of the series

It implements the following algorithms (see below for details) :

- SARIMA
- Prophet
- ETS (implements several exponential smoothing models)
- BATS
- TBATS
- STL
- Custom formula that uses only previous year's data ("short term")

It provides a function `getbestModel` which trains every available algorithm, and compares the predictions of each of them on the last observed year (or n last observations) with the actual data (which is excluded from the training data).
The function `my.predictions` provides automatic prediction for the selected algorithms one year ahead of the last known date.
Warnings :

- Please note that ETS cannot handle time series of frequency higher than 24
- Forecasting more than one year ahead will de facto exclude the "short term" algorithm

The `getbestModel` and `my.predictions` functions also allow the user to compute a **bagged** estimator, defined as the mean of all implemented algorithms

## Usage
**Warning : interface (getBestModel and my.predictions functions) changed as of version 0.9**
Who is performing best on a random walk  with drift ??

```{r}
library(autoTS)
library(magrittr)

## Generate dummy data
dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
values <- 10+ 1:length(dates)/10 + rnorm(length(dates),mean = 0,sd = 10)

## Find best algo and predict on full sample
implement <- getBestModel(dates,values,freq = "month",bagged = T)
getBestModel(dates,values,freq = "month",n_test = 6) %>% 
  my.predictions()
```

You can use the **shiny user interface** to upload a csv file with you own time series to test the prediction interactively. This allows only one prediction at a time ; for bulk prediction, use the code and refer to the example notebook contained in the package.

```{r}
runUserInterface()
```

## Warnings

This package has been developped with very standard and arbitrary default values for the parameters of each algorithm, which cannot be relevant for every use case. **Users are invited to report bugs and are very welcome to do pull request in order to make it more flexible as well**.

## To-Do

- Licencing
- Implement LSTM
- Implement random forest 
- Add parameters to tweak algorithms
- Allow cross validatation wrt starting date to evaluate wether keeping most recent data helps improving the models
- Add more possible frequencies (infra-day ?)

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

**Scalability** : faster than ARIMA but somehow long to train

### Exponential smoothing (ETS)

The idea of this algorithm is to decompose (seasonality, trend,...) the time series and then predict each component with a weighted average of the past. The weights decline exponentially in time. The ets function of the `forecast` package tries different functionnal specifications and keeps the most effective. [More details](https://robjhyndman.com/talks/RevolutionR/6-ETS.pdf) 

**Scalability** : this model is very fast to train

### BATS and TBATS

BATS stands for Box-Cox (transformation), ARIMA, Trend and Seasonality. The T of TBATS stands for Trigonometric (difference for seasonality model). Some insights about how the algorithm works can be found [here](https://medium.com/intive-developers/forecasting-time-series-with-multiple-seasonalities-using-tbats-in-python-398a00ac0e8a) (with python code)

### Seasonal and Trend decomposition using Loess (STLM)

STLM decomposes the time series in trend, seasonal and remainder. The forecast is obtained through :

- Naive prediction of seasonal component 
- Prediction with ARIMA or ETS for the seasonnaly adjusted series (trend + error)


A good general presentation of all algorithms (except prophet) can be found on this [presentation](https://robjhyndman.com/files/2-AutomaticForecasting.pdf)


## Changelog

### Version 0.9 :

- Correct error computation. Add error funtions in packages (rmse, mae)
- Add outputs to bestmodel (errors, algorithms used)
- Allow custom list of algos to compute bagged estimator
- Allow my.predictions function to take bestmodel object as input
- updated documentation

### Version 0.8 :

- Handles daily and weekly series better
- For forecast algorithms : switched from stats::ts objects to forecast::msts (multiple seasonalities)
- SCripts adapt for daily series : if less than one year, seasonality/frequency is set to 7 (for msts) ; if longer, seasonality is set to c(365.25,7)
- Theme change for training graphic
### Version 0.7 :

- Added graphical interface with shiny app
- bug fixes : 
    + fix bug for short term algorithm (wrong predictions if number of periods to forecast forward was different than one year ahead )
    + avoid running short term algorithm if number of period to forecast > one year
    
### Version 0.5 :

First stable version
