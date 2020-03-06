GDP Prediction
================
Nikhil Gupta
2020-03-05 20:10:26

-   [Setup](#setup)
-   [Stationarity](#stationarity)
    -   [Condition 1](#condition-1)
    -   [Condition 2](#condition-2)
    -   [Condition 3](#condition-3)
    -   [Conclusion](#conclusion)
-   [Stationary Model](#stationary-model)
    -   [Setup](#setup-1)
    -   [Model ID](#model-id)
    -   [Parameter Estimation](#parameter-estimation)
    -   [Factored Form](#factored-form)
    -   [Forecasting](#forecasting)
-   [Non Stationary Model](#non-stationary-model)
    -   [Model ID](#model-id-1)
    -   [Parameter Estimation](#parameter-estimation-1)
        -   [ARMA(1,1)](#arma11)
        -   [MA(1)](#ma1)
    -   [Factored Form](#factored-form-1)
    -   [Forecasting](#forecasting-1)
-   [Visualizing Results](#visualizing-results)

Setup
-----

``` r
library(tswge)
```

    ## Warning: package 'tswge' was built under R version 3.5.3

``` r
library(tswgewrapped)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages --------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts ------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(DT)
```

    ## Warning: package 'DT' was built under R version 3.5.3

``` r
data = read.csv("economic_indicators_all_ex_3mo_china.csv")
data %>% glimpse()
```

    ## Observations: 195
    ## Variables: 15
    ## $ date            <fct> 1971 Q1, 1971 Q2, 1971 Q3, 1971 Q4, 1972 Q1, 1...
    ## $ gdp_change      <dbl> 18.2, 7.7, 7.6, 4.4, 14.3, 12.1, 7.9, 12.4, 15...
    ## $ unrate          <dbl> 6.0, 5.9, 6.0, 6.0, 5.8, 5.7, 5.5, 5.2, 4.9, 4...
    ## $ nfjobs          <int> 70860, 71254, 71614, 72109, 72944, 73758, 7426...
    ## $ treas10yr       <dbl> 5.53, 6.70, 6.00, 5.89, 6.12, 6.15, 6.54, 6.41...
    ## $ fedintrate      <dbl> 3.71, 4.91, 5.55, 4.14, 3.83, 4.46, 4.87, 5.33...
    ## $ personincomechg <dbl> 4.7, 4.9, 3.8, 5.2, 3.6, 3.2, 4.7, 7.6, 7.9, 7...
    ## $ cpi             <dbl> 40.0, 40.6, 40.8, 41.1, 41.4, 41.7, 42.1, 42.5...
    ## $ population      <int> 206960, 207562, 208230, 208829, 209299, 209811...
    ## $ corpprofitchg   <dbl> 12.7497657, 3.7704482, 6.6068568, 3.2925583, 5...
    ## $ crude_wti       <dbl> 3.56, 3.56, 3.56, 3.56, 3.56, 3.56, 3.56, 3.56...
    ## $ ppi             <dbl> 37.8, 38.2, 38.3, 38.6, 39.2, 39.7, 40.2, 41.1...
    ## $ gold            <dbl> 38.800, 40.200, 42.475, 43.500, 48.375, 64.100...
    ## $ japan           <dbl> 357.5187, 357.4118, 338.0210, 320.0727, 302.53...
    ## $ uk              <dbl> 2.4187, 2.4188, 2.4694, 2.5266, 2.6181, 2.5691...

``` r
x = data$gdp_change
```

``` r
px = plotts.sample.wge(x)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-4-1.png)

Stationarity
------------

``` r
tswgewrapped::check_stationarity(x)
```

    ## Loading required namespace: ggfortify

    ## Loading required namespace: patchwork

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-5-1.png)

    ## [1] TRUE

### Condition 1

-   Looks like there is a slight trend in the data with the mean moving down over time. This would be expected. As a country is growing, its GDP is expected to be high. As it becomes a more developed economy, the GDP settles at a lower but steadier value.
-   The ACF plots shows extended autocorrelations although there is also a hint of exponentially decaying behavior. Hence, this trend (wanderig behavior) could be a result of a stationary AR process with positive phi values or it could be a result of a non-stationaty ARIMA like process.
-   **In summary, the mean is changing over time (wandering behavior) and based on the ACFs, this could be coming from either a stationary or a non stationary process.**

### Condition 2

-   Since we have only 1 realization, it is hard to say whether the varaince is different at different time points.
-   However we can make some important observations from this realization and domain knowledge. We see that in the initial part of the graph there is more volatility in the GDP numbers compared to the second half of the graph. This is again expected based on domain knowledge. Just as a developing economy has a higher GDP change value per quarter in general, this comes with a higher volatility. As an economy becomes more developed, not only does the GDP settle to a lower value in general but also the volatility decreases as well.
-   Given the above observations, there may be some hints that condition 2 has not been met

### Condition 3

-   Both the first half and second half ACFs show a damped exponential behavior for the first few lags although the second half ACFs take longer to die down. Also, the 1st half ACF shows higher values at lags of 9, 10 and 11 compared to the second half. It is also interesting to see that neither the firs half nor the second half ACF matches the full data ACF. There is enough evience here to indicate that the data is not stationary.

### Conclusion

-   Given the above analysis, there is a good chance that this data is not coming from a stationary process, although there were some hints (when looking at the mean) that it could have resulted from a stationary AR process. In order to completely eliminate the possibility that this may be coming from a stationary process, we will conduct an initial analysis with a stationary model.

Stationary Model
----------------

### Setup

``` r
n = length(x)
n.ahead = 2
batch_size = 50 ## 12 years to predict the next 2 quarters
# Placeholder for results with rolling Window (but could hold just 1 batch if needed)
results = tribble(~model, ~ASE, ~Time) 
```

### Model ID

``` r
aic5.wge(x)
```

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

    ##       p    q        aic
    ## 8     2    1   2.400740
    ## 11    3    1   2.405507
    ## 6     1    2   2.407727
    ## 9     2    2   2.408653
    ## 14    4    1   2.414109

### Parameter Estimation

``` r
est.s.mle = est.arma.wge(x, p = 2, q = 1)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 1.2175 -0.2239 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.9917B              1.0084               0.9917       0.0000
    ## 1-0.2257B              4.4298               0.2257       0.0000
    ##   
    ## 

``` r
est.s.mle$theta
```

    ## [1] 0.9128495

**OBSERVATIONS**

-   This clears a lot of confusion. **Even when fitting a stationary ARMA model, we get an estimate of phi = 0.9917 which is very close to 1 (non stationary)**. Hence the confusion that we had before can be cleared now. The data most definitely is coming from a non stationary process. For the sake of completeness, we will continue modeling with this stationary model and see how well it performs.

### Factored Form

``` r
est.s.mle$avar
```

    ## [1] 10.58793

**(1 - 0.9917B)(1 - 0.2257B)*X*<sub>*t*</sub> = (1 - 0.9128)*a*<sub>*t*</sub> with *σ*<sub>*a*</sub><sup>2</sup> = 10.5879303**

### Forecasting

``` r
phi = est.s.mle$phi
theta = est.s.mle$theta
```

``` r
# Plot the forecasts
f = fore.arma.wge(x, phi=phi, theta = theta,
                  n.ahead = n.ahead, limits=TRUE, lastn = FALSE, plot = TRUE)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
# Calculate the ASE (single batch)
r = sliding_ase(x, phi = phi, theta = theta, n.ahead = n.ahead)
```

    ## Warning in sliding_ase(x, phi = phi, theta = theta, n.ahead = n.ahead):
    ## Batch Size has not been specified. Will assume a single batch

``` r
ASEs = r$ASEs
Time = r$time

print(paste("ASE for the model (Single Batch): ", ASEs))
```

    ## [1] "ASE for the model (Single Batch):  0.264404015634086"

``` r
results = results %>% add_row(model = "ARMA_2_1_MLE_nobatch", ASE = ASEs, Time = Time) 
```

``` r
# Calculate the ASE (rolling window)
r = sliding_ase(x, phi = phi, theta = theta, n.ahead = n.ahead, batch_size = batch_size)

ASEs = r$ASEs
Time = r$time

print(paste("ASE for the model (Rolling Window): ", mean(ASEs)))
```

    ## [1] "ASE for the model (Rolling Window):  6.2215753842291"

``` r
results = results %>% add_row(model = "ARMA_2_1_MLE_bs50", ASE = ASEs, Time = Time) 
```

Non Stationary Model
--------------------

Next we will evaluate this as a non stationary model.

Because of the extended autocorrelations in the data, we will take the first difference and check the resultant data for stationarity

### Model ID

``` r
dif1 = artrans.wge(x, phi.tr = 1)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
px = plotts.sample.wge(dif1)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-17-2.png)

-   ACF is indicatove of a MA(1) model with positive theta since most ACFs die down after lag = 1 and there is a dip in the Spectral Density at f = 0.

``` r
tswgewrapped::check_stationarity(dif1)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-18-1.png)

    ## [1] TRUE

``` r
aic5.wge(dif1, type = 'aic')
```

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

    ##       p    q        aic
    ## 5     1    1   2.410123
    ## 2     0    1   2.414572
    ## 3     0    2   2.473242
    ## 16    5    0   2.490651
    ## 13    4    0   2.496508

-   AIC indicates that ARMA(1,1) is the best although our initial guess of MA(1) is not far behind.

``` r
aic5.wge(dif1, type = 'bic')
```

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  bic

    ##       p    q        bic
    ## 2     0    1   2.448261
    ## 5     1    1   2.460657
    ## 3     0    2   2.523776
    ## 10    3    0   2.567351
    ## 13    4    0   2.580732

-   With BIC, the order of the first 2 models is flipped. Now our initial guess of MA(1) takes 1st place.

We will try both to see which is the better model

### Parameter Estimation

#### ARMA(1,1)

``` r
est.ns.mle = est.arma.wge(dif1, p = 1, q = 1)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 0.2650 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.2650B              3.7731               0.2650       0.0000
    ##   
    ## 

``` r
est.ns.mle$theta
```

    ## [1] 0.973168

-   Theta is the dominant factor here. This is almost no impact of the AR side since the root is so far away from the unit circle.
-   **Hence, we should stick to our initial assessment of an MA(1) model.**

#### MA(1)

``` r
est.ns.mle = est.arma.wge(dif1, p = 0, q = 1)
```

``` r
est.ns.mle$theta
```

    ## [1] 0.8880551

### Factored Form

``` r
est.ns.mle$avar
```

    ## [1] 10.95673

**(1 - B)*X*<sub>*t*</sub> = (1 - 0.8881)*a*<sub>*t*</sub> with *σ*<sub>*a*</sub><sup>2</sup> = 10.9567263**

### Forecasting

``` r
phi = est.ns.mle$phi
theta = est.ns.mle$theta
d = 1
s = 0
```

``` r
# Plot the forecasts
f = fore.aruma.wge(x, phi=phi, theta = theta, d = d, s = s,
                  n.ahead = n.ahead, limits=TRUE, lastn = FALSE, plot = TRUE)
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
# Calculate the ASE (single batch)
r = sliding_ase(x, phi = phi, theta = theta, d = d, s = s, n.ahead = n.ahead)
```

    ## Warning in sliding_ase(x, phi = phi, theta = theta, d = d, s = s, n.ahead =
    ## n.ahead): Batch Size has not been specified. Will assume a single batch

``` r
ASEs = r$ASEs
Time = r$time

print(paste("ASE for the model (Single Batch): ", ASEs))
```

    ## [1] "ASE for the model (Single Batch):  0.207386144573842"

``` r
results = results %>% add_row(model = "ARIMA_0_1_1_0_MLE_nobatch", ASE = ASEs, Time = Time) 
```

``` r
# Calculate the ASE (sliding window)
r = sliding_ase(x, phi = phi, theta = theta, d = d, s = s, n.ahead = n.ahead, batch_size = batch_size)

ASEs = r$ASEs
Time = r$time

print(paste("ASE for the model (Rolling Window): ", mean(ASEs)))
```

    ## [1] "ASE for the model (Rolling Window):  6.6766887788098"

``` r
results = results %>% add_row(model = "ARIMA_0_1_1_0_MLE_bs50", ASE = ASEs, Time = Time) 
```

Visualizing Results
-------------------

``` r
# Final Results
DT::datatable(results)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-2e85d3856b21ba261b2a">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271","272","273","274","275","276","277","278","279","280","281","282","283","284","285","286","287","288","289","290","291","292","293","294"],["ARMA_2_1_MLE_nobatch","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARMA_2_1_MLE_bs50","ARIMA_0_1_1_0_MLE_nobatch","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50","ARIMA_0_1_1_0_MLE_bs50"],[0.264404015634086,10.408797678024,17.5458194193502,8.4101979510301,5.28482421176432,3.37857794752821,2.55254279927929,9.24074502456032,5.1955972196711,4.27504233451641,3.52013755593642,6.56828297555499,10.9588075175572,15.1476675423086,13.522174602249,6.68474661219792,6.0408731487984,0.688984013880033,0.318784198808408,4.68906388050856,7.41688826757564,5.95283297102089,3.26984958823737,1.14235017360933,1.46919992214457,0.120599229700556,1.92925078520758,9.74568659581774,7.31845882323725,3.48253837350324,8.87630772521809,37.7139878137574,34.7524801701537,3.62526172668304,0.737256499225479,2.84664360273764,2.17419128320951,1.60526397967159,0.382368856847066,0.40198599988982,5.73279752055466,6.22936340033929,0.844675757923612,3.30805746605884,3.77605546431176,1.39393006137102,2.20799109215132,1.78687658386517,3.4412893243028,7.93691165709165,2.22875303360915,0.388955691607121,0.582466894209397,4.99302104727088,5.47937272445705,1.1404449263962,0.641157255912122,2.23592253068214,3.14648574704024,0.831979576316361,1.64748615412681,0.789752943847503,1.09306792847937,3.70631019477164,1.67023360152274,1.55815193508175,1.13212923968597,7.0865601786703,6.69449770519822,10.9462579605371,15.496388458948,11.8391440696778,10.1326584923979,9.62165975162627,13.9703062181722,18.7645519861968,0.997000741851854,0.496104551980833,0.958864933744831,1.64809070579078,1.20377652930525,0.0361903106722308,11.6572282806024,14.838887701275,0.708253358910679,0.833165646100319,2.00728263455136,2.44566819013095,4.59815437714043,2.35948183967932,2.37303572318406,1.99396260108916,3.21376805337364,4.3055805148952,5.71931865082565,2.28619348080184,0.101953535238818,0.219294442789179,0.823096285466163,1.48370822463128,19.1824638526587,17.5423038740447,6.75308460813267,78.4906388233762,94.6619417569014,22.2914525499503,2.65946868218229,6.65568231231138,6.31569893326303,4.02779962605798,5.26774983264922,0.648162540126516,2.42162416335829,5.24559508068379,4.52084788600244,2.75211280975759,6.42458564104832,1.76755891812565,0.872523856846926,0.91275085107878,2.15659317685246,3.58541124789436,3.72506557669495,5.40633031988493,6.61418704507665,14.9750759682181,19.494046168188,2.71064755577966,2.5326362194004,1.22149872811943,1.82761269112962,9.32628821759225,8.68045103501012,1.56715041252481,1.50617148611802,0.0702533378693715,0.258208316344687,0.148841537744055,1.48872371607598,5.56016134705963,3.02435731420969,4.59899137514461,4.35175265357888,1.24579195599533,1.24296080645107,0.194958460312006,0.242997920210543,0.207386144573842,11.3528031149426,22.9476975470025,15.1860601928545,10.2459081278806,5.92316584975776,2.61647572734452,8.09669903692293,6.23542792533948,5.01257609388202,3.98530879301779,6.03790778516957,10.3290805014184,15.4773076964904,14.351004978223,7.05288159790852,5.81880813328076,0.909091519696484,0.102649106564792,6.32325600484,7.72537037051192,4.41594913949917,2.96924224126271,1.53830004179728,2.18932412375343,0.451603671073488,1.38912613070067,9.18020707816417,8.24433236193297,2.37330533545584,7.25371876603844,36.5144274952765,39.0891663599062,7.64890358668335,0.280424350651686,1.57890905969357,1.86406270770166,1.80666597079726,1.32776709602426,1.22895051938196,4.90123910538446,4.94066535191418,0.811397719296847,3.58210237643823,3.62837076394652,2.13586221236936,2.37719370240371,1.32267350530727,3.05829348420347,6.13443378676264,2.95695894360689,0.226190962423367,0.292798832256861,5.49924692716782,5.61780672855768,0.578955540364755,0.553621657898204,2.23751082423287,3.07954161396612,1.10270265735655,1.42616197844772,1.24816043247272,1.22350974689775,3.3318189111801,2.20624597557595,1.08414829584724,1.25155465109908,6.82548684330083,7.24986727323523,9.90191223393373,13.9063551976983,8.43069145550446,12.4151794757112,11.2437538707228,15.2972943670869,19.1660418778317,2.72531211621391,0.25730170983801,0.531486478554061,1.46977374350791,1.07634774611198,0.132639913417942,13.1461896749282,17.0993729431368,2.93031270775054,1.00492721942445,2.10159447894929,2.99523447904631,4.89051366903132,3.04079124262734,1.85500507546579,1.39529266223175,3.03807155774713,4.22598646509399,5.2984531646563,3.60882885208283,0.523793039506837,0.398571405223169,0.949346990681043,1.71147367985388,19.8178307418895,18.5856349175318,7.11689732400735,75.6290982889713,101.031196214494,35.0932377040243,5.21354588683534,9.00536095450664,9.31579789731163,6.38620529199014,8.10303496949696,2.71600667334603,2.40369366836765,5.04597515384281,4.09769997945554,2.86875613029178,6.70317243901573,3.20557328084437,0.509015728618998,0.936610598043653,2.19014740874731,3.27769323021354,3.07080816977119,4.60669108136979,6.95124306638303,13.8599157978711,15.7757914221282,4.67180126539636,1.66047082059674,1.32587528025611,1.56863398892439,9.0359752211157,10.3545189295886,2.55324596140392,1.01450970186143,0.159529185343332,0.361864569732755,0.204274394698167,1.58393580754235,5.45014613422757,4.04738803843059,4.85922164707812,4.3926914988768,1.35096376639187,1.58652378548567,0.164997270428066,0.207123983154955],[194,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,194,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>model<\/th>\n      <th>ASE<\/th>\n      <th>Time<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
results %>% 
  group_by(model) %>% 
  summarise(ASE_mean = mean(ASE),
            ASE_median = median(ASE),
            ASE_sd = sd(ASE),
            num_batches = n())
```

    ## # A tibble: 4 x 5
    ##   model                     ASE_mean ASE_median ASE_sd num_batches
    ##   <chr>                        <dbl>      <dbl>  <dbl>       <int>
    ## 1 ARIMA_0_1_1_0_MLE_bs50       6.68       3.24    11.8         146
    ## 2 ARIMA_0_1_1_0_MLE_nobatch    0.207      0.207  NaN             1
    ## 3 ARMA_2_1_MLE_bs50            6.22       3.24    11.3         146
    ## 4 ARMA_2_1_MLE_nobatch         0.264      0.264  NaN             1

``` r
ggplot(results, aes(x = model, y = ASE, color = model)) + geom_boxplot()
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
all_time = data.frame(Time = seq(1, length(x)),
                      model = c(rep("ARMA_2_1_MLE_bs50", length(x)), rep("ARIMA_0_1_1_0_MLE_bs50", length(x))),
                      ASE = 0) %>% 
  mutate_if(is.factor, as.character) 

results2 = left_join(all_time, results, by = c("Time", "model")) %>% 
  mutate(ASE = ASE.x + ASE.y) 
```

``` r
data = data.frame(Time = seq(1, length(x)), Data = x)
g1 = ggplot(data, aes(x = Time, y = Data)) + geom_line()

g2 = ggplot(results2 %>% dplyr::filter(model %in% c("ARMA_2_1_MLE_bs50", "ARIMA_0_1_1_0_MLE_bs50")),
            aes(x = Time, y = ASE, color = model)) + 
  geom_line()

g1/g2
```

![](gdp_prediction_analysis_univariate_files/figure-markdown_github/unnamed-chunk-37-1.png)

**CONCLUSION**

-   It looks like both model performs pooprly in predicting severe downturns (~ time point 80, 120, 150) and upturns (~ time points 48, 127, 172).
-   We may need to inclue exogenous variables into our model that are more indicative of these downturns and upturns in order to improve the model performance.
