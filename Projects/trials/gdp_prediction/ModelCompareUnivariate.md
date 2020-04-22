Model Compare Univariate
================
Nikhil Gupta
2020-03-12 17:25:11

``` r
library(tswge)
library(tswgewrapped)
library(tidyverse)
```

``` r
source("ModelCompareUnivariate.R")
```

``` r
data("airlog")

# Woodward Gray Airline Model
phi_wg = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
d_wg = 1
s_wg = 12

# Parzen Model
phi_pz = c(0.7400, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.3800, -0.2812)
s_pz = 12

# Box Model
d_bx = 1
s_bx = 12  
theta_bx =  c(0.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.60, -0.24)


models = list("Woodward Gray Model A" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = FALSE),
              "Woodward Gray Model B" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = TRUE),
              "Parzen Model A" = list(phi = phi_pz, s = s_pz, sliding_ase = FALSE),
              "Parzen Model B" = list(phi = phi_pz, s = s_pz, sliding_ase = TRUE),
              "Box Model A" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = FALSE),
              "Box Model B" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = TRUE)
              )
```

``` r
mdl_compare = ModelCompareUnivariate$new(x = airlog, mdl_list = models, n.ahead = 36, batch_size = 72)
```

    ## 
    ## 
    ## 
    ## Computing metrics for:  Woodward Gray Model A

    ## Warning in private$sliding_ase(x = self$get_x(), phi = self$get_models()
    ## [[name]][["phi"]], : Batch Size has not been specified. Will assume a
    ## single batch

    ## 
    ## Number of batches expected:  1 
    ## 
    ## 
    ## 
    ## Computing metrics for:  Woodward Gray Model B 
    ## Number of batches expected:  3 
    ## 
    ## 
    ## 
    ## Computing metrics for:  Parzen Model A

    ## Warning in private$sliding_ase(x = self$get_x(), phi = self$get_models()
    ## [[name]][["phi"]], : Batch Size has not been specified. Will assume a
    ## single batch

    ## 
    ## Number of batches expected:  1 
    ## 
    ## 
    ## 
    ## Computing metrics for:  Parzen Model B 
    ## Number of batches expected:  3 
    ## 
    ## 
    ## 
    ## Computing metrics for:  Box Model A

    ## Warning in private$sliding_ase(x = self$get_x(), phi = self$get_models()
    ## [[name]][["phi"]], : Batch Size has not been specified. Will assume a
    ## single batch

    ## 
    ## Number of batches expected:  1 
    ## 
    ## 
    ## 
    ## Computing metrics for:  Box Model B 
    ## Number of batches expected:  3

``` r
mdl_compare$plot_histogram_ases()
```

![](ModelCompareUnivariate_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
mdl_compare$plot_forecasts(only_sliding = TRUE)
```

![](ModelCompareUnivariate_files/figure-markdown_github/unnamed-chunk-6-1.png)![](ModelCompareUnivariate_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
mdl_compare$statistical_compare()  
```

    ##             Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Model        2 0.001460 0.0007298   2.197  0.192
    ## Residuals    6 0.001993 0.0003322               
    ## 
    ## 
    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = ASE ~ Model, data = results)
    ## 
    ## $Model
    ##                                              diff         lwr        upr
    ## Parzen Model B-Box Model B            0.028566162 -0.01709432 0.07422664
    ## Woodward Gray Model B-Box Model B     0.003429999 -0.04223048 0.04909048
    ## Woodward Gray Model B-Parzen Model B -0.025136163 -0.07079665 0.02052432
    ##                                          p adj
    ## Parzen Model B-Box Model B           0.2134940
    ## Woodward Gray Model B-Box Model B    0.9712778
    ## Woodward Gray Model B-Parzen Model B 0.2838256

    ## Call:
    ##    aov(formula = ASE ~ Model, data = results)
    ## 
    ## Terms:
    ##                       Model   Residuals
    ## Sum of Squares  0.001459617 0.001993128
    ## Deg. of Freedom           2           6
    ## 
    ## Residual standard error: 0.01822602
    ## Estimated effects may be unbalanced

``` r
# Only compares models that have sliding_ase calculations, since we need more than 1 result per model
```

``` r
ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
print(ASEs)
```

    ## # A tibble: 12 x 5
    ##    Model                     ASE Time_Test_Start Time_Test_End Batch
    ##    <chr>                   <dbl>           <dbl>         <dbl> <dbl>
    ##  1 Woodward Gray Model A 0.00419             109           144     1
    ##  2 Woodward Gray Model B 0.0193               37            72     1
    ##  3 Woodward Gray Model B 0.00563              73           108     2
    ##  4 Woodward Gray Model B 0.00419             109           144     3
    ##  5 Parzen Model A        0.0125              109           144     1
    ##  6 Parzen Model B        0.0227               37            72     1
    ##  7 Parzen Model B        0.0693               73           108     2
    ##  8 Parzen Model B        0.0125              109           144     3
    ##  9 Box Model A           0.00690             109           144     1
    ## 10 Box Model B           0.00261              37            72     1
    ## 11 Box Model B           0.00762              73           108     2
    ## 12 Box Model B           0.00862             109           144     3

``` r
forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
print(forecasts)
```

    ## # A tibble: 1,008 x 5
    ##    Model                  Time     f    ll    ul
    ##    <chr>                 <dbl> <dbl> <dbl> <dbl>
    ##  1 Woodward Gray Model A     1    NA    NA    NA
    ##  2 Woodward Gray Model A     2    NA    NA    NA
    ##  3 Woodward Gray Model A     3    NA    NA    NA
    ##  4 Woodward Gray Model A     4    NA    NA    NA
    ##  5 Woodward Gray Model A     5    NA    NA    NA
    ##  6 Woodward Gray Model A     6    NA    NA    NA
    ##  7 Woodward Gray Model A     7    NA    NA    NA
    ##  8 Woodward Gray Model A     8    NA    NA    NA
    ##  9 Woodward Gray Model A     9    NA    NA    NA
    ## 10 Woodward Gray Model A    10    NA    NA    NA
    ## # ... with 998 more rows
