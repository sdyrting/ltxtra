
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ltxtra

<!-- badges: start -->

[![R-CMD-check](https://github.com/sdyrting/ltxtra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sdyrting/ltxtra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for calculating non-standard life table variables

## Installation

You can install the development version of ltxtra from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sdyrting/ltxtra")
```

## Example: Adding life disparity to a life table

This is a basic example which shows you how to solve a common problem:

``` r
library(ltxtra)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

head(aus_2021_2023)
#> # A tibble: 6 × 12
#> # Groups:   State, Sex [1]
#>   State Sex      Age        mx      qx     ax      lx     dx    Lx      Tx    ex
#>   <chr> <chr>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>  <dbl> <dbl>   <dbl> <dbl>
#> 1 NSW   Female     0 0.00252   0.00251 0.0916 100000  251    99772 8521447  85.2
#> 2 NSW   Female     1 0.000190  0.00019 0.472   99749   19.0  99739 8421675  84.4
#> 3 NSW   Female     2 0.000110  0.00011 0.449   99730.  11.0  99724 8321936  83.4
#> 4 NSW   Female     3 0.0000900 0.00009 0.546   99719.   8.97 99715 8222212  82.5
#> 5 NSW   Female     4 0.0000800 0.00008 0.486   99710.   7.98 99706 8122497  81.5
#> 6 NSW   Female     5 0.0000700 0.00007 0.409   99702.   6.98 99698 8022791  80.5
#> # ℹ 1 more variable: OpenInterval <lgl>

daus_lt <- aus_2021_2023 %>% group_by(State,Sex) %>% lifedisp()

head(daus_lt)
#> # A tibble: 6 × 13
#> # Groups:   State, Sex [1]
#>   State Sex      Age        mx      qx     ax      lx     dx    Lx      Tx    ex
#>   <chr> <chr>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>  <dbl> <dbl>   <dbl> <dbl>
#> 1 NSW   Female     0 0.00252   0.00251 0.0916 100000  251    99772 8521447  85.2
#> 2 NSW   Female     1 0.000190  0.00019 0.472   99749   19.0  99739 8421675  84.4
#> 3 NSW   Female     2 0.000110  0.00011 0.449   99730.  11.0  99724 8321936  83.4
#> 4 NSW   Female     3 0.0000900 0.00009 0.546   99719.   8.97 99715 8222212  82.5
#> 5 NSW   Female     4 0.0000800 0.00008 0.486   99710.   7.98 99706 8122497  81.5
#> 6 NSW   Female     5 0.0000700 0.00007 0.409   99702.   6.98 99698 8022791  80.5
#> # ℹ 2 more variables: OpenInterval <lgl>, vx <dbl>
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
