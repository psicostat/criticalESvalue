
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/psicostat/criticalESvalue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psicostat/criticalESvalue/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/psicostat/criticalESvalue/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/psicostat/criticalESvalue/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

# criticalESvalue

The `criticalESvalue` package provide a set of functions to calculate
the critical effects size value for the common statistical model.
Currently, the package support the `htest` class (i.e., `t.test` and
`cor.test`), the `lm` class and the `rma` class from the `metafor`
package.

The package contains a set of functions to work with summary data and
the `critical()` method that takes objects of class `htest`, `lm` or
`rma` and provide enhanced printing and summary methods with information
about critical effects size.

## Installation

You can install the development version of `criticalESvalue` like so:

``` r
# require(remotes)
remotes::install_github("psicostat/criticalESvalue")
```

## Examples

``` r
# loading the package
library(criticalESvalue)
```

### T Test

``` r
# t-test (welch)

x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)
critical(ttest)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x and y
#> t = 1.8, df = 57.7, p-value = 0.077
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.055794  1.065374
#> sample estimates:
#> mean of x mean of y 
#>   0.36937  -0.13542 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.46545 dc = ± 0.51689 bc = ± 0.56058 
#> g = 0.45937 gc = ± 0.51014

# t-test (standard)

ttest <- t.test(x, y, var.equal = TRUE)
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.8, df = 58, p-value = 0.077
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.055739  1.065319
#> sample estimates:
#> mean of x mean of y 
#>   0.36937  -0.13542 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.46545 dc = ± 0.51684 bc = ± 0.56053 
#> g = 0.4594 gc = ± 0.51012

# t-test (standard) with monodirectional hyp

ttest <- t.test(x, y, var.equal = TRUE, alternative = "less")
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.8, df = 58, p-value = 0.96
#> alternative hypothesis: true difference in means is less than 0
#> 95 percent confidence interval:
#>     -Inf 0.97286
#> sample estimates:
#> mean of x mean of y 
#>   0.36937  -0.13542 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.46545 dc = -0.43159 bc = -0.46807 
#> g = 0.4594 gc = -0.42598

# within the t-test object saved from critical we have all the new values

ttest <- critical(ttest)
str(ttest)
#> List of 15
#>  $ statistic  : Named num 1.8
#>   ..- attr(*, "names")= chr "t"
#>  $ parameter  : Named num 58
#>   ..- attr(*, "names")= chr "df"
#>  $ p.value    : num 0.962
#>  $ conf.int   : num [1:2] -Inf 0.973
#>   ..- attr(*, "conf.level")= num 0.95
#>  $ estimate   : Named num [1:2] 0.369 -0.135
#>   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "difference in means"
#>  $ stderr     : num 0.28
#>  $ alternative: chr "less"
#>  $ method     : chr " Two Sample t-test"
#>  $ data.name  : chr "x and y"
#>  $ g          : num 0.459
#>  $ gc         : num 0.426
#>  $ d          : num 0.465
#>  $ bc         : num 0.468
#>  $ dc         : num 0.432
#>  - attr(*, "class")= chr [1:3] "critvalue" "ttest" "htest"
```

We can check the results using:

``` r
ttest <- t.test(x, y)
ttest <- critical(ttest)

t <- ttest$bc/ttest$stderr # critical numerator / standard error

# should be 0.05 (or alpha)
(1 - pt(ttest$bc/ttest$stderr, ttest$parameter)) * 2
#> [1] 0.05
```

### Correlation Test

``` r
# cor.test
ctest <- cor.test(x, y)
critical(ctest)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 0.257, df = 28, p-value = 0.8
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.31735  0.40172
#> sample estimates:
#>      cor 
#> 0.048461 
#> 
#> |== Critical Value ==| 
#> |rc| = 0.36101
```

### Linear Model

``` r
# linear model (unstandardized)
z <- rnorm(30)
q <- rnorm(30)

dat <- data.frame(x, y, z, q)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit)
fit
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Coefficients:
#> (Intercept)            x            q            z  
#>    -0.11680      0.06495     -0.13635      0.00332  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.48599     0.43970     0.50174     0.39505
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.8786 -0.9037 -0.0576  0.8473  2.3993 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)
#> (Intercept) -0.11680             0.48599    0.23643   -0.49     0.63
#> x            0.06495             0.43970    0.21391    0.30     0.76
#> q           -0.13635             0.50174    0.24409   -0.56     0.58
#> z            0.00332             0.39505    0.19219    0.02     0.99
#> 
#> Residual standard error: 1.17 on 26 degrees of freedom
#> Multiple R-squared:  0.0162, Adjusted R-squared:  -0.0973 
#> F-statistic: 0.143 on 3 and 26 DF,  p-value: 0.933
```

### Meta-analysis

``` r
library(metafor)
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 4.6-0). For an
#> introduction to the package please type: help(metafor)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
fit <- rma(yi, vi, mods=cbind(ablat, year), data=dat)
critical(fit)
#> 
#> Mixed-Effects Model (k = 13; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of residual heterogeneity):     0.1108 (SE = 0.0845)
#> tau (square root of estimated tau^2 value):             0.3328
#> I^2 (residual heterogeneity / unaccounted variability): 71.98%
#> H^2 (unaccounted variability / sampling variability):   3.57
#> R^2 (amount of heterogeneity accounted for):            64.63%
#> 
#> Test for Residual Heterogeneity:
#> QE(df = 10) = 28.3251, p-val = 0.0016
#> 
#> Test of Moderators (coefficients 2:3):
#> QM(df = 2) = 12.2043, p-val = 0.0022
#> 
#> Model Results:
#> 
#>          estimate       se     zval    pval     ci.lb    ci.ub     
#> intrcpt   -3.5455  29.0959  -0.1219  0.9030  -60.5724  53.4814     
#> ablat     -0.0280   0.0102  -2.7371  0.0062   -0.0481  -0.0080  ** 
#> year       0.0019   0.0147   0.1299  0.8966   -0.0269   0.0307     
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>         |critical estimate|
#> intrcpt            62.85783
#> ablat               0.02211
#> year                0.03172
```

## Example from summary statistics

``` r
x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)

m1 <- mean(x)
m2 <- mean(y)
sd1 <- sd(x)
sd2 <- sd(y)
n1 <- n2 <- 30

critical_t2s(m1, m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2)
#> $d
#> [1] -0.023955
#> 
#> $dc
#> [1] 0.51721
#> 
#> $bc
#> [1] 0.52285
#> 
#> $se
#> [1] 0.26102
#> 
#> $df
#> [1] 56.144
#> 
#> $g
#> [1] -0.023633
#> 
#> $gc
#> [1] 0.51026

critical_t2s(t = ttest$statistic, se = ttest$stderr, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t = t, n1 = n1, n2 = n2, se = se, conf.level =
#> conf.level, : When var.equal = FALSE the critical value calculated from t
#> assume sd1 = sd2!
#> $d
#> [1] -0.023955
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] 0.52248
#> 
#> $se
#> [1] 0.26102
#> 
#> $df
#> [1] 58
#> 
#> $g
#> [1] -0.023643
#> 
#> $gc
#> [1] 0.51012

critical_t2s(t = ttest$statistic, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t = t, n1 = n1, n2 = n2, se = se, conf.level =
#> conf.level, : When var.equal = FALSE the critical value calculated from t
#> assume sd1 = sd2!
#> Warning in crit_from_t_t2s(t = t, n1 = n1, n2 = n2, se = se, conf.level =
#> conf.level, : When se = NULL bc cannot be computed, returning NA!
#> $d
#> [1] -0.023955
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] NA
#> 
#> $se
#> NULL
#> 
#> $df
#> [1] 58
#> 
#> $g
#> [1] -0.023643
#> 
#> $gc
#> [1] 0.51012
```
