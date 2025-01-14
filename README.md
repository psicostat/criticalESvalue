
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/psicostat/criticalESvalue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psicostat/criticalESvalue/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/psicostat/criticalESvalue/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/psicostat/criticalESvalue/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

# criticalESvalue

The goal of `criticalESvalue` is to …
<!-- TODO aggiungi qualche riga qui -->

## Installation

You can install the development version of criticalESvalue like so:

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
#> t = 2.93, df = 57, p-value = 0.0049
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  0.21597 1.14895
#> sample estimates:
#> mean of x mean of y 
#>   0.44086  -0.24160 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.7564 dc = ± 0.51704 bc = ± 0.46649 
#> g = 0.7464 gc = ± 0.5102

# t-test (standard)

ttest <- t.test(x, y, var.equal = TRUE)
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 2.93, df = 58, p-value = 0.0048
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  0.21614 1.14877
#> sample estimates:
#> mean of x mean of y 
#>   0.44086  -0.24160 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.7564 dc = ± 0.51684 bc = ± 0.46631 
#> g = 0.74657 gc = ± 0.51012

# t-test (standard) with monodirectional hyp

ttest <- t.test(x, y, var.equal = TRUE, alternative = "less")
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 2.93, df = 58, p-value = 1
#> alternative hypothesis: true difference in means is less than 0
#> 95 percent confidence interval:
#>    -Inf 1.0719
#> sample estimates:
#> mean of x mean of y 
#>   0.44086  -0.24160 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.7564 dc = -0.43159 bc = -0.3894 
#> g = 0.74657 gc = -0.42598

# within the t-test object saved from critical we have all the new values

ttest <- critical(ttest)
str(ttest)
#> List of 15
#>  $ statistic  : Named num 2.93
#>   ..- attr(*, "names")= chr "t"
#>  $ parameter  : Named num 58
#>   ..- attr(*, "names")= chr "df"
#>  $ p.value    : num 0.998
#>  $ conf.int   : num [1:2] -Inf 1.07
#>   ..- attr(*, "conf.level")= num 0.95
#>  $ estimate   : Named num [1:2] 0.441 -0.242
#>   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "difference in means"
#>  $ stderr     : num 0.233
#>  $ alternative: chr "less"
#>  $ method     : chr " Two Sample t-test"
#>  $ data.name  : chr "x and y"
#>  $ g          : num 0.747
#>  $ gc         : num 0.426
#>  $ d          : num 0.756
#>  $ bc         : num 0.389
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
#> t = 0.388, df = 28, p-value = 0.7
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.29490  0.42228
#> sample estimates:
#>      cor 
#> 0.073142 
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
#>     -0.2201       0.0107      -0.1969       0.0602  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.42311     0.39527     0.47312     0.54343
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -1.324 -0.547 -0.102  0.249  1.608 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)
#> (Intercept)  -0.2201              0.4231     0.2058   -1.07     0.29
#> x             0.0107              0.3953     0.1923    0.06     0.96
#> q            -0.1969              0.4731     0.2302   -0.86     0.40
#> z             0.0602              0.5434     0.2644    0.23     0.82
#> 
#> Residual standard error: 0.872 on 26 degrees of freedom
#> Multiple R-squared:  0.0329, Adjusted R-squared:  -0.0787 
#> F-statistic: 0.295 on 3 and 26 DF,  p-value: 0.829
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
#> [1] 0.77423
#> 
#> $dc
#> [1] 0.51698
#> 
#> $bc
#> [1] 0.54906
#> 
#> $se
#> [1] 0.27422
#> 
#> $df
#> [1] 57.277
#> 
#> $g
#> [1] 0.76404
#> 
#> $gc
#> [1] 0.51018

critical_t2s(t = ttest$statistic, se = ttest$stderr, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t = t, n1 = n1, n2 = n2, se = se, conf.level =
#> conf.level, : When var.equal = FALSE the critical value calculated from t
#> assume sd1 = sd2!
#> $d
#> [1] 0.77423
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] 0.54891
#> 
#> $se
#> [1] 0.27422
#> 
#> $df
#> [1] 58
#> 
#> $g
#> [1] 0.76417
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
#> [1] 0.77423
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
#> [1] 0.76417
#> 
#> $gc
#> [1] 0.51012
```
