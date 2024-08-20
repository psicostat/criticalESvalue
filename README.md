
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalvalue

<!-- badges: start -->
<!-- badges: end -->

The goal of criticalvalue is to …

## Installation

You can install the development version of criticalvalue like so:

``` r
# require(devtools)
devtools::install_github("filippogambarota/criticalvalue")
```

## Installation (temporary)

The package is not production-ready thus to test the feature we suggest
to clone or download the repository, open the R Project and load the
functions with `devtools::load_all()`.

## Example from fitted objects

This is a basic example which shows you how to solve a common problem:

### T Test

``` r
# library(criticalvalue) # when the package will be ready
devtools::load_all() # loading all functions for testing
#> ℹ Loading criticalvalue

# t-test (welch)

x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)
critical(ttest)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x and y
#> t = 1.57, df = 56.5, p-value = 0.12
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.12697  1.03569
#> sample estimates:
#> mean of x mean of y 
#>  0.487716  0.033355 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.40418 |dc| = 0.51713 |bc| = 0.58133 
#> g = 0.39879 |gc| = 0.51023

# t-test (standard)

ttest <- t.test(x, y, var.equal = TRUE)
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.57, df = 58, p-value = 0.12
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.12665  1.03537
#> sample estimates:
#> mean of x mean of y 
#>  0.487716  0.033355 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.40418 |dc| = 0.51684 |bc| = 0.58101 
#> g = 0.39893 |gc| = 0.51012

# t-test (standard) with monodirectional hyp

ttest <- t.test(x, y, var.equal = TRUE, alternative = "less")
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.57, df = 58, p-value = 0.94
#> alternative hypothesis: true difference in means is less than 0
#> 95 percent confidence interval:
#>     -Inf 0.93954
#> sample estimates:
#> mean of x mean of y 
#>  0.487716  0.033355 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.40418 dc = -0.43159 bc = -0.48518 
#> g = 0.39893 gc = -0.42598

# within the t-test object saved from critical we have all the new values

ttest <- critical(ttest)
str(ttest)
#> List of 15
#>  $ statistic  : Named num 1.57
#>   ..- attr(*, "names")= chr "t"
#>  $ parameter  : Named num 58
#>   ..- attr(*, "names")= chr "df"
#>  $ p.value    : num 0.939
#>  $ conf.int   : num [1:2] -Inf 0.94
#>   ..- attr(*, "conf.level")= num 0.95
#>  $ estimate   : Named num [1:2] 0.4877 0.0334
#>   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "difference in means"
#>  $ stderr     : num 0.29
#>  $ alternative: chr "less"
#>  $ method     : chr " Two Sample t-test"
#>  $ data.name  : chr "x and y"
#>  $ g          : Named num 0.399
#>   ..- attr(*, "names")= chr "1"
#>  $ gc         : Named num 0.426
#>   ..- attr(*, "names")= chr "1"
#>  $ d          : num 0.404
#>  $ bc         : num 0.485
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
#> t = -0.434, df = 28, p-value = 0.67
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.42940  0.28694
#> sample estimates:
#>       cor 
#> -0.081785 
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
#>      0.0261      -0.0606      -0.1595       0.1569  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.56091     0.48702     0.57990     0.54323
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -1.959 -0.585 -0.247  0.481  3.209 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)
#> (Intercept)   0.0261              0.5609     0.2729    0.10     0.92
#> x            -0.0606              0.4870     0.2369   -0.26     0.80
#> q            -0.1595              0.5799     0.2821   -0.57     0.58
#> z             0.1569              0.5432     0.2643    0.59     0.56
#> 
#> Residual standard error: 1.26 on 26 degrees of freedom
#> Multiple R-squared:  0.0272, Adjusted R-squared:  -0.085 
#> F-statistic: 0.243 on 3 and 26 DF,  p-value: 0.866

# linear model (standardized)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit, standardize = TRUE)
fit
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Coefficients:
#>            (Intercept)                       x                       q  
#>  0.0000000000000000024  -0.0515084809938952254  -0.1122834490871214147  
#>                      z  
#>  0.1226511806886874945  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.39091     0.41415     0.40820     0.42476
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -1.618 -0.483 -0.204  0.398  2.649 
#> 
#> Coefficients:
#>                           Estimate    |Critical Estimate|
#> (Intercept)  0.0000000000000000024  0.3909120122152270893
#> x           -0.0515084809938952254  0.4141515247927876620
#> q           -0.1122834490871214147  0.4082029527114897638
#> z            0.1226511806886874945  0.4247575870243170404
#>                         Std. Error t value Pr(>|t|)
#> (Intercept)  0.1901758276316976304    0.00     1.00
#> x            0.2014816801000057778   -0.26     0.80
#> q            0.1985877433995782149   -0.57     0.58
#> z            0.2066414516080858910    0.59     0.56
#> 
#> Residual standard error: 1.04 on 26 degrees of freedom
#> Multiple R-squared:  0.0272, Adjusted R-squared:  -0.085 
#> F-statistic: 0.243 on 3 and 26 DF,  p-value: 0.866
```

### Meta-analysis

``` r
require(metafor)
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 4.0-0). For an
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
#> [1] 0.5146
#> 
#> $dc
#> [1] 0.51901
#> 
#> $bc
#> [1] 0.52147
#> 
#> $df
#> [1] 48.481
#> 
#> $g
#> [1] 0.50659
#> 
#> $gc
#> [1] 0.51093

critical_t2s(t = ttest$statistic, se = ttest$stderr, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!
#> $d
#>      t 
#> 0.5146 
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] 0.51929
#> 
#> $df
#> [1] 58
#> 
#> $g
#>       t 
#> 0.50791 
#> 
#> $gc
#> [1] 0.51012

critical_t2s(t = ttest$statistic, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When se = NULL bc cannot be computed, returning NA!
#> $d
#>      t 
#> 0.5146 
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] NA
#> 
#> $df
#> [1] 58
#> 
#> $g
#>       t 
#> 0.50791 
#> 
#> $gc
#> [1] 0.51012
```
