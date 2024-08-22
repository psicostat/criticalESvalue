
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalESvalue

<!-- badges: start -->
<!-- badges: end -->

The goal of criticalvalue is to …
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
#> t = 1.27, df = 57.7, p-value = 0.21
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.20152  0.90726
#> sample estimates:
#> mean of x mean of y 
#>  0.290583 -0.062284 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.329 dc = ± 0.51689 bc = ± 0.55439 
#> g = 0.3247 gc = ± 0.51014

# t-test (standard)

ttest <- t.test(x, y, var.equal = TRUE)
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.27, df = 58, p-value = 0.21
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.20147  0.90721
#> sample estimates:
#> mean of x mean of y 
#>  0.290583 -0.062284 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.329 dc = ± 0.51684 bc = ± 0.55434 
#> g = 0.32472 gc = ± 0.51012

# t-test (standard) with monodirectional hyp

ttest <- t.test(x, y, var.equal = TRUE, alternative = "less")
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 1.27, df = 58, p-value = 0.9
#> alternative hypothesis: true difference in means is less than 0
#> 95 percent confidence interval:
#>     -Inf 0.81577
#> sample estimates:
#> mean of x mean of y 
#>  0.290583 -0.062284 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.329 dc = -0.43159 bc = -0.46291 
#> g = 0.32472 gc = -0.42598

# within the t-test object saved from critical we have all the new values

ttest <- critical(ttest)
str(ttest)
#> List of 15
#>  $ statistic  : Named num 1.27
#>   ..- attr(*, "names")= chr "t"
#>  $ parameter  : Named num 58
#>   ..- attr(*, "names")= chr "df"
#>  $ p.value    : num 0.896
#>  $ conf.int   : num [1:2] -Inf 0.816
#>   ..- attr(*, "conf.level")= num 0.95
#>  $ estimate   : Named num [1:2] 0.2906 -0.0623
#>   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "difference in means"
#>  $ stderr     : num 0.277
#>  $ alternative: chr "less"
#>  $ method     : chr " Two Sample t-test"
#>  $ data.name  : chr "x and y"
#>  $ g          : num 0.325
#>  $ gc         : num 0.426
#>  $ d          : num 0.329
#>  $ bc         : num 0.463
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
#> t = 1.25, df = 28, p-value = 0.22
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.14148  0.54550
#> sample estimates:
#>     cor 
#> 0.23054 
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
#>      0.0822       0.3405      -0.0142      -0.4964  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.46049     0.40167     0.36100     0.48140
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.6531 -0.6124 -0.0963  0.4325  2.3414 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)   0.0822              0.4605     0.2240    0.37    0.717  
#> x             0.3405              0.4017     0.1954    1.74    0.093 .
#> q            -0.0142              0.3610     0.1756   -0.08    0.936  
#> z            -0.4964              0.4814     0.2342   -2.12    0.044 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.05 on 26 degrees of freedom
#> Multiple R-squared:  0.193,  Adjusted R-squared:  0.0998 
#> F-statistic: 2.07 on 3 and 26 DF,  p-value: 0.128
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
#> [1] 0.35587
#> 
#> $dc
#> [1] 0.51703
#> 
#> $bc
#> [1] 0.50091
#> 
#> $se
#> [1] 0.25015
#> 
#> $df
#> [1] 57.018
#> 
#> $g
#> [1] 0.35116
#> 
#> $gc
#> [1] 0.51019

critical_t2s(t = ttest$statistic, se = ttest$stderr, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t = t, n1 = n1, n2 = n2, se = se, conf.level =
#> conf.level, : When var.equal = FALSE the critical value calculated from t
#> assume sd1 = sd2!
#> $d
#> [1] 0.35587
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] 0.50073
#> 
#> $se
#> [1] 0.25015
#> 
#> $df
#> [1] 58
#> 
#> $g
#> [1] 0.35124
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
#> [1] 0.35587
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
#> [1] 0.35124
#> 
#> $gc
#> [1] 0.51012
```
