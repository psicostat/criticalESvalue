
devtools::load_all()
library(criticalvalue)

# two sample t.test (var.equal = TRUE)

x <- rnorm(500, 0.5, 1)
y <- rnorm(500, 0, 1)

tt <- t.test(x, y)
tt
cc = cor.test(x,y)
fit <- lm(y ~ x)

critical(fit)
critical(fit, standardize = TRUE)
critical(tt)
critical(cc)

n = 20
yi = rnorm(n, .5, .2)
vi = runif(n,.05,.10)
xi = rnorm(n, 2, 1)
library(metafor)
fitrma = rma(yi=yi,vi=vi,mods=~xi)
summary(fitrma)
critical(fitrma)



niter = 1e4
n = 500
ps = rep(NA,niter)

pwr::pwr.t.test(n=500,power=0.8)
metafor:::.cmicalc()


