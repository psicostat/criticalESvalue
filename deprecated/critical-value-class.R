set.seed(2021)
x <- rnorm(100)
y <- rnorm(100)
ht <- t.test(x, y)
attributes(ht$conf.int)
attr(ht, "class") <- c("feonyte", "htest")
class(ht)

print.feonyte <- function(x, ...){
    NextMethod(x, ...)
    alpha <- attr(x$conf.int, "conf.level")
    cat("confidence level:", alpha, "\n\n")
    invisible(NULL)
}

stats:::print.htest(ht)

fit <- t.test(x, y, var.equal = TRUE)


t.test()

.all_null <- function(...){
    all(sapply(list(...), is.null))
}

critical_t_test <- function(sd1 = NULL, 
                            sd2 = NULL, 
                            n1 = NULL,
                            n2 = NULL,
                            se = NULL, 
                            df = NULL,
                            alpha = 0.05,
                            var.equal = FALSE){
    if(is.null(se) & is.null(df)){
        if(is.null(sd1) | is.null(sd2) | is.null(n1) | is.null(n2)){
            stop("when se/df are NULL, sd1, sd2, n1 and n2 must be specified!")
        }
        stats <- t_test_se_df(n1, n2, sd1, sd2, var.equal)
        se <- stats$se
        df <- stats$df
    }
    tc <- critical_t(df, alpha/2)
    critical_b <- se * tc
    dc <- abs(critical_b)
    list(critical = dc, se = se, df = df)
}

critical_cor_test <- function(n = NULL, alpha = 0.05){
    df <- n - 2
    tc <- critical_t(df, alpha/2)
    abs(tc / sqrt(df + tc))
}

critical_t <- function(df, alpha = 0.05){
    qt(alpha, df = df)
}


x <- rnorm(100)
y <- rnorm(100)

fit <- t.test(x, y)
d <- critical_t_test(fit$stderr, fit$parameter)
pt(d/fit$stderr, fit$parameter, lower.tail = FALSE)


se <- sqrt(var(x)/100 + var(y)/100)
fit$stderr

t_test_se_df <- function(n1 = NULL, n2 = NULL, 
                         sd1 = NULL, sd2 = NULL,
                         denominator = c("pooled", "diff"),
                         var.equal = FALSE,
                         method = "paired"){
    if(var.equal){
        # from t.test source code https://github.com/SurajGupta/r-source/blob/a28e609e72ed7c47f6ddfbb86c85279a0750f0b7/src/library/stats/R/t.test.R#L81
        df <- n1 + n2 - 2
        v <- 0
        if(n1 > 1) v <- v + (n1-1) * sd1^2
        if(n2 > 1) v <- v + (n2-1) * sd2^2
        v <- v/df
        se <- sqrt(v*(1/n1+1/n2))
    }else{
        se1 <- sqrt(sd1^2/n1)
        se2 <- sqrt(sd2^2/n2)
        se <- sqrt(se1^2 + se2^2)
        df <- se^4/(se1^4/(n1-1) + se2^4/(n2-1))
    }
    list(se = se, df = df)
}

critical_t_test(sd1 = 1, sd2 = 1, n1 = 100, n2 = 100, var.equal = TRUE)
critical_t_test(se = 0.1414214, df = 198)

# one-sample
# t = (mu1 - mu)/se
# se = sd1/sqrt(n)
# df = n - 1
# d = (mu1 - mu)/sd1

# paired
# t = mud / se
# se = sdd/sqrt(n)
# df = n - 1
# dz = mud / sdd
# d = mud / sp
# sp = (sd1^2 + sd2^2)/2

# two-sample
# t = (mu1 - mu2) / se
## var.equal = TRUE
# se = sp * (1/n1 + 1/n2)
# sp = sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1))/(n1 + n2 - 2))
# df = n1 + n2 - 2
# d = (mu1 - mu2)/sp
## var.equal = FALSE (welch test)
# se1 = sqrt(sd1^2/n1)
# se2 = sqrt(sd2^2/n2)
# se = sqrt(se1^2 + se2^2)
# df = se^4/(se1^4/(n1 - 1) + se2^4/(n2 - 1))
# d = (mu1 - mu2)/sem
# sem = sqrt((sd1^2 + sd2^2)/2)

# g conversion
# cm = metafor:::.cmicalc(df) # correction factor
# g = d * cm
# vg = cm^2 * vd

# critical value
## standardized

