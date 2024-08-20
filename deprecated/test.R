t.test <- function(x, den = "pooled", ...){
    res <- stats::t.test(x, ...)
    alpha <- 1 - attributes(res$conf.int)$conf.level
    nu <- res$parameter
    res$tc <- qt(p = alpha/2, df = nu)
    res$bc <- res$tc * res$stderr
    class(res) <- c("critval", class(res))
    return(res)
}

print.critval <- function(x, digits = getOption("digits"), prefix = "\t", ...){
    NextMethod(x, ...)
    critical_b <- sprintf("critical |b| = %.3f", abs(x$bc))
    cat(critical_b, "\n")
    invisible(NULL)
}

x <- t.test(rnorm(100), rnorm(100), paired = TRUE)

x
