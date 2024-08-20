get_htest <- function(x){
    patterns <- c("ttest" = "t-test", 
                  "cortest" = "correlation")
    if(!"htest" %in% class(x)){
        stop("x need to be of class htest!")
    }else{
        p <- sapply(patterns, function(p) grepl(p, x$method))
        names(patterns)[p]
    }
}

add_critical <- function(x){
    htest <- get_htest(x)
    class(x) <- c("critval", class(x))
    return(x)
}

print.critval <- function(x, ...){
    NextMethod(x, ...)
    critical_b <- sprintf("critical b = %.3f", 0.45)
    critical_d <- sprintf("critical d = %.3f", 0.1)
    cat(critical_b)
    invisible(NULL)
}

x <- t.test(rnorm(100))
y <- cor.test(rnorm(100), rnorm(100))

x <- add_critical(x)
x
