critical <- function(x, ...){
  UseMethod("critical")
}

#' @export
critical.htest <- function(x, denominator = "pooled"){
  denominator <- match.arg(denominator, c("pooled", "differences"))
  # all implemented subtype of htest objects
  mtds <- c("Two Sample", "One Sample", "correlation", "Paired")
  
  if(all(!grepl(paste0(mtds, collapse = "|"), x$method))){
    stop(
      sprintf("method %s of class %s not implemented yet!",
              x$method, class(x))
    )
  } else{
    D <- insight::get_data(x)
    conf.level <- attributes(x$conf.int)$conf.level
    
    hypothesis <- switch(x$alternative,
      "two.sided" = "><",
      "greater" = ">",
      "less" = "<"
    )
    
    alpha <- .get_alpha(conf.level, hypothesis)
    df <- x$parameter
    tc <- qt(alpha, df)
    t <- x$statistic
    se <- x$stderr
    b <- x$estimate
    method <- x$method
  }
  
  if(grepl("correlation", method)){
    d <- b
    dc <- bc <- tc / sqrt((df + tc^2))
    class(x) <- c("critvalue", "ctest", "htest")
  }else{ # if t-test
    if(grepl("Two Sample", method)){
      b <- b[1] - b[2] # x - y
      n <- tapply(D$x, D$y, length)
      if(grepl("Welch", method)){
        ss <- tapply(D$x, D$y, sd)
        s <- sqrt((ss[1]^2 + ss[2]^2)/2) # average sd
      }else{
        se_df <- sqrt(1/n[1] + 1/n[2]) # standard error without sd
        s <- se / se_df
      }
    } else if(grepl("One Sample", method)){
      n <- length(D$x)
      se_df <- sqrt(1/n) # standard error without sd
      s <- se / se_df
    } else if(grepl("Paired", method)){
      n <- tapply(D$x, D$y, length)
      se_df <- sqrt(1/n[1]) # standard error without sd
      ss <- tapply(D$x, D$y, sd)
      if(denominator == "pooled"){
        s <- sqrt((ss[1]^2 + ss[2]^2)/2)
      } else if(denominator == "differences"){
        s <- se / se_df
      }
    }
    d <- b / s
    bc <- tc * se
    dc <- bc / s
    
    # hedges correction
    J <- .get_J(df)
    g <- d * J
    gc <- dc * J
    
    x$g <- unname(g)
    x$gc <- unname(gc)
    
    class(x) <- c("critvalue", "ttest", "htest")
    
  }
  
  # common elements
  x$d <- unname(d)
  x$bc <- unname(bc)
  x$dc <- unname(dc)

  return(x)
}
#' @export
critical.lm <- function(x, conf.level = 0.95, 
                        hypothesis = c("><", ">", "<"),
                        standardize = FALSE, ...){
  
  hypothesis <- match.arg(hypothesis)
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- x$df.residual
  
  if(standardize){
    x <- arm::standardize(x, ...)
  }
  summ <- data.frame(summary(x)$coefficients)
  tc <- qt(alpha, df)
  bc <- tc * summ[, 2]
  d <- NA
  dc <- NA
  x$d <- unname(d)
  x$bc <- unname(bc)
  x$dc <- unname(dc)
  class(x) <- c("critvalue", "lm")
  return(x)
}

#' @export
print.critvalue <- function(x, digits = getOption("digits"), ...){
  NextMethod(x, ...)
  # TODO vedi se c'è un modo di fare un metodo con due classi
  # nested in modo da fare critvalue/ttest e critvalue/htest
  x <- .round_list(x, digits)
  if(inherits(x, "ttest")){
    cat("|== Effect Size and Critical Value ==|", "\n")
    cat("d =", x$d, "|dc| =", abs(x$dc), "|bc| =", abs(x$bc),"\n")
    cat("g =", x$g, "|gc| =", abs(x$gc), "\n\n")
  }else if(inherits(x, "ctest")){
    cat("|== Critical Value ==|", "\n")
    cat("|rc| =", abs(x$dc),"\n\n")
  }
  invisible(x)
}

#' @export
summary.critvalue <- function(x, ...){
  NextMethod(x, ...)
  # taken from https://github.com/cran/lm.beta/blob/master/R/summary.lm.beta.R
  x2 <- x
  attr(x2, "class") <- "lm"
  x.summary <- summary(x2, ...)
  x.summary$coefficients <- cbind(x.summary$coefficients[, 
                                                         1, drop = F], `|Critical Estimate|` = abs(x$bc), 
                                  x.summary$coefficients[, -1, drop = F])
  class(x.summary) <- c("summary.critvalue", class(x.summary))
  x.summary
}


