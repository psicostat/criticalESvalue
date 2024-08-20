critical_ttest <- function(m1 = NULL, m2 = NULL,
                           sd1 = NULL, sd2 = NULL,
                           n1, n2,
                           r12 = NULL,
                           se = NULL,
                           sed = NULL,
                           t = NULL,
                           type = "ts",
                           var.equal = FALSE,
                           denominator = "pooled",
                           conf.level = 0.95,
                           hypothesis = c("><", "<", ">")){
  
  # check arguments with multiple options
  type <- match.arg(type, c("ts", "os", "ps"))
  denominator <- match.arg(denominator, c("pooled", "differences"))
  hypothesis <- match.arg(hypothesis)
  
  # get the alpha value for the critical value
  alpha <- .get_alpha(conf.level, hypothesis)
  
  if(type == "ts"){ # two sample t test
    # t, n1, n2 -> works but assume sd1 = sd2
    df <- n1 + n2 - 2
    if(is.null(t)){
      if(is.null(m1) | is.null(m2) | is.null(sd1) | is.null(sd2)){
        stop("When t is NULL m1, m2, sd1 and sd2 need to be provided!")
      }
      b <- m1 - m2
      if(!var.equal){ # welch
        se1 <- sd1 / sqrt(n1)
        se2 <- sd2 / sqrt(n2)
        se <- sqrt((se1^2 + se2^2)/2)
        s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1))/(n1 + n2 - 2))
        df <- se^4/(se1^4/(n1-1) + se2^4/(n2-1))
      }else{ # standard
        s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1)) / (n1 + n2 - 2))
        se <- s * (1/n1 + 1/n2)
      }
    }else{
      if(is.null(se)){
        warning("bc cannot be calculated without se, returning NA")
        s <- NA
        se <- NA
      }else{
        s <- se / (1/n1 + 1/n2)
      }
    }
  } else if(type == "os"){ # one sample
    if(is.null(t)){
      if(is.null(m1) | is.null(sd1)){
        stop("When t is NULL m1 and sd1 need to be provided!")
      }
      b <- m1
      s <- sd1
      df <- n1 - 1
      se <- s / sqrt(n1)
    }else{
      if(is.null(se)){
        warning("bc cannot be calculated without se, returning NA")
        s <- NA
        b <- NA
        se <- 1/n1 + 1/n2
      }else{
        s <- se / (1/n1)
        b <- t * se
      }
    }
  }else if(type == "ps"){
    df <- n1 - 1
    if(is.null(t)){
      # m1, m2, sd1, sd2 and r
      b <- m1 - m2
      
      if(is.null(r12) & is.null(sed)){
        warning("when r12 and sed are both NULL, bc cannot be calculated, returning NA")
        se <- NA
      }else{
        if(denominator == "pooled"){
          s <- (sd1^2 + sd2^2)/2
        }else{
          s <- sd1^2 + sd2^2 - 2*r*sd1*sd2
        }
        se <- s / sqrt(n1)
      }
      
    }else{
      if(is.null(sed)){
        warning("when t is provided and sed = NULL bc cannot be calculated, returning NA")
      }
    }
  }
  
  if(is.null(m1) | is.null(sd1)){
    stop("When t is NULL m1 and sd1 need to be provided!")
  }
  b <- m1
  s <- sd1
  df <- n1 - 1
  se <- s / sqrt(n1)
  
  # critical t
  tc <- abs(qt(alpha, df))
  
  if(type == "ts"){
    d <- b / sp
    bc <- tc * se
    dc <- bc/sp
  }else if(type == "os"){
    d <- b / sp
    bc <- tc * se
    dc <- bc / sd1
  }else{
    # paired sample
    if(denominator == "pooled"){
      d <- b / sp
      bc <- tc * se
      dc <- bc / sp
    }else{
      # dz here
      d <- b / sdiff
      bc <- tc * se
      dc <- bc / sdiff
    }
  }
  
  # correction factor for hedges's g
  J <- .get_J(df)
  
  out <- list(d = d, g = d * J, bc = bc, dc = dc, gc = dc * J, se = se, df = df)
  return(out)
}

critical_cortest <- function(n, r = NULL, 
                             test = "t", 
                             conf.level = 0.95,
                             hypothesis = c("><", "<", ">")){
  df <- n - 2
  test <- match.arg(test, c("t", "z"))
  hypothesis <- match.arg(hypothesis)
  
  alpha <- get_alpha(conf.level, hypothesis)
  sc <- if(hypothesis == "<") -1 else 1
  
  if(test == "t"){
    tc <- qt(alpha, df)
    rc <- tc / sqrt(n - 2 + tc^2)
    se_rc <- sqrt((1 - rc^2)/(n - 2))
    if(!is.null(r)){
      se_r <- sqrt((1 - r^2)/(n - 2))
    }else{
      se_r <- NA
    }
  }else{
    zc <- qnorm(alpha)
    rc <- tanh(zc / sqrt(n - 3))
    se_rc <- 1 / sqrt(n - 3)
    if(!is.null(r)){
      se_r <- se_rc
    }else{
      se_r <- NA
    }
  }
  out <- list(n = n, rc = rc, se_rc = se_rc, se_r = se_r, df = df, test = test)
  return(out)
}



   




