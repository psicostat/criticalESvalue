.p_value <- function(q,
                 df = NULL,
                 test = c("t", "z"),
                 conf.level = 0.95,
                 hypothesis = c("1t", "2t")){
  hypothesis <- match.arg(hypothesis)
  test <- match.arg(test)
  # two tails
  if(test == "t"){
    p <- 2 * pt(-abs(q), df)
  }else{
    p <- 2 * pnorm(-abs(q))
  }
  if(hypothesis == "1t"){
    p <- p / 2
  }
  return(p)
}

.get_alpha <- function(conf.level = 0.95,
                      hypothesis = c("2t", "1t")){
  hypothesis <- if(hypothesis == "two.sided") "2t" else "1t"
  alpha <- 1 - conf.level
  if(hypothesis == "2t"){
    alpha <- alpha / 2
  }
  return(alpha)
}

.get_J <- function(df){
  # taken from https://github.com/easystats/effectsize/blob/a579a5093549be2edc264e2d1da95726ff4f4369/R/cohens_d.R#L315
  exp(lgamma(df/2) - log(sqrt(df/2)) - lgamma((df - 1)/2))
}

.round_list <- function(x, digits = getOption("digits")){
  out <- lapply(x, function(el){
    if(is.numeric(el)){
      round(el, digits)
    } else{
      el
    }
  })
  class(out) <- class(x)
  out
}

.hyp_sign <- function(x, hypothesis){
  x <- abs(x)
  ifelse(hypothesis %in% c("two.sided", "greater"), x, -x) 
}