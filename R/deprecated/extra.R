zscore <- function(x, s = 1, center = TRUE, scale = TRUE, na.rm = FALSE){
  m <- mean(x, na.rm = na.rm)
  sd <- sd(x, na.rm = na.rm)
  if(center){
    x <- x - m
  }
  if(scale){
    x <- x / (sd*s)
  }
  return(x)
}

.weighted.sd <- function(x, w){
  n <- length(x)
  sqrt(sum(((x - weighted.mean(x, w))*w)^2)/(n - 1))
}

standardize <- function(x, stand.y = TRUE, stand.x = TRUE){
  
  Y <- model.frame(x)[, 1, drop = FALSE]
  X <- model.frame(x)[, -1, drop = FALSE]
  
  numIDX <- sapply(X, is.numeric)
  
  if(stand.y){
    Y <- scale(Y)
  }
  if(stand.x){
    for(i in 1:ncol(X)){
      if(is.numeric(X[, i])){
        X[, i] <- scale(X[, i])
      }
    }
  }
  D <- cbind(Y, X)
  update(x, data = D)
  
}

old_standardize <- function(x, 
                        stand.x = TRUE, 
                        stand.y = FALSE,
                        nsd = 1){
  Y <- model.frame(x)[, 1]
  X <- model.matrix(x)
  if(exists("weights", x)){
    w <- x$weights
  }else{
    w <- rep(1, nrow(x$model))
  }
  
  sY <- .weighted.sd(Y, w)
  sX <- apply(X, 2, .weighted.sd, w)
  
  sY <- sY * nsd
  sX <- sX * nsd
  
  B <- coef(x)
  SE <- summary(x)$coefficients[, 2]
  
  if(stand.x){
    B <- B * sX
    SE <- SE * sX 
  }
  if(stand.y){
    B <- B/sY
    SE <- SE/sY
  }
  x$standardized.coefficients <- B
  x$standardized.se <- SE
  return(x)
}