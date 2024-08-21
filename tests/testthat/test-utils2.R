# COR TEST ----------------------------------------------------------------

## is the critical value correct? ---------------------------------------

# the p value calculated with the critical value should be equal to alpha

test_that("correlation test (grid conditions) critical value produce alpha", {
  
  conds <- expand.grid(
    r = 0,
    n = c(10, 50),
    hypothesis = c("two.sided", "greater", "less"),
    alpha = c(0.001, 0.05, 0.1),
    test = c("t", "z"),
    stringsAsFactors = FALSE
  )
  
  conds$conf.level <- 1 - conds$alpha
  conds$tail <- ifelse(conds$hypothesis == "two.sided", "2t", "1t")
  
  rr <- vector(mode = "list", length = nrow(conds))
  
  for(i in 1:nrow(conds)){
    ii <- with(conds[i, ], 
               critical_cor(r = r, n = n, conf.level = conf.level, test = test,
                            hypothesis = hypothesis))
    if(conds$test[i] == "t"){
      q <- ii$rc/ii$se_rc
    } else{
      q <- atanh(ii$rc) * sqrt(conds$n[i] - 3)
    }
    ii$pval <- .p_value(q, df = ii$df, 
                        test = conds$test[i], 
                        conf.level = conds$conf.level[i],
                        hypothesis = conds$tail[i])
    rr[[i]] <- data.frame(ii)
  }
  
  rr <- do.call(rbind, rr)
  rr <- cbind(conds, rr)
  expect_equal(rr$pval, rr$alpha)
})


# T-TEST -----------------------------------------------------------------

## is the critical value correct? ---------------------------------------

# the p value calculated with the critical value should be equal to alpha


### two-sample t-test -----------------------------------------------------

test_that("two sample t-test (grid conditions) critical value produce alpha", {
  conds <- expand.grid(
    m1 = 0,
    m2 = 0,
    sd1 = 1,
    sd2 = c(0.5, 1, 2),
    n1 = c(10, 50),
    n2 = c(10, 50),
    hypothesis = c("two.sided", "greater", "less"),
    alpha = c(0.001, 0.05, 0.1),
    var.equal = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  conds$conf.level <- 1 - conds$alpha
  conds$tail <- ifelse(conds$hypothesis == "two.sided", "2t", "1t")
  
  t2s <- vector(mode = "list", length = nrow(conds))
  
  for(i in 1:nrow(conds)){
    ii <- with(conds[i, ], 
               critical_t2s(m1 = m1, m2 = m2, 
                            sd1 = sd1, sd2 = sd2, 
                            n1 = n1, n2 = n2,
                            var.equal = var.equal,
                            conf.level = conf.level,
                            hypothesis = hypothesis)
    )
    ii$pval <- .p_value(ii$bc/ii$se, ii$df, conf.level = conds$conf.level[i],
                        hypothesis = conds$tail[i])
    t2s[[i]] <- data.frame(ii)
  }
  
  t2s <- do.call(rbind, t2s)
  t2s <- cbind(conds, t2s)
  
  expect_equal(t2s$pval, t2s$alpha)
})

### one-sample t-test -----------------------------------------------------

test_that("one sample t-test (grid conditions) critical value produce alpha", {
  conds <- expand.grid(
    m = 0,
    s = c(0.5, 1, 2),
    n = c(10, 50),
    hypothesis = c("two.sided", "greater", "less"),
    alpha = c(0.001, 0.05, 0.1),
    stringsAsFactors = FALSE
  )
  
  conds$conf.level <- 1 - conds$alpha
  conds$tail <- ifelse(conds$hypothesis == "two.sided", "2t", "1t")
  
  t1s <- vector(mode = "list", length = nrow(conds))
  
  for(i in 1:nrow(conds)){
    ii <- with(conds[i, ], 
               critical_t1s(m = m, s = s, 
                            n = n, conf.level = conf.level,
                            hypothesis = hypothesis)
    )
    ii$pval <- .p_value(ii$bc/ii$se, ii$df, conf.level = conds$conf.level[i],
                        hypothesis = conds$tail[i])
    t1s[[i]] <- data.frame(ii)
  }
  
  t1s <- do.call(rbind, t1s)
  t1s <- cbind(conds, t1s)
  
  expect_equal(t1s$pval, t1s$alpha)
})

### paired-sample t-test --------------------------------------------------

test_that("paired sample t-test (grid conditions) critical value produce alpha", {
  # paired-sample t-test critical value conditions -----------------------------
  
  conds <- expand.grid(
    m1 = 0,
    m2 = 0,
    sd1 = 1,
    sd2 = c(0.5, 1, 2),
    n = c(10, 50),
    hypothesis = c("two.sided", "greater", "less"),
    alpha = c(0.001, 0.05, 0.1),
    r12 = c(0, 0.5, 0.8),
    stringsAsFactors = FALSE
  )
  
  conds$conf.level <- 1 - conds$alpha
  conds$tail <- ifelse(conds$hypothesis == "two.sided", "2t", "1t")
  
  t2sp <- vector(mode = "list", length = nrow(conds))
  
  for(i in 1:nrow(conds)){
    ii <- with(conds[i, ], 
               critical_t2sp(m1 = m1, m2 = m2, 
                             sd1 = sd1, sd2 = sd2, 
                             n = n,
                             r12 = r12,
                             conf.level = conf.level,
                             hypothesis = hypothesis)
    )
    ii$pval <- .p_value(ii$bc/ii$se, ii$df, conf.level = conds$conf.level[i],
                        hypothesis = conds$tail[i])
    t2sp[[i]] <- data.frame(ii)
  }
  
  t2sp <- do.call(rbind, t2sp)
  t2sp <- cbind(conds, t2sp)
  
  expect_equal(t2sp$pval, t2sp$alpha)
})


## is the d and g correct? -----------------------------------------------

### two-samples t-test ---------------------------------------------------


#### two-sample t-test, var equal = TRUE, n1 = n2 ---------------------

test_that("d and g for two-sample t-test, var.equal = TRUE, n1 = n2", {
  
  m1 <- 0.5
  m2 <- 0
  sd1 <- 1
  sd2 <- 1
  n1 <- 30
  n2 <- 30
  
  var.equal <- TRUE
  
  s <- .get_s(sd1, sd2, n1, n2, var.equal = var.equal)
  d <- (m1 - m2) / s
  df <- (n1 + n2 - 2)
  g <- d * .get_J(df)
  
  dat <- .sim_t_data(m1, m2, sd1, sd2, n1, n2, "2s", empirical = TRUE)
  tt <- t.test(dat$y[dat$x == 0], dat$y[dat$x == 1], var.equal = var.equal)
  tt_t <- critical_t2s(t = tt$statistic, 
                       n1 = n1, n2 = n2,
                       se = tt$stderr,
                       var.equal = var.equal)
  tt_data <- critical_t2s(m1 = m1, m2 = m2, 
                          sd1 = sd1, sd2 = sd2,
                          n1 = n1, n2 = n2, 
                          var.equal = var.equal)
  
  # critical() use by default crit_from_t_*, thus we tested also t_from_data_*
  expect_equal(tt_t$d, d, info = "d is calculated correctly using critical_t2s() with data")
  expect_equal(tt_t$g, g, info = "g is calculated correctly using critical_t2s() with data")
  expect_equal(tt_data$d, d, info = "d is calculated correctly using critical_t2s() with t")
  expect_equal(tt_data$g, g, info = "g is calculated correctly using critical_t2s() with t")
})

#### two-sample t-test, var equal = FALSE, n1 != n2 ----------------------

test_that("d and g for two-sample t-test, var.equal = FALSE, n1 != n2", {
  
  m1 <- 0.5
  m2 <- 0
  sd1 <- 1
  sd2 <- 5
  n1 <- 30
  n2 <- 100
  
  var.equal <- FALSE
  
  s <- .get_s(sd1, sd2, n1, n2, var.equal = var.equal)
  se1 <- sd1/sqrt(n1)
  se2 <- sd2/sqrt(n2)
  se <- sqrt(se1^2 + se2^2)
  d <- (m1 - m2) / s
  df <- se^4/(se1^4/(n1-1) + se2^4/(n2-1))
  g <- d * .get_J(df)
  
  dat <- .sim_t_data(m1, m2, sd1, sd2, n1, n2, "2s", empirical = TRUE)
  tt <- t.test(dat$y[dat$x == 0], dat$y[dat$x == 1], var.equal = var.equal)
  
  # we are not testing the *_from_t method because we cannot compute the effect size
  # bacause it assumes that the variance are the same
  
  tt_data <- critical_t2s(m1 = m1, m2 = m2, 
                          sd1 = sd1, sd2 = sd2,
                          n1 = n1, n2 = n2, 
                          var.equal = var.equal)
  
  # critical() use by default crit_from_t_*, thus we tested also t_from_data_*
  expect_equal(tt_data$d, d, info = "d is calculated correctly using critical_t2s() with t")
  expect_equal(tt_data$g, g, info = "g is calculated correctly using critical_t2s() with t")
  
})

### one-sample t-test ----------------------------------------------

test_that("d and g for one-sample t-test", {
  m1 <- 0.5
  n1 <- 40
  sd1 <- 1
  df <- n1 - 1
  
  d <- m1 / sd1
  g <- d * .get_J(df)
  
  dat <- .sim_t_data(m1 = m1, sd1 = sd1, n1 = n1, type = "1s", empirical = TRUE)
  tt <- t.test(dat$y, mu = 0)
  tt_data <- critical_t1s(m = m1, s = sd1, n = n1)
  tt_t <- critical_t1s(t = tt$statistic, n = n1, se = tt$stderr)
  
  expect_equal(tt_t$d, d, info = "d is calculated correctly using critical_t1s() with data")
  expect_equal(tt_t$g, g, info = "g is calculated correctly using critical_t1s() with data")
  expect_equal(tt_data$d, d, info = "d is calculated correctly using critical_t1s() with t")
  expect_equal(tt_data$g, g, info = "g is calculated correctly using critical_t1s() with t")
})

### paired-sample t-test ----------------------------------------------

test_that("d and g for paired-sample t-test", {
  m1 <- 0.5
  m2 <- 0
  n1 <- 30
  sd1 <- 1
  sd2 <- 1
  df <- n1 - 1
  r12 <- 0.7
  
  sp <- .get_s(sd1 = sd1, sd2 = sd2, var.equal = FALSE)
  sd <- sp * sqrt(2 * (1 - r12))
  
  d <- (m1 - m2) / sp
  dz <- (m1 - m2) / sd
  g <- d * .get_J(df)
  gz <- dz * .get_J(df)
  
  dat <- .sim_t_data(m1 = m1, m2 = m2,
                    sd1 = sd1, sd2 = sd2,
                    n1 = n1,
                    type = "2sp",
                    r12 = r12, 
                    empirical = TRUE)
  
  tt <- t.test(dat$y[dat$x == 0], dat$y[dat$x == 1], paired = TRUE)
  tt_data <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12,
                           n = n1)
  tt_t <- critical_t2sp(t = tt$statistic, n = n1, se = tt$stderr, r12 = r12)
  
  expect_equal(tt_t$d, d, info = "d is calculated correctly using critical_t2sp() with data")
  expect_equal(tt_t$g, g, info = "g is calculated correctly using critical_t2sp() with data")
  expect_equal(tt_data$d, d, info = "d is calculated correctly using critical_t2sp() with t")
  expect_equal(tt_data$g, g, info = "g is calculated correctly using critical_t2sp() with t")
})

# LM ----------------------------------------------------------------------

## is the critical value correct? ---------------------------------------

# the p value calculated with the critical value should be equal to alpha

test_that("lm critical value", {
  fit <- lm(Sepal.Length ~ Species, data = iris)
  fit <- critical(fit)
  se <- sqrt(diag(vcov(fit)))
  pval <- .p_value(fit$bc/se, fit$df.residual, test = "t", hypothesis = "2t")
  expect_equal(unname(pval), rep(0.05, length(se)))
})
