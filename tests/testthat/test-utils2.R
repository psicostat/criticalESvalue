# cor test, t, conf.level = 0.95 -------------------------------

test_that("critical value for correlation works (t, two sided)", {
  n <- 40
  rr <- critical_cor(n = n, test = "t")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- .p_value(q = t, df = rr$df, test = "t", hypothesis = "2t")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (t, greater)", {
  n <- 40
  rr <- critical_cor(n = n, test = "t", hypothesis = "greater")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- .p_value(t, rr$df, test = "t", hypothesis = "1t")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (t, less)", {
  n <- 40
  rr <- critical_cor(n = n, test = "t", hypothesis = "less")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- .p_value(t, rr$df, test = "t", hypothesis = "1t")
  expect_equal(pval, 0.05)
})

# cor test, t, conf.level = 0.80 -------------------------------

test_that("critical value for correlation works (t, two sided)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "t", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval <- .p_value(t, rr$df, test = "t", hypothesis = "2t", conf.level = conf.level)
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (t, greater)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "t", hypothesis = "greater", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval_expected <- 1 - conf.level
  pval <- .p_value(t, rr$df, test = "t", hypothesis = "1t", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (t, less)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "t", hypothesis = "less", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval_expected <- 1 - conf.level
  pval <- .p_value(t, rr$df, test = "t", hypothesis = "1t", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

# cor test, z, conf.level = 0.95 -------------------------------

test_that("critical value for correlation works (z, two sided)", {
  n <- 40
  rr <- critical_cor(n = n, test = "z")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "2t")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (z, greater)", {
  n <- 40
  rr <- critical_cor(n = n, test = "z", hypothesis = "greater")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "1t")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (z, less)", {
  n <- 40
  rr <- critical_cor(n = n, test = "z", hypothesis = "less")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "1t")
  expect_equal(pval, 0.05)
})

# cor test, z, conf.level = 0.80 -------------------------------

test_that("critical value for correlation works (z, two sided)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "z", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "2t", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (z, greater)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "z", hypothesis = "greater", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "1t", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (z, less)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cor(n = n, test = "z", hypothesis = "less", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- .p_value(z, rr$df, test = "z", hypothesis = "1t", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})


# two-samples t-test -------------------------------------------------------

test_that("critical value for t test works (two sided, n1 = n2, sd1 = sd2)", {
  n1 <- n2 <- 50
  m1 <- 150
  m2 <- 100
  sd1 <- sd2 <- 50
  tt95 <- critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, var.equal = TRUE,
                       conf.level = 0.95)
  tt80 <- critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, var.equal = TRUE,
                       conf.level = 0.80)
  pval95 <- .p_value(tt95$bc / tt95$se, tt95$df, hypothesis = "2t", conf.level = 0.95)
  pval80 <- .p_value(tt80$bc / tt80$se, tt80$df, hypothesis = "2t", conf.level = 0.80)
  expect_equal(pval95, 0.05)
  expect_equal(pval80, 0.20)
})

test_that("critical value for welch t test works (two sided, n1 = n2, sd1 = sd2)", {
  n1 <- n2 <- 50
  m1 <- 150
  m2 <- 100
  sd1 <- 50
  sd2 <- 30
  tt95 <- critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, var.equal = FALSE, conf.level = 0.95)
  tt80 <- critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, var.equal = FALSE, conf.level = 0.80)
  pval95 <- .p_value(tt95$bc / tt95$se, tt95$df, hypothesis = "2t", conf.level = 0.95)
  pval80 <- .p_value(tt80$bc / tt80$se, tt80$df, hypothesis = "2t", conf.level = 0.80)
  expect_equal(pval95, 0.05)
  expect_equal(pval80, 0.2)
})

# one sample t-test -------------------------------------------------------

test_that("critical value for the one sample t-test (two sided)", {
  n <- 50
  m <- 100
  s <- 80
  tt95 <- critical_t1s(m = m, s = s, n = n, hypothesis = "two.sided", conf.level = 0.95)
  tt95 <- critical_t1s(m = m, s = s, n = n, hypothesis = "two.sided", conf.level = 0.95)
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "2t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for the one sample t-test (greater)", {
  n <- 50
  m <- 100
  s <- 80
  conf.level <- 0.95
  tt <- critical_t1s(m = m, s = s, n = n, hypothesis = "greater")
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "1t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for the one sample t-test (less)", {
  n <- 50
  m <- 100
  s <- 80
  conf.level <- 0.95
  tt <- critical_t1s(m = m, s = s, n = n, hypothesis = "less")
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "1t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for the paired sample t test (two sided)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  tt <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, hypothesis = "two.sided",
                      conf.level = conf.level)
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "2t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for the paired sample t test (greater)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  tt <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, hypothesis = "greater",
                      conf.level = conf.level)
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "1t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for the paired sample t test (less)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  tt <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, hypothesis = "less",
                      conf.level = conf.level)
  pval <- .p_value(tt$bc / tt$se, tt$df, hypothesis = "1t")
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})


# comparing from_t and from_data ------------------------------------------

test_that("critical value for the paired sample t test, same results from data and from t (two sided)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  hypothesis <- "two.sided"
  
  sp <- sqrt((sd1^2 + sd2^2) / 2)
  sdiff <- sp * sqrt(2 * (1 - r12))
  se <- sdiff / sqrt(n)
  t <- (m1 - m2) / se
  
  tt_from_data <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, conf.level = conf.level, hypothesis = hypothesis)
  tt_from_t <- critical_t2sp(t = t, n = n, se = se, r12 = r12, conf.level = conf.level, hypothesis = hypothesis)
  
  cm <- intersect(names(tt_from_data), names(tt_from_t))
  expect_equal(tt_from_data[cm], tt_from_t[cm])
})

test_that("critical value for the paired sample t test, same results from data and from t (greater)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  hypothesis <- "greater"
  
  sp <- sqrt((sd1^2 + sd2^2) / 2)
  sdiff <- sp * sqrt(2 * (1 - r12))
  se <- sdiff / sqrt(n)
  t <- (m1 - m2) / se
  
  tt_from_data <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, conf.level = conf.level)
  tt_from_t <- critical_t2sp(t = t, n = n, se = se, r12 = r12, conf.level = conf.level)
  
  cm <- intersect(names(tt_from_data), names(tt_from_t))
  expect_equal(tt_from_data[cm], tt_from_t[cm])
})

test_that("critical value for the paired sample t test, same results from data and from t (less)", {
  m1 <- 100
  m2 <- 90
  sd1 <- 50
  sd2 <- 50
  n <- 40
  r12 <- 0.6
  conf.level <- 0.95
  hypothesis <- "less"
  
  sp <- sqrt((sd1^2 + sd2^2) / 2)
  sdiff <- sp * sqrt(2 * (1 - r12))
  se <- sdiff / sqrt(n)
  t <- (m1 - m2) / se
  
  tt_from_data <- critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, conf.level = conf.level)
  tt_from_t <- critical_t2sp(t = t, n = n, se = se, r12 = r12, conf.level = conf.level)
  
  cm <- intersect(names(tt_from_data), names(tt_from_t))
  expect_equal(tt_from_data[cm], tt_from_t[cm])
})
