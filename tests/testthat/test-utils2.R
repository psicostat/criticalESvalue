# cor test, t, conf.level = 0.95 -------------------------------

test_that("critical value for correlation works (t, two sided)", {
  n <- 40
  rr <- critical_cortest(n, test = "t")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- p_value(t, rr$df, test = "t", hypothesis = "><")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (t, greater)", {
  n <- 40
  rr <- critical_cortest(n, test = "t", hypothesis = ">")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- p_value(t, rr$df, test = "t", hypothesis = ">")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (t, less)", {
  n <- 40
  rr <- critical_cortest(n, test = "t", hypothesis = "<")
  t <- rr$rc / rr$se_rc
  # should be 0.05
  pval <- p_value(t, rr$df, test = "t", hypothesis = "<")
  expect_equal(pval, 0.05)
})

# cor test, t, conf.level = 0.80 -------------------------------

test_that("critical value for correlation works (t, two sided)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "t", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval <- p_value(t, rr$df, test = "t", hypothesis = "><", conf.level = conf.level)
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (t, greater)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "t", hypothesis = ">", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval_expected <- 1 - conf.level
  pval <- p_value(t, rr$df, test = "t", hypothesis = ">", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (t, less)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "t", hypothesis = "<", conf.level = conf.level)
  t <- rr$rc / rr$se_rc
  pval_expected <- 1 - conf.level
  pval <- p_value(t, rr$df, test = "t", hypothesis = "<", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

# cor test, z, conf.level = 0.95 -------------------------------

test_that("critical value for correlation works (z, two sided)", {
  n <- 40
  rr <- critical_cortest(n, test = "z")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- p_value(z, rr$df, test = "z", hypothesis = "><")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (z, greater)", {
  n <- 40
  rr <- critical_cortest(n, test = "z", hypothesis = ">")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- p_value(z, rr$df, test = "z", hypothesis = ">")
  expect_equal(pval, 0.05)
})

test_that("critical value for correlation works (z, less)", {
  n <- 40
  rr <- critical_cortest(n, test = "z", hypothesis = "<")
  z <- atanh(rr$rc) * sqrt(n - 3)
  # should be 0.05
  pval <- p_value(z, rr$df, test = "z", hypothesis = "<")
  expect_equal(pval, 0.05)
})

# cor test, z, conf.level = 0.80 -------------------------------

test_that("critical value for correlation works (z, two sided)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "z", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- p_value(z, rr$df, test = "z", hypothesis = "><", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (z, greater)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "z", hypothesis = ">", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- p_value(z, rr$df, test = "z", hypothesis = ">", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

test_that("critical value for correlation works (z, less)", {
  n <- 40
  conf.level <- 0.8
  rr <- critical_cortest(n, test = "z", hypothesis = "<", conf.level = conf.level)
  z <- atanh(rr$rc) * sqrt(n - 3)
  pval_expected <- 1 - conf.level
  pval <- p_value(z, rr$df, test = "z", hypothesis = "<", conf.level = conf.level)
  expect_equal(pval, pval_expected)
})

# t test, conf.level = 0.95, n1 = n2, sd1 = sd2 two sided ----

test_that("critical value for t test works (two sided, n1 = n2, sd1 = sd2)", {
  n1 <- n2 <- 50
  m1 <- 150
  m2 <- 100
  sd1 <- sd2 <- 50
  conf.level <- 0.95
  tt <- critical_ttest(m1, m2, sd1, sd2, n1, n2, var.equal = TRUE)
  pval <- p_value(tt$bc / tt$se, tt$df)
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})

test_that("critical value for welch t test works (two sided, n1 = n2, sd1 = sd2)", {
  n1 <- n2 <- 50
  m1 <- 150
  m2 <- 100
  sd1 <- sd2 <- 50
  conf.level <- 0.95
  tt <- critical_ttest(m1, m2, sd1, sd2, n1, n2, var.equal = FALSE)
  pval <- p_value(tt$bc / tt$se, tt$df)
  pval_expected <- 1 - conf.level
  expect_equal(pval, pval_expected)
})




