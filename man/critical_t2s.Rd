% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criticalvalue-funs.R
\name{critical_t2s}
\alias{critical_t2s}
\title{critical_t2s}
\usage{
critical_t2s(
  m1 = NULL,
  m2 = NULL,
  t = NULL,
  sd1 = NULL,
  sd2 = NULL,
  n1,
  n2,
  se = NULL,
  df = NULL,
  var.equal = FALSE,
  hypothesis = c("two.sided", "greater", "less"),
  conf.level = 0.95
)
}
\arguments{
\item{m1}{a number representing the mean of group 1.}

\item{m2}{a number representing the mean of group 2.}

\item{t}{the t value.}

\item{sd1}{a number representing the standard deviation of group 1.}

\item{sd2}{a number representing the standard deviation of group 2.}

\item{n1}{a number corresponding to the sample size of group 1.}

\item{n2}{a number corresponding to the sample size of group 2.}

\item{se}{a number corresponding to the standard error.}

\item{df}{degrees of freedom.}

\item{var.equal}{a logical variable indicating whether to treat the two variances as being equal.}

\item{hypothesis}{a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").}

\item{conf.level}{the confidence level to set the confidence interval, default is set to 0.95.}
}
\value{
the output returns a \code{d} which is the Cohen's d, the critical d (\code{dc}) which is the minimum value for which to get a significant result with a given sample, the \code{bc} is the numerator of the formula from which the d is calculated, \code{se} which is the standard error, \code{df} are the degrees of freedom, then it also gives the \code{g} and \code{gc} which are respectively \code{d} and \code{dc} with Hedfer's Correction for small samples.
}
\description{
The function allows to calculate cohen's d and critical d for a two samples t-test.
}
\examples{
# critical value from summary statistics
m1 <- 0.5
m2 <- 1.0
sd1 <- 1
sd2 <- 1.5
n1 <- 30
n2 <- 35
critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2)
# critical value from the t statistic
se <- sqrt(sd1^2 / n1 + sd2^2 / n2)
t <- (m1 - m2) / se
critical_t2s(t = t, n1 = n1, n2 = n2, se = se) # se only required for calculating bc
}
