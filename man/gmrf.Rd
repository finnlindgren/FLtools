\name{covariance.ar1}
\alias{precision.ar1}
\title{Gaussian Markov random field tools}
\usage{
covariance.ar1(n, a, sd=1)
precision.ar1(n, a, sd=1)
}
\arguments{
  \item{n}{The number of nodes}
  \item{a}{The AR model dependence parameter (x[t] = a * x[t-1] +
    eps[t])}
  \item{sd}{The marginal standard deviation}
  }
\description{
    Construct a dense covariance or sparse precision atrix for an AR(1)
    model.
}
\author{
Finn Lindgren
}
