covariance.ar1 <- function(n, a, sd=1) {
  if (n < 1) {
    stop(paste("The number of nodes must be at least 1, not", n))
  }
  if (n == 1) {
    S <- matrix(sd^2, 1, 1)
  } else {
    D <- as.matrix(dist(0:(n-1)))
    S <- sd^2 * a^D
  }
  S
}

precision.ar1 <- function(n, a, sd=1) {
  if (n < 1) {
    stop(paste("The number of nodes must be at least 1, not", n))
  }
  if (n == 1) {
    Q <- Matrix::Diagonal(n=1, x=1/sd^2)
  } else {
    Q <- Matrix::sparseMatrix(i=c(1:n, 2:n, 1:(n-1)),
                              j=c(1:n, 1:(n-1), 2:n),
                              x=rep(c(1, 1+a^2, 1, -a), c(1, n-2, 1, 2*(n-1))) / (1 - a^2) / sd^2,
                              dims=c(n, n))
  }
  Q
}
