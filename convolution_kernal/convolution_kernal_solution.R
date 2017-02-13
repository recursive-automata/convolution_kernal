


convolution_kernal <- function(x, y, w, t, tau) {
  z <- as.matrix(sapply(tau, function(tau) sum(x[w] * y[w + tau])))
  k <- outer(tau, t, FUN = Vectorize(function(tau, t) sum(x[w] * x[w - t + tau])))
  solve(t(k) %*% k, t(k) %*% z)
}

