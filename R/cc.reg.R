cc.reg <- function(y, tol = 1e-7) {

  n <- length(y)
  x <- model.matrix( y~., data = as.data.frame(x) )

  fun <- function(par, y, x, n) {
    lambda <- exp(par[1])
    mu <- x %*% par[-1]
    - n * log( tanh(lambda * pi) ) - n * log(lambda) + sum( log( lambda^2 + (y - mu)^2) )
  }
  s <- 0.5 * abs( Rfast::nth(y, 3 * n/4) - Rfast::nth(y, n/4) )
  logs <- log(s)
  be <- Rfast::lmfit(x, y)$be
  mod <- optim( c(logs, be), y = y, x = x, n = n, fun, control = list(maxit = 5000) )
  mod <- optim(mod$par, y = y, x = x, n = n, fun, control = list(maxit = 5000) )

  lambda <- exp(mod$par[1])
  be <- mod$par[-1]
  names(be) <- colnames(x)
  list(lambda = lambda, be = be, loglik = -mod$value - n * log(pi))
}
