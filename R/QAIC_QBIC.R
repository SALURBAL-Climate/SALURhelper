QAIC <- function(model) {
  L_hat   <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)

  -2*L_hat+2*phi_hat*k
}


QBIC <- function(model) {
  L_hat   <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)
  n       <- nrow(model$data)

  -2*L_hat+log(n)*phi_hat*k
}
