#' Calculate QAIC and QBIC
#'
#' This function calculates quasi-Akaike information criterion (QAIC) or the quasi-Bayesian
#' information criterion (QBIC) for quasi-Poisson regression as defined in Gasparrini,
#' Armstrong, and Kenward (2010). When comparing multiple models, the "best" model
#' is that which minimizes these criteria.
#'
#' The equations used to calculate the QAIC and QBIC are \eqn{\text{QAIC} = -2L(\hat{\boldsymbol{\theta}}) + 2\hat{\phi}k},
#' and \eqn{\text{QBIC} = -2L(\hat{\boldsymbol{\theta}}) + 2\hat{\phi}k}, where
#' \eqn{L} is the log-likelihood of the fitted model with parameters \eqn{\hat{\boldsymbol{\theta}}},
#' \eqn{\hat{\phi}} is the overdispersion parameter, \eqn{k} is the number of parameters,
#' and \eqn{n} is the number of observations.
#'
#' @references Gasparrini, A., B. Armstrong, and M. G. Kenward. “Distributed Lag Non‐linear Models.” *Statistics in Medicine* 29, no. 21 (2010): 2224–34. https://doi.org/10.1002/sim.3940.

#'
#' @rdname QAIC
#'
#' @param model A model object inheriting from the "`glm`" class, whose family parameter was specified as `quasipoisson`.
#'
#' @returns Returns a number, either the QAIC or QBIC.
#' @export
#'
#' @examples
#' fit <- glm(hp ~ mpg + disp + wt, family = "quasipoisson", data = mtcars)
#' AIC(fit)
#' QAIC(fit)

QAIC <- function(model) {
  L_hat   <- sum(stats::dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)

  -2*L_hat+2*phi_hat*k
}

#' @rdname QAIC
#' @export
QBIC <- function(model) {
  L_hat   <- sum(stats::dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)
  n       <- nrow(model$data)

  -2*L_hat+log(n)*phi_hat*k
}
