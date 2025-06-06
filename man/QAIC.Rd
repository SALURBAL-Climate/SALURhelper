% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QAIC_QBIC.R
\name{QAIC}
\alias{QAIC}
\alias{QBIC}
\title{Calculate QAIC and QBIC}
\usage{
QAIC(model)

QBIC(model)
}
\arguments{
\item{model}{A model object inheriting from the "\code{glm}" class, whose family parameter was specified as \code{quasipoisson}.}
}
\value{
Returns a number, either the QAIC or QBIC.
}
\description{
This function calculates quasi-Akaike information criterion (QAIC) or the quasi-Bayesian
information criterion (QBIC) for quasi-Poisson regression as defined in Gasparrini,
Armstrong, and Kenward (2010). When comparing multiple models, the "best" model
is that which minimizes these criteria.
}
\details{
The equations used to calculate the QAIC and QBIC are \eqn{\text{QAIC} = -2L(\hat{\boldsymbol{\theta}}) + 2\hat{\phi}k},
and \eqn{\text{QBIC} = -2L(\hat{\boldsymbol{\theta}}) + 2\hat{\phi}k}, where
\eqn{L} is the log-likelihood of the fitted model with parameters \eqn{\hat{\boldsymbol{\theta}}},
\eqn{\hat{\phi}} is the overdispersion parameter, \eqn{k} is the number of parameters,
and \eqn{n} is the number of observations.
}
\examples{
fit <- glm(hp ~ mpg + disp + wt, family = "quasipoisson", data = mtcars)
AIC(fit)
QAIC(fit)
}
\references{
Gasparrini, A., B. Armstrong, and M. G. Kenward. “Distributed Lag Non‐linear Models.” \emph{Statistics in Medicine} 29, no. 21 (2010): 2224–34. https://doi.org/10.1002/sim.3940.
}
