#' Get the MMT
#'
#' When making predictions with distributed lag nonlinear models, this function
#' retrieves the value of the exposure that corresponds to the minimum value of
#' the response. These models are commonly used to predict mortality as a function
#' of temperature, for which this function returns the minimum mortality temperature
#' (MMT).
#'
#' @param crosspred An object of class "`crosspred`" or "`crossreduce`".
#' @param limits  A numeric vector of length two that sets the lower and upper limit returned.
#'
#' @returns The numeric value of the exposure corresponding to the minimum value of the response.
#'
#' @rdname get_MMT
#' @export
#'
#' @examples
#'
#' library(dlnm)
#' # The NYC airquality sample dataset
#' head(airquality, 10)
#'
#' # Arguments to specify cross-basis
#' arg_var <- list(fun = "ns", knots = quantile(airquality$Temp, c(.1, .75, .9)))
#' arg_lag <- list(fun = "ns", knots = logknots(7, nk = 3))
#'
#' # Cross-basis and model fit
#' cb <- crossbasis(airquality$Temp, lag = 7, argvar = arg_var, arglag = arg_lag)
#' fit <- glm(Ozone ~ cb, family = "quasipoisson", data = airquality)
#'
#' # Prediction with default centering
#' pred <- crosspred(cb, fit)
#' plot(pred, "overall")
#'
#' # Prediction centering at the MMT
#' pred_centered <- crosspred(cb, fit, cen = get_MMT(pred))
#' plot(pred_centered, "overall")
get_MMT <- function(crosspred, limits = NULL) {
  # Confirm crosspred is of correct object type and extract the fitted values
  if (class(crosspred) == "crossreduce") {
    fit <- crosspred$fit
  } else if (class(crosspred) == "crosspred") {
    fit <- crosspred$allfit
  } else {
    return(print("Must submit object of class crosspred or crossreduce."))
  }

  # Get limits for the predictor variable
  if (is.null(limits)) {
    limits <- c(-Inf, Inf)
  } else if (is.numeric(limits) & length(limits) == 2) {
    limits
  } else {
    return(print("limits must be a numeric vector of length 2 or NULL"))
  }

  # Create a new df of the predictor variable and associated response
  df_pred <-
    data.frame(
      predvar = crosspred$predvar,
      fit = fit
    ) |>
    dplyr::filter(dplyr::between(predvar, limits[1], limits[2]))

  # Return the value of the predictor corresponding to the minimum prediction
  df_pred$predvar[which.min(df_pred$fit)]
}

#' @rdname get_MMT
#' @export

get_cen <- function(crosspred) {
  get_MMT(crosspred)
}
