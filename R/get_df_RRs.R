#' Create a Dataframe from Reduced Predictions
#'
#' This function extracts the predicted exposure response curve and its associated
#' confidence bands from a DLNM prediction. This function accepts a `crossreduce`
#' or `crosspred` object and, depending on the model link used in the prediction,
#' will return values associated with the RR or output.
#'
#' @param pred An object of class "`crossreduce`" or "`crosspred`".
#'
#' @returns A tibble with with the following columns:
#'  - `exposure`: The values of the predictor variable.
#'  - `RR`: The predicted risk, rate, or odds ratio from `crossreduce`.
#'  - `RR_low`, `RR_high`: The lower/upper bound of the confidence bands for `RR`. Depends on the `ci.level` argument in `crossreduce`.
#'  - `fit`: The predicted output from `crossreduce`.
#'  - `low`, `high`: The lower/upper bound of the confidence bands for `fit`. Depends on the `ci.level` argument in `crossreduce`.
#'
#' @export
#'
#' @examples
#'
#' library(dlnm)
#' library(ggplot2)
#' # The NYC airquality sample dataset
#' head(airquality, 10)
#'
#' # Arguments to specify cross-basis
#' arg_var <- list(fun = "ns", knots = quantile(airquality$Temp, c(.1, .75, .9)))
#' arg_lag <- list(fun = "ns", knots = logknots(7, nk = 3))
#'
#' # Cross-basis and model fit
#' cb <- crossbasis(airquality$Temp, lag = 7, argvar = arg_var, arglag = arg_lag)
#' fit1 <- glm(Ozone ~ cb, family = "gaussian",     data = airquality)
#' fit2 <- glm(Ozone ~ cb, family = "quasipoisson", data = airquality)
#'
#' # Prediction and extracted RRs (or output)
#' pred1 <- crossreduce(cb, fit1)
#' df_RR1 <- get_df_RRs(pred1)
#'
#' pred2 <- crossreduce(cb, fit2)
#' df_RR2 <- get_df_RRs(pred2)
#'
#' # Plots with ggplot2
#' df_RR1 |>
#'   ggplot() +
#'   geom_line(aes(exposure, fit)) +
#'   geom_ribbon(aes(exposure, ymin = low, ymax = high), alpha = .2)
#'
#' df_RR2 |>
#'   ggplot() +
#'   geom_line(aes(exposure, RR)) +
#'   geom_ribbon(aes(exposure, ymin = RR_low, ymax = RR_high), alpha = .2)

get_df_RRs <- function(pred) {
  tibble::tibble(exposure = pred$predvar,
                 RR = pred$RRfit,
                 RR_low = pred$RRlow,
                 RR_high = pred$RRhigh,
                 RR = pred$allRRfit,
                 RR_low = pred$allRRlow,
                 RR_high = pred$allRRhigh,
                 fit = pred$fit,
                 low = pred$low,
                 high = pred$high)
}


