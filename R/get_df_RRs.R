#' Create a Dataframe from Reduced Predictions
#'
#' This function extracts the predicted exposure response curve and its associated
#' confidence bands from a DLNM prediction. This function accepts a `crossreduce`
#' object and, depending on the model link used in the prediction, will return values
#' associated with the RR or output.
#'
#' @param pred An object of class "`crossreduce`".
#' @param var_name The name of the exposure variable of interest.
#'
#' @returns A tibble with with the following columns:
#'  - `Temperature`: The values of the predictor variable. Column name determined by the `var_name` argument.
#'  - `RR`: The predicted risk, rate, or odds ratio from `crossreduce`.
#'  - `RR_low`, `RR_high`: The lower/upper bound of the confidence bands for `RR`. Depends on the `ci.level` argument in `crossreduce`.
#'  - `fit`: The predicted output from `crossreduce`.
#'  - `low`, `high`: The lower/upper bound of the confidence bands for `fit`. Depends on the `ci.level` argument in `crossreduce`.
#'
#' @export
#'
#' @examples
#' # The NYC airquality sample dataset
#' help(airquality)
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
#'   geom_line(aes(Temperature, fit)) +
#'   geom_ribbon(aes(Temperature, ymin = low, ymax = high), alpha = .2)
#'
#' df_RR2 |>
#'   ggplot() +
#'   geom_line(aes(Temperature, RR)) +
#'   geom_ribbon(aes(Temperature, ymin = RR_low, ymax = RR_high), alpha = .2)

get_df_RRs <- function(pred, var_name = "Temperature") {
  tibble::tibble(!!var_name := pred$predvar,
                 RR = pred$RRfit,
                 RR_low = pred$RRlow,
                 RR_high = pred$RRhigh,
                 fit = pred$fit,
                 low = pred$low,
                 high = pred$high)
}


