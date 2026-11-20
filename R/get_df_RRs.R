#' Create a Dataframe from DLNM Predictions
#'
#' These functions extract the predicted exposure response curve and its associated
#' confidence bands from a DLNM prediction.
#'
#' The `get_df_RRs()` function accepts a `crossreduce` or `crosspred` object and will return the estimated means, lower bounds, and upper bounds for the supplied values of the predictor variable, cumulated across all lags.
#' The `get_df_lag_RRs()` function accepts a `crosspred` object and returns the estimated matrix of means, lower bounds, and upper bounds for all predictor variable and lag combinations.
#' The `crosspred_to_df()` function acts as an alias for `get_df_RRs()` when `pred_type == "cumulative"` or `get_df_lag_RRs()` when `pred_type == "lagged"`.
#'
#' @param pred An object of class "`crosspred`" or "`crossreduce`".
#' @param pred_type A string, either "cumulative" or "lagged" specifying if the cumulated values or full matrix values should be returned.
#'
#' @returns A tibble with with the following columns:
#'  - `exposure`: The values of the predictor variable.
#'  - `fit`, `RR`, `OR`: The predicted risk, rate, odds ratio or fitted value. Depends on the value of `model.link` argument in the `crosspred` or `crossreduce` object.
#'  - `low`, `RR_low`, `OR_low`, `high`, `RR_high`, `OR_high`: The lower/upper bound of the confidence bands. Depends on the `ci.level` argument in the `crossreduce` object.
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
#' df_RR1 <- crosspred_to_df(pred1)
#'
#' pred2 <- crosspred(cb, fit2)
#' df_RR2 <- crosspred_to_df(pred2)
#'
#' df_RR3 <- crosspred_to_df(pred2, pred_type = "lagged")
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
#'
#' df_RR3 |>
#'   ggplot() +
#'   geom_line(aes(exposure, RR)) +
#'   geom_ribbon(aes(exposure, ymin = RR_low, ymax = RR_high), alpha = .2) +
#'   facet_wrap(~ lag, labeller = as_labeller(\(x) paste("Lag", x)))
#'

get_df_RRs <- function(pred) {
  # The base names of the variables to extract from pred
  base_names <- c("fit", "low", "high")

  # Confirm crosspred is of correct object type and extract the fitted values
  if (class(pred) == "crossreduce") {
    var_mod1 <- ""
  } else if (class(pred) == "crosspred") {
    var_mod1 <- "all"
  } else {
    stop("Must submit object of class crosspred or crossreduce.")
  }

  # Create returned col_names and modify var_Names by model.link
  if (pred$model.link == "log") {
    var_mod2 <- "RR"
    col_names <- c("RR", "RR_low", "RR_high")
  } else if (pred$model.link == "logit") {
    var_mod2 <- "RR"
    col_names <- c("OR", "OR_low", "OR_high")
  } else {
    var_mod2 <- ""
    col_names <- c("fit", "low", "high")
  }

  # Names to extract from crosspred
  var_names <- paste0(var_mod1, var_mod2, base_names)

  # Returned df
  tibble::tibble(
    exposure = pred[["predvar"]],
    !!col_names[1] := pred[[var_names[1]]],
    !!col_names[2] := pred[[var_names[2]]],
    !!col_names[3] := pred[[var_names[3]]]
  )
}

#' @rdname get_df_RRs
#' @export

get_df_lag_RRs <- function(pred) {
  # The base names of the variables to extract from pred
  base_names <- c("fit", "low", "high")

  # Confirm crosspred is of correct object type and extract the fitted values
  if (class(pred) != "crosspred") {
    stop("Must submit object of class crosspred.")
  }
  var_mod1 <- "mat"

  # Create returned col_names and modify var_Names by model.link
  if (pred$model.link == "log") {
    var_mod2 <- "RR"
    col_names <- c("RR", "RR_low", "RR_high")
  } else if (pred$model.link == "logit") {
    var_mod2 <- "RR"
    col_names <- c("OR", "OR_low", "OR_high")
  } else {
    var_mod2 <- ""
    col_names <- c("fit", "low", "high")
  }

  # Names to extract from crosspred
  var_names <- paste0(var_mod1, var_mod2, base_names)

  # Extract RR in matrix form for mean, lower, and upper bounds
  list_df <- Map(
    \(pred_mat, col_name) {
      pred_mat |>
        data.frame() |>
        dplyr::mutate(exposure = pred$predvar, .before = "lag0") |>
        tidyr::pivot_longer(
          dplyr::contains("lag"),
          values_to = col_name,
          names_to = "lag"
        ) |>
        dplyr::mutate(lag = as.integer(sub("lag", "", lag)))
    },
    # Lists
    list(pred[[var_names[1]]], pred[[var_names[2]]], pred[[var_names[3]]]),
    list(col_names[1], col_names[2], col_names[3])
  )

  # Return single df of matrix of fitted, lower, and upper bounds
  Reduce(\(x, y) dplyr::left_join(x, y, by = c("exposure", "lag")), list_df)
}

#' @rdname get_df_RRs
#' @export

crosspred_to_df <- function(pred, pred_type = "cumulative") {
  if (pred_type == "cumulative") {
    get_df_RRs(pred)
  } else if (pred_type == "lagged") {
    get_df_lag_RRs(pred)
  } else {
    stop('pred_type must be either "cumulative" or "lagged"')
  }
}
