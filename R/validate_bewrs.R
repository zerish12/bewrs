#' Validate BEWRS predictions
#'
#' Computes AUC, Brier score, calibration intercept, and calibration slope.
#'
#' @param observed Binary vector of observed outcomes, coded 0/1.
#' @param predicted Numeric vector of predicted probabilities.
#'
#' @return A data.frame containing validation metrics.
#' @export
#'
#' @examples
#' validate_bewrs(c(0, 1, 1, 0), c(0.1, 0.8, 0.7, 0.3))
validate_bewrs <- function(observed, predicted) {
  observed <- as.integer(observed)
  predicted <- as.numeric(predicted)

  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length.")
  }

  if (!all(observed %in% c(0, 1))) {
    stop("observed must be binary: 0 or 1.")
  }

  eps <- 1e-6
  predicted <- pmin(pmax(predicted, eps), 1 - eps)

  auc <- as.numeric(pROC::auc(observed, predicted))
  brier <- mean((observed - predicted)^2)

  logit_pred <- log(predicted / (1 - predicted))
  fit <- stats::glm(observed ~ logit_pred, family = stats::binomial())

  data.frame(
    auc = auc,
    brier_score = brier,
    calibration_intercept = stats::coef(fit)[1],
    calibration_slope = stats::coef(fit)[2],
    n = length(observed),
    events = sum(observed)
  )
}
