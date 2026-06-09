#' Compute Dynamic BEWRS
#'
#' Computes the Bayesian Dynamic Early-Warning Risk Score using current posterior
#' underperformance probability, persistence, and deterioration.
#'
#' @param up Numeric vector of posterior underperformance probabilities.
#' @param persistence Numeric vector measuring persistence of elevated risk.
#' @param deterioration Numeric vector measuring recent deterioration.
#' @param alpha Intercept parameter.
#' @param beta_up Coefficient for logit-transformed posterior risk.
#' @param beta_persistence Coefficient for persistence.
#' @param beta_deterioration Coefficient for deterioration.
#'
#' @return Numeric vector of Dynamic BEWRS probabilities.
#' @export
#'
#' @examples
#' compute_dynamic_bewrs(
#'   up = c(0.2, 0.5, 0.8),
#'   persistence = c(0.1, 0.4, 0.7),
#'   deterioration = c(0.0, 0.1, 0.2)
#' )
compute_dynamic_bewrs <- function(
    up,
    persistence,
    deterioration,
    alpha = 0,
    beta_up = 1,
    beta_persistence = 1,
    beta_deterioration = 1
) {
  eps <- 1e-6
  up <- pmin(pmax(up, eps), 1 - eps)

  logit_up <- log(up / (1 - up))

  eta <- alpha +
    beta_up * logit_up +
    beta_persistence * persistence +
    beta_deterioration * deterioration

  1 / (1 + exp(-eta))
}
