#' Stratify Dynamic BEWRS risk
#'
#' Classifies risk probabilities into Low, Watchlist, High, and Critical groups.
#'
#' @param risk Numeric vector of predicted risk probabilities.
#' @param cutoffs Numeric vector of three cutoffs. Default is c(0.25, 0.50, 0.75).
#'
#' @return Ordered factor of risk categories.
#' @export
#'
#' @examples
#' risk_stratify(c(0.1, 0.4, 0.6, 0.9))
risk_stratify <- function(risk, cutoffs = c(0.25, 0.50, 0.75)) {
  if (length(cutoffs) != 3) {
    stop("cutoffs must contain exactly three values.")
  }

  risk <- pmin(pmax(risk, 0), 1)

  factor(
    cut(
      risk,
      breaks = c(-Inf, cutoffs, Inf),
      labels = c("Low", "Watchlist", "High", "Critical"),
      right = FALSE
    ),
    levels = c("Low", "Watchlist", "High", "Critical"),
    ordered = TRUE
  )
}
