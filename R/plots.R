#' Plot calibration curve
#'
#' @param observed Binary observed outcome, coded 0/1.
#' @param predicted Predicted probabilities.
#' @param groups Number of quantile groups.
#'
#' @return A ggplot object.
#' @export
plot_calibration <- function(observed, predicted, groups = 10) {
  dat <- data.frame(
    observed = observed,
    predicted = predicted
  )

  dat$group <- cut(
    dat$predicted,
    breaks = stats::quantile(dat$predicted, probs = seq(0, 1, length.out = groups + 1), na.rm = TRUE),
    include.lowest = TRUE
  )

  cal <- stats::aggregate(
    cbind(observed, predicted) ~ group,
    data = dat,
    FUN = mean
  )

  ggplot2::ggplot(cal, ggplot2::aes(x = predicted, y = observed)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Mean predicted risk",
      y = "Observed event rate",
      title = "Calibration Plot"
    ) +
    ggplot2::theme_minimal()
}


#' Plot risk group event rates
#'
#' @param observed Binary observed outcome, coded 0/1.
#' @param risk_group Ordered risk group factor.
#'
#' @return A ggplot object.
#' @export
plot_risk_groups <- function(observed, risk_group) {
  dat <- data.frame(
    observed = observed,
    risk_group = risk_group
  )

  agg <- stats::aggregate(
    observed ~ risk_group,
    data = dat,
    FUN = mean
  )

  ggplot2::ggplot(agg, ggplot2::aes(x = risk_group, y = observed)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Risk group",
      y = "Observed event rate",
      title = "Observed Event Rate by Risk Group"
    ) +
    ggplot2::theme_minimal()
}
