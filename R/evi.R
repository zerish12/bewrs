#' Compute Expected Value of Intervention
#'
#' Computes the Expected Value of Intervention (EVI) as the reduction
#' in expected loss from no action to the selected intervention.
#'
#' @param loss_no_action Numeric vector of expected loss under no action.
#' @param loss_intervention Numeric vector of expected loss under intervention.
#'
#' @return Numeric vector of EVI values.
#' @export
#'
#' @examples
#' compute_evi(c(10, 5, 2), c(7, 4, 3))
compute_evi <- function(loss_no_action, loss_intervention) {
  if (length(loss_no_action) != length(loss_intervention)) {
    stop("loss_no_action and loss_intervention must have the same length.")
  }

  loss_no_action - loss_intervention
}


#' Select optimal intervention
#'
#' Selects the intervention with the lowest expected loss.
#'
#' @param loss_matrix Numeric matrix or data.frame of expected losses.
#' Rows represent observational units and columns represent intervention options.
#'
#' @return A data.frame containing optimal action and minimum expected loss.
#' @export
#'
#' @examples
#' losses <- data.frame(
#'   no_action = c(10, 5, 2),
#'   monitor = c(8, 4, 3),
#'   review = c(7, 6, 4)
#' )
#' optimal_intervention(losses)
optimal_intervention <- function(loss_matrix) {
  loss_matrix <- as.data.frame(loss_matrix)

  if (!all(vapply(loss_matrix, is.numeric, logical(1)))) {
    stop("All columns in loss_matrix must be numeric.")
  }

  min_index <- apply(loss_matrix, 1, which.min)

  data.frame(
    optimal_action = names(loss_matrix)[min_index],
    minimum_expected_loss = apply(loss_matrix, 1, min)
  )
}
