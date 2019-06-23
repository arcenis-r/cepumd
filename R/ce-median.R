#' Calculate a CE weighted, estimated median
#'
#' @param ce_data A data frame containing at least a finlwt21 column and a cost
#' column. Both columns must be numeric.
#'
#' @return A weighted, estimated median (numeric)
#' @export
#'
#' @seealso \code{\link{ce_mean}}
#'
#' @examples
#' # Calculate the mean pet expenditure
#' ce_median(ce_diary_pets17)

ce_median <- function(ce_data) {

  check_cols <- c("finlwt21", "cost")

  if (length(setdiff(check_cols, names(ce_data))) > 0) {
    stop("Your dataset needs to include 'finlwt21' and the 'cost' variable")
  } else if (
    length(setdiff(sapply(ce_data[, check_cols], class), "numeric")) > 0
  ) {
    stop("'finlwt21' and the 'cost' variable must be numeric.")
  }

  df <- ce_data %>%
    dplyr::select(.data$newid, .data$finlwt21, .data$cost) %>%
    group_by(.data$newid) %>%
    summarise(cost = sum(.data$cost), finlwt21 = mean(.data$finlwt21)) %>%
    dplyr::arrange(.data$cost)

  below <- df %>%
    dplyr::filter(cumsum(.data$finlwt21) < sum(.data$finlwt21 / 2))

  above <- df %>%
    dplyr::filter(cumsum(.data$finlwt21) > sum(.data$finlwt21 / 2))

  if (sum(below$finlwt21) == sum(above$finlwt21)) {
    ce_median <- sum(
      below %>% dplyr::slice(.data$n()) %>% dplyr::pull(.data$cost),
      above %>% dplyr::slice(1) %>% dplyr::pull(.data$cost)
    ) / 2
  } else {
    ce_median <- above %>% dplyr::slice(1) %>% dplyr::pull(.data$cost)
  }

  return(ce_median)
}
