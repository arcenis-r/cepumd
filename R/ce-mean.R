#' Calculate a CE weighted, estimated mean
#'
#' @param ce_data A data frame containing at least a finlwt21 column,
#' 44 replicate weight columns (wtrep01-44), and a cost column. All of these
#' columns must be numeric.
#'
#' @return A 1-row dataframe containing the following columns:
#' \itemize{
#'   \item agg_exp - The estimated aggregate expenditure
#'   \item mean_exp - The estimated mean expenditure
#'   \item se - The estimated standard error of the estimated mean expenditure
#'   \item cv - The coefficient of variation of the estimated mean expenditure
#' }
#'
#' @export
#'
#' @importFrom graphics title
#' @importFrom dplyr bind_rows
#'
#' @seealso \code{\link{ce_median}}
#'
#' @examples
#' # Calculate the mean pet expenditure using only Diary expenditures
#' ce_mean(ce_diary_pets17)

ce_mean <- function(ce_data) {

  ### Check dataframe for ce_means()

  check_cols <- c(
    "finlwt21", paste0("wtrep", stringr::str_pad(1:44, 2, "left", "0")),
    "cost"
  )

  if (length(setdiff(check_cols, names(ce_data))) == 0) {
    if (length(setdiff(sapply(ce_data[, check_cols], class), "numeric")) > 0) {
      stop(
        paste(
          "'finlwt21', all replicate weight variables, i.e., 'wtrep01' to",
          "'wtrep44', and the 'cost' variable must be numeric."
        )
      )
    }
  } else {
    stop(
      paste(
        "Your dataset needs to include 'finlwt21', all 44 replicate weights,",
        "i.e., 'wtrep01' to 'wtrep44', and the 'cost' variable"
      )
    )
  }

  estimates <- ce_data %>%

    # Generate an aggregate expenditure column by multiplying the cost by the
    # consumer unit's weight
    dplyr::mutate(agg_exp = .data$finlwt21 * .data$cost) %>%

    # Adjust each of the weight variables by multiplying it by the expenditure
    # and dividing by the aggregate weight variable, which represents the
    # population weight. This has the effect of converting each observation of
    # the consumer unit weight and associated replicate weights into the ratio
    # of the expenditures of the population representation by a given consumer
    # unit to the total population
    dplyr::mutate_at(
      dplyr::vars(.data$finlwt21, dplyr::contains("wtrep")),
      list(~(cost * (. / .data$aggwt)))
    ) %>%

    # Rename each of the weight variables with the prefix "adj_" to reflect that
    # they've been adjusted
    dplyr::rename_at(
      dplyr::vars(dplyr::contains("wtrep"), .data$finlwt21),
      list(~paste0("adj_", .))
    ) %>%

    # Group by UCC
    dplyr::group_by(.data$ucc) %>%

    # Collapse (sum) each adjusted weight column by UCC, which will result in
    # an aggregate expenditure, a mean expenditure, and 44 replicate mean
    # expenditures for each UCC.
    dplyr::summarise_at(
      dplyr::vars(dplyr::contains("adj_"), .data$agg_exp), list(~sum(.))
    ) %>%

    # Drop the observation that accounts for households not having reported any
    # expenditures in the selected categories
    tidyr::drop_na(.data$ucc) %>%

    # Remove the grouping layer
    dplyr::ungroup() %>%

    # Drop the UCC column
    dplyr::select(-.data$ucc) %>%

    # Get the sum of the mean and each of the squared differences from the mean
    dplyr::summarise_all(sum) %>%

    # For each UCC, get the differences between the mean and each of its
    # replicate means then square those differences. Upon running the next
    # command, the values in the "adj_finlwt21" column will represent the
    # estimated means for each of the UCCs.
    dplyr::mutate_at(
      dplyr::vars(
        dplyr::contains("adj_wtrep")), list(~(.data$adj_finlwt21 - .) ^ 2
      )
    ) %>%

    # Generate a standard error column by taking the sum of the 44 squared
    # differences, dividing it by 44, then taking the square root of the result
    dplyr::mutate(
      sum_sqrs = dplyr::select(., dplyr::contains("adj_wtrep")) %>% rowSums(),
      se = (.data$sum_sqrs / 44) %>% sqrt(),
      cv = .data$se / .data$adj_finlwt21
    ) %>%
    dplyr::select(.data$agg_exp, mean_exp = "adj_finlwt21", .data$se, .data$cv)

  return(estimates)
}
