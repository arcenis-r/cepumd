#' Calculate a CE weighted mean
#'
#' @description Calculate a weighted mean using the method used to produce
#' official CE estimates.
#'
#' @param ce_data A data frame containing at least a finlwt21 column,
#' 44 replicate weight columns (wtrep01-44), a cost column, and a survey
#' indicator column. All but the survey column must be numeric.
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
#' @seealso \code{\link{ce_quantiles}}
#'
#' @examples
#' # Calculate the mean pet expenditure using only Diary expenditures
#' ce_mean(ce_diary_pets17)
#'
#' @note
#' Estimates produced using PUMD, which is topcoded by the CE and has some
#' records suppressed to protect respondent confidentiality, will not match the
#' published estimates released by the CE in most cases. The CE's published
#' estimates are based on data that are not topcoded nor have records
#' suppressed. You can learn more at
#' \href{https://www.bls.gov/cex/pumd_disclosure.htm}{CE Protection of
#' Respondent Confidentiality}

ce_mean <- function(ce_data) {

  ### Check dataframe for ce_means()

  check_cols <- c(
    "finlwt21", paste0("wtrep", stringr::str_pad(1:44, 2, "left", "0")),
    "cost"
  )

  if (length(setdiff(c(check_cols, "survey"), names(ce_data))) > 0) {
    stop(
      paste(
        "Your dataset needs to include 'finlwt21', all 44 replicate weights,",
        "i.e., 'wtrep01' to 'wtrep44', the 'cost' variable, and the 'survey'",
        "variable."
      )
    )
  } else if (
    length(setdiff(sapply(ce_data[, check_cols], class), "numeric")) > 0
  ) {
    stop(
      paste(
        "'finlwt21', all replicate weight variables, i.e., 'wtrep01' to",
        "'wtrep44', and the 'cost' variable must be numeric."
      )
    )
  }

  wtrep_vars <- grep("wtrep", names(ce_data), value = TRUE)

  estimates <- ce_data %>%

    # Calculate an aggregate population by survey
    dplyr::group_by(.data$survey) %>%
    dplyr::mutate(aggwt = sum(.data$popwt)) %>%
    dplyr::ungroup() %>%

    # Generate an aggregate expenditure column by multiplying the cost by the
    # consumer unit's weight
    dplyr::mutate(agg_exp = .data$finlwt21 * .data$cost)

  # Adjust each of the weight variables by multiplying it by the expenditure
  # and dividing by the aggregate weight variable, which represents the
  # population weight. This has the effect of converting each observation of
  # the consumer unit weight and associated replicate weights into the ratio
  # of the expenditures of the population representation by a given consumer
  # unit to the total population
  for (i in c("finlwt21", wtrep_vars)) {
    estimates[i] = (estimates[i] * estimates$cost) / estimates$aggwt
  }

  estimates <- estimates %>%

    # Group by UCC
    dplyr::group_by(.data$ucc) %>%

    # Collapse (sum) each adjusted weight column by UCC, which will result in
    # an aggregate expenditure, a mean expenditure, and 44 replicate mean
    # expenditures for each UCC.
    dplyr::summarise_at(
      dplyr::vars(dplyr::contains("wtrep"), .data$finlwt21, .data$agg_exp),
      sum
    ) %>%

    # Drop the observation that accounts for households not having reported any
    # expenditures in the selected categories
    tidyr::drop_na(.data$ucc) %>%

    # Remove the grouping layer
    dplyr::ungroup() %>%

    # Drop the UCC column
    dplyr::select(-.data$ucc) %>%

    # Get the sum of the mean and each of the squared differences from the mean
    dplyr::summarise_all(sum)

  # For each UCC, get the differences between the mean and each of its
  # replicate means then square those differences. Upon running the next
  # command, the values in the "adj_finlwt21" column will represent the
  # estimated means for each of the UCCs.
  for (i in wtrep_vars) {
    (estimates$finlwt21 - estimates[i]) ^ 2
  }

  # Sum up the 44 squared differences
  estimates$sum_sqrs <- rowSums(estimates[, wtrep_vars])

  estimates <- estimates  %>%

    # Generate a standard error column by taking the sum of the 44 squared
    # differences, dividing it by 44, then taking the square root of the result
    dplyr::mutate(
      se = (.data$sum_sqrs / 44) %>% sqrt(),
      cv = .data$se / .data$finlwt21
    ) %>%
    dplyr::select(.data$agg_exp, mean_exp = "finlwt21", .data$se, .data$cv)

  return(estimates)
}
