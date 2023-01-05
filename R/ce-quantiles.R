#' Calculate a CE weighted quantiles
#'
#' @param ce_data A data frame containing at least a finlwt21 column and a cost
#' column. Both columns must be numeric.
#' @param probs A numeric vector of probabilities between 0 and 1 for which to
#' compute quantiles. Default is 0.5 (median).
#'
#' @return A two-column data frame in which the first column contains the
#' probabilities for which quantiles were calculated and their corresponding
#' quantiles in the second column.
#'
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#'
#' @seealso \code{\link{ce_mean}}
#'
#' @examples
#' \dontrun{
#' # Download the HG file keeping the section for expenditures on utilities
#' utils_hg <- ce_hg(2017, interview) %>%
#'   ce_uccs("Utilities, fuels, and public services", uccs_only = FALSE)
#'
#' # Download and prepare interview data
#' utils_interview <- ce_prepdata(
#'   2017,
#'   interview,
#'   uccs = ce_uccs(utils_hg, "Utilities, fuels, and public services"),
#'   zp = NULL,
#'   integrate_data = FALSE,
#'   hg = utils_hg,
#'   bls_urbn
#' )
#'
#' # Calculate the 25%, 50%, and 75% utilities expenditure quantiles
#' ce_quantiles(utils_interview)
#'
#' # Calculate the 25%, 50%, and 75% utilities expenditure quantiles by
#' # urbanicity
#' utils_interview %>%
#'   tidyr::nest(-bls_urbn) %>%
#'   mutate(quant_utils = purrr::map(data, ce_quantiles, c(0.25, 0.5, 0.75))) %>%
#'   select(-data) %>%
#'   unnest(quant_utils)
#' }

ce_quantiles <- function(ce_data, probs = 0.5) {

  check_cols <- c("finlwt21", "cost")

  if (length(setdiff(check_cols, names(ce_data))) > 0) {
    stop("Your dataset needs to include 'finlwt21' and the 'cost' variable")
  } else if (
    length(setdiff(sapply(ce_data[, check_cols], class), "numeric")) > 0
  ) {
    stop("'finlwt21' and the 'cost' variable must be numeric.")
  }

  ce_data <- ce_data %>%
    dplyr::group_by(survey, newid, ucc) %>%
    dplyr::summarise(
      dplyr::across(
        c(finlwt21, tidyselect::starts_with("wtrep"), mo_scope, popwt),
        mean
      ),
      cost = sum(cost),
      .groups = "drop"
    )

  df <- ce_data %>%
    dplyr::select(newid, finlwt21, cost) %>%
    dplyr::group_by(newid) %>%
    dplyr::summarise(
      cost = sum(cost),
      finlwt21 = mean(finlwt21)
    ) %>%
    dplyr::arrange(cost)

  results <- numeric(length(probs))

  for (i in 1:length(probs)) {
    below <- df %>%
      dplyr::filter(cumsum(finlwt21) < sum(finlwt21 * probs[i]))

    above <- df %>%
      dplyr::filter(cumsum(finlwt21) > sum(finlwt21 * probs[i]))

    if (sum(below$finlwt21) == sum(above$finlwt21)) {
      result <- sum(
        below %>% dplyr::slice(n()) %>% dplyr::pull(cost),
        above %>% dplyr::slice(1) %>% dplyr::pull(cost)
      ) / 2
    } else {
      result <- above %>% dplyr::slice(1) %>% dplyr::pull(cost)
    }

    results[i] <- result
  }

  result_names <- paste0(format(probs * 100, trim = TRUE), "%")

  result_df <- data.frame(probs = result_names, quantile = results)

  return(result_df)
}
