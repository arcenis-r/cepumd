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
#' @seealso [ce_mean()]
#'
#' @examples
#' \dontrun{
#' # Download the HG file keeping the section for expenditures on utilities
#' utils_hg <- ce_hg(2017, interview) |>
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
#' utils_interview |>
#'   tidyr::nest(-bls_urbn) |>
#'   mutate(quant_utils = purrr::map(data, ce_quantiles, c(0.25, 0.5, 0.75))) |>
#'   select(-data) |>
#'   unnest(quant_utils)
#' }

ce_quantiles <- function(ce_data, probs = 0.5) {

  check_cols <- c("finlwt21", "cost")

  if (length(setdiff(check_cols, names(ce_data))) > 0) {
    stop("Your dataset needs to include 'finlwt21' and the 'cost' variable")
  } else if (
    !all(vapply(ce_data[, check_cols], class, character(1)) == "numeric")
  ) {
    stop("'finlwt21' and the 'cost' variable must be numeric.")
  }

  ce_data <- ce_data |>
    dplyr::group_by(.data$survey, .data$newid, .data$ucc) |>
    dplyr::summarise(
      dplyr::across(
        c(
          tidyselect::starts_with("wtrep"),
          all_of(c("finlwt21", "mo_scope", "popwt"))
        ),
        mean
      ),
      cost = sum(.data$cost),
      .groups = "drop"
    )

  df <- ce_data |>
    dplyr::select(all_of(c("newid", "finlwt21", "cost"))) |>
    dplyr::group_by(.data$newid) |>
    dplyr::summarise(
      cost = sum(.data$cost),
      finlwt21 = mean(.data$finlwt21)
    ) |>
    dplyr::arrange(.data$cost)

  results <- numeric(length(probs))

  for (i in seq_along(probs)) {
    below <- df |>
      dplyr::filter(cumsum(.data$finlwt21) < sum(.data$finlwt21 * probs[i]))

    above <- df |>
      dplyr::filter(cumsum(.data$finlwt21) > sum(.data$finlwt21 * probs[i]))

    if (sum(below$finlwt21) == sum(above$finlwt21)) {
      result <- sum(
        below[length(below), "cost"],
        above[1, "cost"]
      ) / 2
    } else {
      result <- above[1, "cost"]
    }

    results[i] <- result
  }

  result_names <- paste0(format(probs * 100, trim = TRUE), "%")

  result_df <- data.frame(probs = result_names, quantile = unlist(results))

  return(result_df)
}
