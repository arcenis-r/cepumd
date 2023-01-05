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
#' @importFrom dplyr bind_rows across
#' @importFrom tidyselect all_of contains everything
#'
#' @seealso \code{\link{ce_quantiles}}
#'
#' @examples
#'
#' # Download the HG file keeping the section for expenditures on utilities
#' \dontrun{
#' utils_hg <- ce_hg(2017, interview) %>%
#'   ce_uccs("Utilities, fuels, and public services", uccs_only = FALSE)
#' }
#'
#' # Download and prepare interview data
#' \dontrun{
#' utils_interview <- ce_prepdata(
#'   2017,
#'   interview,
#'   uccs = ce_uccs(utils_hg, "Utilities, fuels, and public services"),
#'   zp = NULL,
#'   integrate_data = FALSE,
#'   hg = utils_hg,
#'   bls_urbn
#' )
#' }
#'
#' # Calculate the mean expenditure on utilities
#' \dontrun{ce_mean(utils_interview)}
#'
#' # Calculate the mean expenditure on utilities by urbanicity
#' \dontrun{
#' utils_interview %>%
#'   tidyr::nest(-bls_urbn) %>%
#'   mutate(mean_utils = purrr::map(data, ce_mean)) %>%
#'   select(-data) %>%
#'   unnest(mean_utils)
#' }
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

  # Store a vector of replicate weight variable names
  wtrep_vars <- stringr::str_c("wtrep", stringr::str_pad(1:44, 2, "left", "0"))

  check_cols <- c("finlwt21", wtrep_vars, "cost")

  if (length(setdiff(c(check_cols, "survey"), names(ce_data))) > 0) {
    stop(
      stringr::str_c(
        "Your dataset needs to include 'finlwt21', all 44 replicate weights,",
        "i.e., 'wtrep01' to 'wtrep44', the 'cost' variable, and the 'survey'",
        "variable.",
        sep = " "
      )
    )
  } else if (
    length(setdiff(sapply(ce_data[, check_cols], class), "numeric")) > 0
  ) {
    stop(
      stringr::str_c(
        "'finlwt21', all replicate weight variables, i.e., 'wtrep01' to",
        "'wtrep44', and the 'cost' variable must be numeric.",
        sep = " "
      )
    )
  }

  # Summarise the data at the UCC level. ce_prepdata() summarises at the level
  # of reference year and month to allow for inflation adjustment.
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

  # Calculate aggregate weights by survey type
  aggwts <- ce_data %>%
    dplyr::select(survey, newid, popwt) %>%
    dplyr::group_by(survey, newid, popwt) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(survey) %>%
    dplyr::summarise(aggwt = sum(popwt)) %>%
    dplyr::ungroup()

  ce_data %>%

    # Calculate an aggregate population by survey
    dplyr::left_join(aggwts, by = "survey") %>%
    dplyr::mutate(
      # Generate an aggregate expenditure column by multiplying the cost by the
      # consumer unit's weight
      agg_exp = finlwt21 * cost,

      # Adjust each of the weight variables by multiplying it by the expenditure
      # and dividing by the aggregate weight variable, which represents the
      # population weight. This has the effect of converting each observation of
      # the consumer unit weight and associated replicate weights into the ratio
      # of the expenditures of the population representation by a given consumer
      # unit to the total population
      dplyr::across(
        c(finlwt21, tidyselect::all_of(wtrep_vars)),
        ~ (.x * cost) / aggwt
      )
    ) %>%

    # Group by UCC
    dplyr::group_by(ucc) %>%

    # Collapse (sum) each adjusted weight column by UCC, which will result in
    # an aggregate expenditure, a mean expenditure, and 44 replicate mean
    # expenditures for each UCC.
    dplyr::summarise(
      dplyr::across(c(dplyr::contains("wtrep"), finlwt21, agg_exp), sum),
      .groups = "drop"
    ) %>%

    # Drop the observation that accounts for households not having reported any
    # expenditures in the selected categories
    tidyr::drop_na(ucc) %>%

    # Drop the UCC column
    dplyr::select(-ucc) %>%

    # Get the sum of the mean and each of the replicate means
    dplyr::summarise(dplyr::across(tidyselect::everything(), sum)) %>%

    dplyr::mutate(
      # For each UCC, get the differences between the mean and each of its
      # replicate means then square those differences. Upon running the next
      # command, the values in the "adj_finlwt21" column will represent the
      # estimated means for each of the UCCs.
      dplyr::across(tidyselect::all_of(wtrep_vars), ~ (finlwt21 - .x) ^ 2),

      # Sum up the 44 squared differences
      sum_sqrs = rowSums(across(tidyselect::all_of(wtrep_vars))),

      # Generate a standard error column by taking the sum of the 44 squared
      # differences, dividing it by 44, then taking the square root of the
      # result
      se = sqrt((sum_sqrs / 44)),
      cv = (se * 100) / finlwt21
    ) %>%
    dplyr::select(agg_exp, mean_exp = "finlwt21", se, cv)
}
