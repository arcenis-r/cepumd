#' Prepare CE data for calculating an estimated mean or median
#'
#' @description Reads in the family characteristics (FMLI/-D) and expenditure
#' tabulation (MTBI/EXPD) files and merges the relevant data for calculating an
#' estimated mean or median.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey One of either "interview" or "diary" as a string or symbol.
#' @param uccs A character vector of UCC's corresponding to expenditure
#' categories in the stub file for a given year and survey
#' @param zp A string indicating the zip file path where data are stored. The
#' default is NULL.
#' @param integrate_data A logical indicating whether to prepare the data to
#' calculate an integrated mean or median. The default is TRUE. (See details)
#' @param stub A data frame that has, at least, the title, level, ucc, and
#' factor columns of a CE stub file. Calling \code{\link{ce_stub}} will generate
#' a valid stub file.
#' @param ... Variables to include in the dataset from the family
#' characteristics file. This is intended to allow the user to calculate
#' estimates for subsets of the data.
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item newid - A consumer unit (CU), or household, identifier
#'   \item finlwt21 - CU weight variable
#'   \item wtrep01 through wtrep44 - CU replicate weight variables (see details)
#'   \item ... - Any family characteristics variables that were kept
#'   \item mo_scope - Months in scope (see details)
#'   \item popwt - An adjusted weight meant to account for the fact that a CU's
#'         value of finlwt21 is meant to be representative of only 1 quarter of
#'         data (see details)
#'   \item aggwt - The sum of popwt (used for calculating estimated means)
#'   \item ucc - The UCC for a given expenditure
#'   \item cost - The value of the expenditure (in U.S. Dollars)
#' }
#'
#' @details
#' CE data include 45 weights. The primary weight that is used for calculating
#' estimated means and medians is finlwt21. The 44 replicate weights are
#' computed using Balanced Repeated Replication (BRR) and are used for
#' calculating weighted standard errors.
#'
#' "Months in scope" refers to the proportion of the data collection quarter for
#' which a CU reported expenditures. For the Diary survey the months in scope is
#' always 3 because the expenditure data collected are meant to be reported for
#' the quarter in which they are collected. The Interview Survey, on the other
#' hand, is a quarterly, rolling, recall survey and the CU's report expenditures
#' for the 3 months previous to the month in which the data are collected. For
#' example, if a CU was interviewed in February 2017, then they would be
#' providing data for November 2016, December 2016, and January 2017. If one is
#' calculating a weighted estimated mean for the 2017 calendar year, then only
#' the January 2017 data would be "in scope."
#'
#' CE data are reported quarterly, but the sum of the weights (finlwt21) is
#' for all CU's is meant to represent the total number of U.S. CU's for a given
#' year. Since a calculating a calendar year estimate requires the use of 4
#' quarters of data and the sum of the weights in each quarter equals the
#' number of households in the U.S. for a given year, adding up the sums of the
#' weights in the 4 quarters of data would yield a total number of households
#' that is approximately 4 times larger than the actual number of households in
#' the U.S. in the corresponding year.
#'
#' Since some UCC's can appear in both surveys, for the purposes of integration,
#' the CE has a source selection procedure by which to choose which source data
#' will be taken from for a given UCC. For example, of the 4 UCC's in the "Pets"
#' category in 2017 two were sourced for publication from the Diary and two from
#' the Interview. Please download the CE Source Selection Document for a
#' complete listing: \url{https://www.bls.gov/cex/ce_source_integrate.xlsx}.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # The following workflow will prepare a dataset for calculating integrated
#' # pet expenditures for 2017 keep the "sex_ref" variable in the data to
#' # potentially calculate means by sex of the reference person.
#'
#' # First generate a stub file
#' mystub <- ce_stub(2017, interview)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(mystub, "Pets")
#'
#' # Store the interview data
#' pets_int <- ce_prepdata(
#'   year = 2017, survey = interview, uccs = pet_uccs, zp = NULL,
#'   integrate_data = TRUE, stub = NULL, sex_ref
#' )

ce_prepdata <- function(
  year, survey, uccs, zp = NULL, integrate_data = TRUE, stub = NULL, ...
) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_string(survey) %>% tolower()

  grp_vars <- rlang::ensyms(...)
  grp_var_names <- purrr::map(grp_vars, rlang::as_string) %>% unlist()

  unlink_zp <- is.null(zp)

  if (!year %in% 1996:2017) {
    stop("'year' must be a number between 1996 and 2017")
  }

  if (!survey_name %in% c("interview", "diary")) {
    stop("'survey' must be one of 'interview' or 'diary'.")
  }

  if (length(uccs) > 0 & is.character(uccs)) {
    for (u in uccs) {
      if (is.na(as.numeric(u)) | nchar(u) != 6) {
        stop(
          paste0(
            "'", u, "' is not a valid UCC ",
            "Please review the CE survey documentation to ensure '", u,
            "' is a UCC in your dataset (check by year)."
          )
        )
      }
    }
  }

  if (!is.null(zp)) {
    if (is.character(zp)) {
      if (!nzchar(zp)) stop("'zp' must be non-empty string.")
      if (!file.exists(zp)) stop("'zp' does not exist.")
    }
  } else {
    zp <- tempfile()
    ce_download(year = year, survey = !!survey_name, zp = zp)
  }

  if (survey_name %in% "interview") {
    fmli_files <- grep(
      "fmli",
      unzip(zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    mtbi_files <- grep(
      "mtbi",
      unzip(zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    fmli <- purrr::map_df(
      fmli_files, read.fmli, zp = zp, year = year,
      grp_var_names = grp_var_names
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("wtrep")), list(~ replace(., is.na(.), 0))
      ) %>%
      dplyr::mutate(aggwt = sum(.data$popwt))

    mtbi <- purrr::map_df(
      mtbi_files, read.mtbi, zp = zp, year = year, uccs = uccs,
      integrate_data = integrate_data, stub = stub
    ) %>%
      dplyr::bind_rows()

    dat <- dplyr::left_join(fmli, mtbi, by = "newid") %>%
      dplyr::mutate(cost = replace(.data$cost, is.na(.data$cost), 0))
  }

  if (survey_name %in% "diary") {
    fmld_files <- grep(
      "fmld",
      unzip(zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    expd_files <- grep(
      "expd",
      unzip(zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    fmld <- purrr::map_df(
      fmld_files, read.fmld, zp = zp, grp_var_names = grp_var_names
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("wtrep")), list(~ replace(., is.na(.), 0))
      ) %>%
      dplyr::mutate(aggwt = sum(.data$popwt))

    expd <- purrr::map_df(
      expd_files, read.expd, zp = zp, year = year, uccs = uccs,
      integrate_data = integrate_data, stub = stub
    ) %>%
      dplyr::bind_rows()

    dat <- dplyr::left_join(fmld, expd, by = "newid") %>%
      dplyr::mutate(cost = replace(.data$cost, is.na(.data$cost), 0))
  }

  if (unlink_zp) unlink(zp)

  return(dat)
}
