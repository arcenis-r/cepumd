#' Generate a CE stub file as data frame
#'
#' @description A CE stub file shows the levels of aggregation for expenditure
#' categories used to produce official CE expenditure estimates. This function
#' reads in a CE stub file for the given year and stub type as data frame.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param stub_type The type of stub file, i.e., interview, diary, or
#' integrated. Accepted as a string or symbol (quotes or no quotes). Integrated
#' stub files are only available starting in the year 2002 (see details).
#'
#' @return A data frame containing the following columns:
#' * level - hierarchical level of the expenditure category
#' * title - the title of the expenditure category
#' * ucc - the Universal Classification Code (UCC) for the expenditure category
#' * factor - the factor by which to multiply the expenditure in the calculation
#'   of estimated means / medians
#'
#' @details
#' The CE did not have publicly available stub files between 1996 and 2001.
#' Instead, they had label files and crosswalk files. The label files had line
#' line numbers indicating the order of the categories in the title column and
#' the crosswalk file contained line numbers and UCC's. The titles in the,
#' labels files, however, were the titles for high level category groupings.
#' The titles for the lower level UCC's were published in the documentation for
#' the given year of data. I have copied those sections of the documentation
#' into data files contained in this package, e.g., "interview_uccs_1996."
#' This function pulls the crosswalk and labels from the CE website, merges
#' them, then merges in the UCC level titles from the data files.
#'
#' Also, because of the above mentioned issue, integrated stub files between
#' 1996 and 2001 do not exist, so entering a combination of year <= 2001 and
#' stub_type = "integrated" will throw an error.
#'
#' Finally, the output will contain only expenditure UCCs and not UCCs related
#' to household characteristics, income, assets, or liabilities. The scope of
#' the functions in this package is limited to expenditures. Income, for
#' example, is imputed and calculation of income means goes through a different
#' process than do expenditure means. Please see
#' \url{https://www.bls.gov/cex/csxguide.pdf}{User's Guide to Income Imputation
#' in the CE}
#'
#' @export
#'
#' @importFrom rlang ensym
#'
#' @examples
#' # 'stub_type' can be entered as a string
#' ce_stub(2016, "integrated")
#'
#' # 'stub_type' can also be entered as a symbol
#' ce_stub(2016, integrated)

ce_stub <- function(year, stub_type) {

  stub_type <- rlang::ensym(stub_type)
  stub_type_name <- rlang::as_name(stub_type) %>% tolower

  ###### Check for bad arguments ######
  if (!year %in% 1996:2017) {
    stop("'year' must be a number between 1996 and 2017")
  }

  if (
    !stub_type_name %in% c("interview", "diary", "integrated")
  ) {
    stop("'stub_type' must be one of interview, diary, or integrated")
  }

  if (stub_type_name %in% "integrated" & year < 2002) {
    stop("Integrated stub files are only available for 2002 through 2017")
  }

  # Generate a stub file depending on the "year" argument
  if (as.numeric(year) %in% 1996:2001) {
    make.stub.96.01(year, stub_type_name)
  } else {
    make.stub.post.01(year, stub_type_name)
  }
}
