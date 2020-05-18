#' Generate a CE heiarchical grouping file as data frame
#'
#' @description A CE heiarchical grouping ('HG') file shows the levels of
#' aggregation for expenditure categories used to produce official CE
#' expenditure estimates. This function reads in a CE HG file for the given
#' year and HG type as data frame.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param hg_type The type of HG file, i.e., interview, diary, or
#' integrated. Accepted as a string or symbol (quotes or no quotes).
#' @param hg_zip_path The path to a zip file containing HG files downloaded
#' from the CE website. The structure of the zip file must be exactly as it is
#' when downloaded to be useful to this function. The default is \code{NULL}
#' (see details).
#'
#' @return A data frame containing the following columns:
#' * level - hierarchical level of the expenditure category
#' * title - the title of the expenditure category
#' * ucc - the Universal Classification Code (UCC) for the expenditure category
#' * survey - the survey instrument from which the data for a given UCC are
#'            sourced. This is most helpful when data for a type of expenditure
#'            are collected in both the Interview and the Diary.
#' * factor - the factor by which to multiply the expenditure in the calculation
#'            of estimated means / medians
#'
#' @details
#' Interview and Diary HG files are available starting in 1997 and integrated
#' files start in 1996. For consistency, this function and other \code{ce_r}
#' functions only work with data starting in 1997.
#'
#' The output will contain only expenditure UCCs and not UCCs related
#' to household characteristics, income, assets, or liabilities. The scope of
#' the functions in this package is limited to expenditures. Income, for
#' example, is imputed and calculation of income means goes through a different
#' process than do expenditure means. Please see
#' \url{https://www.bls.gov/cex/csxguide.pdf}{User's Guide to Income Imputation
#' in the CE}
#'
#' The default argument for \code{hg_zip_path} is \code{NULL}, which will direct
#' the function to call \code{\link{store_ce_hg}} and store the zip file to a
#' temporary file.
#'
#' @export
#'
#' @importFrom rlang ensym
#' @importFrom dplyr case_when
#' @importFrom dplyr row_number
#'
#' @examples
#' # 'hg_type' can be entered as a string
#' ce_hg(2016, "integrated")
#'
#' # 'hg_type' can also be entered as a symbol
#' ce_hg(2016, integrated)

ce_hg <- function(year, hg_type, hg_zip_path = NULL) {

  hg_type <- rlang::ensym(hg_type)
  hg_type_name <- rlang::as_name(hg_type) %>% tolower

  ###### Check for bad arguments ######
  if (!year %in% 1997:2018) {
    stop("'year' must be a number between 1997 and 2018")
  }

  if (
    !hg_type_name %in% c("interview", "diary", "integrated")
  ) {
    stop("'hg_type' must be one of interview, diary, or integrated")
  }

  if (is.null(hg_zip_path)) {
    hg_zip_path <- tempfile()
    store_ce_hg(hg_zip_path)
  } else if (isFALSE(file.exists(hg_zip_path))) {
    store_ce_hg(hg_zip_path = hg_zip_path)
  }

  instrument <- switch(
    hg_type_name,
    "diary" = "Diary",
    "interview" = "Inter",
    "integrated" = "Integ"
  )

  # Declare a function that conditionally selects out an extra column that
  # was added only for the 2013 and 2014 years of the hg files
  cond_select <- function(df, yr) {
    if (yr %in% 2013:2014) {
      as.data.frame(df) %>%
        dplyr::select(-.data$X5)
    } else {
      df
    }
  }

  guess_lines <- ifelse(
    year == 2009 & instrument %in% c("Integ", "Inter"),
    39,
    15
  )

  hg_lines <- readr::read_lines(
    unzip(
      hg_zip_path,
      stringr::str_c("stubs/CE-HG-", instrument, "-", year, ".txt"),
      exdir = tempdir()
    )
  )

  removals <- which(
    stringr::str_sub(hg_lines, 1, 6) %in% c("*  UCC", "*  NEW")
  )

  if (length(removals > 0)) hg_lines <- hg_lines[-removals]

  hg_lines <- stringr::str_replace_all(hg_lines, "[#]", "")

  tmp <- tempfile()

  readr::write_lines(hg_lines, tmp)

  first_line <- match("1", stringr::str_sub(readr::read_lines(tmp), 1, 1))

  hg <- readr::read_table(
    tmp, col_names = FALSE, guess_max = guess_lines,
    skip = (first_line - 1)
  ) %>%
    cond_select(year) %>%
    dplyr::select(1:7) %>%
    rlang::set_names(
      c("linenum", "level", "title", "ucc", "survey", "factor", "group")
    ) %>%
    dplyr::mutate_all(
      list(~(stringr::str_trim(.) %>% stringr::str_squish()))
    ) %>%
    dplyr::mutate(
      rnum = dplyr::row_number(),
      title = dplyr::case_when(
        rnum == max(rnum) & .data$linenum == 1 ~ title,
        dplyr::lead(linenum == 2) ~ paste(title, dplyr::lead(title)),
        TRUE ~ title
      ) %>%
        stringr::str_replace(" #$", "")
    ) %>%
    dplyr::filter(
      !.data$linenum %in% "2",
      .data$group %in% c("FOOD", "EXPEND")
    ) %>%
    dplyr::select(
      .data$level, .data$title, .data$ucc, .data$survey, .data$factor
    )

  return(hg)
}
