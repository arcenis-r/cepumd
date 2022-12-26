#' Generate a CE heiarchical grouping file as data frame
#'
#' @description A CE heiarchical grouping ('HG') file shows the levels of
#' aggregation for expenditure categories used to produce official CE
#' expenditure estimates. This function reads in a CE HG file for the given
#' year and HG type as data frame.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey The type of HG file, i.e., interview, diary, or
#' integrated. Accepted as a character or symbol.
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
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
#' @importFrom stringr str_replace_all
#' @importFrom readr read_lines
#'
#' @examples
#' # 'survey' can be entered as a string
#' ce_hg(2016, "integrated")
#'
#' # 'survey' can also be entered as a symbol
#' ce_hg(2016, integrated)

ce_hg <- function(year, survey, ce_dir = NULL, hg_zip_path = NULL) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_name(survey) %>% tolower

  ###### Check for bad arguments ######
  max_year <- max(ce_pumd_years())

  if (!year %in% 1997:max_year) {
    stop(
      stringr::str_c("'year' must be a number between 1997 and ", max_year, ".")
    )
  }

  if (
    !survey_name %in% c("interview", "diary", "integrated")
  ) {
    stop("'survey' must be one of interview, diary, or integrated")
  }

  # Ensure that there's a directory to put files into
  if (is.null(ce_dir)) {
    if (!file.exists(file.path(tempdir(), "ce-data"))) {
      dir.create(file.path(tempdir(), "ce-data"))
    }

    ce_dir <- file.path(tempdir(), "ce-data")
  }

  if (is.null(hg_zip_path)) {
    hg_zip_path <- "ce-stubs.zip"

    if (isFALSE(file.exists(file.path(ce_dir, hg_zip_path)))) {
      store_ce_hg(ce_dir, hg_zip_path)
    }
  }

  if (year %in% 2013:2014) {
    pos_start <- c(1, 4, 7, 70, 80, 83, 86, 89)
    pos_end <- c(1, 4, 69, 77, 80, 83, 86, NA)
    c_names <- c(
      "info_type", "level", "ucc_name", "ucc", "x", "source", "factor",
      "section"
    )
  } else if (year %in% 2015:2020) {
    pos_start <- c(1, 4, 7, 70, 83, 86, 89)
    pos_end <- c(1, 4, 69, 77, 83, 86, NA)
    c_names <- c(
      "info_type", "level", "ucc_name", "ucc", "source", "factor", "section"
    )
  } else if (survey_name %in% "diary" & year %in% 2000) {
    pos_start <- c(1, 4, 7, 69, 79, 82, 85)
    pos_end <- c(1, 4, 68, 77, 80, 83, NA)
    c_names <- c(
      "info_type", "level", "ucc_name", "ucc", "source", "factor", "section"
    )
  } else if (survey_name %in% "integrated" & year %in% 1998:2000) {
    pos_start <- c(1, 4, 7, 70, 83, 86, 89)
    pos_end <- c(1, 4, 69, 77, 83, 86, NA)
    c_names <- c(
      "info_type", "level", "ucc_name", "ucc", "source", "factor", "section"
    )
  } else {
    pos_start <- c(1, 4, 7, 70, 80, 83, 86)
    pos_end <- c(1, 4, 69, 77, 80, 83, NA)
    c_names <- c(
      "info_type", "level", "ucc_name", "ucc", "source", "factor", "section"
    )
  }

  instrument <- switch(
    survey_name,
    "diary" = "Diary",
    "interview" = "Inter",
    "integrated" = "Integ"
  )

  # Declare a function that conditionally removes an extra column that
  # was added only for the 2013 and 2014 years of the hg files
  cond_select <- function(df, yr) {
    if (yr %in% 2013:2014) {
      as.data.frame(df) %>%
        dplyr::select(-x)
    } else {
      df
    }
  }

  hg_lines_temp <- tempfile("ce-stub-lines-", tmpdir = ce_dir)

  hg_lines <- readr::read_lines(
    unzip(
      file.path(ce_dir, hg_zip_path),
      stringr::str_c("stubs/CE-HG-", instrument, "-", year, ".txt"),
      exdir = hg_lines_temp
    )
  )

  removals <- which(
    stringr::str_sub(hg_lines, 1, 6) %in% c("*  UCC", "*  NEW")
  )

  if (length(removals > 0)) hg_lines <- hg_lines[-removals]

  hg_lines <- stringr::str_replace_all(hg_lines, "[#]", "")

  hg_tmp_clean <- tempfile("ce-stub-lines-clean-", tmpdir = ce_dir)

  readr::write_lines(hg_lines, hg_tmp_clean)

  first_line <- match(
    "1",
    stringr::str_sub(readr::read_lines(hg_tmp_clean), 1, 1)
  )

  hg <- readr::read_fwf(
    hg_tmp_clean,
    col_positions = readr::fwf_positions(pos_start, pos_end, c_names),
    skip = (first_line - 1),
    show_col_types = FALSE
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
        rnum == max(rnum) & linenum == 1 ~ title,
        dplyr::lead(linenum == 2) ~ paste(title, dplyr::lead(title)),
        TRUE ~ title
      ) %>%
        stringr::str_replace(" #$", "")
    ) %>%
    dplyr::filter(
      !linenum %in% "2",
      group %in% c("FOOD", "EXPEND")
    ) %>%
    dplyr::select(
      level, title, ucc, survey, factor
    )

  unlink(hg_lines_temp, recursive = TRUE)
  unlink(hg_tmp_clean, recursive = TRUE)

  return(hg)
}
