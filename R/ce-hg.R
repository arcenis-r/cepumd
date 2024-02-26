#' Convert a CE heiarchical grouping file to a data frame
#'
#' @description A CE heiarchical grouping ('HG') file shows the levels of
#' aggregation for expenditure categories used to produce official CE
#' expenditure estimates. This function reads in a CE HG file for the given
#' year and HG type as data frame.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey The type of HG file; one of "interview", "diary", or
#' "integrated". Accepted as a character or symbol.
#' @param hg_zip_path The path to a zip file containing HG files downloaded
#' from the CE website. The structure of the zip file must be exactly as it is
#' when downloaded to be useful to this function.
#' @param hg_file_path The path to a single HG file that has already been
#' extracted. If this argument is given 'hg_zip_path' is ignored.
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
#' files start in 1996. For consistency, this function and other \code{cepumd}
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
#' @export
#'
#' @importFrom rlang ensym
#' @importFrom rlang .data
#' @importFrom dplyr summarise across filter select group_by
#' @importFrom stringr str_replace_all str_c
#' @importFrom readr read_lines
#' @importFrom tidyr fill nest unnest
#' @importFrom tidyselect everything
#' @importFrom tidyselect all_of
#' @importFrom tidyselect one_of
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # 'survey' can be entered as a string
#' ce_hg(2016, "integrated", "hg-files.zip")
#'
#' # 'survey' can also be entered as a symbol
#' ce_hg(2016, integrated, "hg-files.zip")
#' }

ce_hg <- function(year, survey, hg_zip_path = NULL, hg_file_path = NULL) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_name(survey) |> tolower()

  ###### Check for bad arguments ######
  valid_hg_file <- FALSE

  if (year < 1997) {
    stop(
      paste(
        "This function can only convert hierarchical grouping files from 1997",
        "onward."
      )
    )
  }

  if (
    !survey_name %in% c("interview", "diary", "integrated")
  ) {
    stop("'survey' must be one of interview, diary, or integrated")
  }

  if (is.null(hg_zip_path) & is.null(hg_file_path)) {
    stop("Either 'hg_zip_path' or 'hg_file_path' is required.")
  }

  if (!is.null(hg_file_path)) {
    if (!file.exists(hg_file_path)) {
      stop("The path provided for 'hg_file_path' does not exist.")
    } else {
      valid_hg_file <- TRUE
    }
  }

  if (!is.null(hg_zip_path)) {
    if (!file.exists(hg_zip_path) & !valid_hg_file) {
      stop("The path provided for 'hg_zip_path' does not exist.")
    }
  }

  instrument <- switch(
    survey_name,
    "diary" = "Diary",
    "interview" = "Inter",
    "integrated" = "Integ"
  )

  if (year %in% 2013:2020 | (year %in% 1998:2000 & instrument %in% "Integ")) {
    pos_start <- c(1, 4, 7, 70, 83, 86, 89)
    pos_end <- c(1, 4, 69, 77, 83, 86, NA)
  } else {
    pos_start <- c(1, 4, 7, 70, 80, 83, 86)
    pos_end <- c(1, 4, 69, 77, 80, 83, NA)
  }

  c_names <- c(
    "linenum", "level", "title", "ucc", "survey", "factor", "group"
  )

  if (!is.null(hg_file_path)) {
    hg_lines <- readr::read_lines(hg_file_path)
  } else {
    hg_lines <- readr::read_lines(
      unz(
        hg_zip_path,
        stringr::str_c("stubs/CE-HG-", instrument, "-", year, ".txt")
      )
    )
  }

  removals <- which(stringr::str_sub(hg_lines, 1, 6) %in% c("*  UCC", "*  NEW"))

  if (length(removals > 0)) hg_lines <- hg_lines[-removals]

  hg_lines <- stringr::str_replace_all(hg_lines, "[#]", "")

  first_line <- match("1", stringr::str_sub(hg_lines, 1, 1))

  hg_lines <- hg_lines[first_line:length(hg_lines)]

  purrr::map(
    hg_lines,
    \(x) purrr::map2(
      pos_start,
      pos_end,
      \(y, z) stringr::str_sub(x, y, dplyr::if_else(is.na(z), nchar(x), z)) |>
        stringr::str_squish()
    ) |>
      rlang::set_names(c_names) |>
      dplyr::bind_cols()
  ) |>
    dplyr::bind_rows() |>

    # Collapse all multi-line titles down to one line each
    mutate(
      line_group = cumsum(as.numeric(.data$linenum == "1")),
      across(
        all_of(c("level", "ucc", "survey", "factor", "group")),
        \(x) dplyr::na_if(x, "")
      )
    ) |>
    tidyr::fill(
      all_of(c("group", "level", "survey", "ucc", "factor")),
      .direction = "down"
    ) |>
    dplyr::group_by(.data$line_group) |>
    tidyr::nest(.key = "stub_df") |>
    dplyr::mutate(
      data = purrr::map(
        .data$stub_df,
        \(x) x |>
          dplyr::group_by(
            .data$group, .data$level, .data$survey, .data$ucc, .data$factor
          ) |>
          dplyr::summarise(
            title = stringr::str_c(.data$title, collapse = " "),
            .groups = "drop"
          )
      )
    ) |>
    tidyr::unnest(one_of("stub_df")) |>
    dplyr::ungroup() |>
    dplyr::select(!one_of("line_group")) |>

    # Keep only expenditure groups
    dplyr::filter(.data$group %in% c("FOOD", "EXPEND")) |>
    dplyr::select(all_of(c("level", "title", "ucc", "survey", "factor"))) |>
    dplyr::mutate(title = stringr::str_replace_all(.data$title, " #$", ""))
}
