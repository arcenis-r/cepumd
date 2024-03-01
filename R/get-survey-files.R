#' Generate tables of the necessary survey data files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey One of either interview, diary, or integrated as a character or
#' symbol.
#' @param file_yrs The substrings of years for which to pull data, i.e., for
#' some years files have to be pulled from across different files.
#' @param qtrs The quarters to be included in the analysis for a given year.
#' @param zp_file Character indicating the zip file containing the CE PUMD for a
#' given year
#'
#' @importFrom purrr map map2_df
#' @importFrom dplyr contains filter
#' @importFrom utils unzip

get_survey_files <- function(year, survey, file_yrs, qtrs, zp_file) {

  family_abbrev <- ifelse(survey %in% "diary", "fmld", "fmli")
  expenditure_abbrev <- ifelse(survey %in% "diary", "expd", "mtbi")

  if (!is.null(zp_file)) {
    if (sum(file.exists(zp_file)) == 0) {
      stop(stringr::str_c(zp_file, " does not exist."))
    } else {
      # Make a dataframe of the required files from all of the zip files
      file_df <- purrr::map(
        zp_file,
        \(x) utils::unzip(x, list = TRUE) |> dplyr::mutate(zip_file = x)
      ) |>
        purrr::list_rbind() |>
        dplyr::filter(
          stringr::str_detect(
            .data$Name,
            stringr::str_c(qtrs, collapse = "|")
          )
        )

      family_files <- file_df |>
        dplyr::filter(stringr::str_detect(.data$Name, family_abbrev))

      expenditure_files <- file_df |>
        dplyr::filter(stringr::str_detect(.data$Name, expenditure_abbrev))

      if (
        nrow(family_files) != length(qtrs) ||
          nrow(expenditure_files) != length(qtrs)
      ) {
        warning(
          "The number of files in the zip does not match the number of ",
          "quarters in your analysis."
        )
      }
    }
  }

  list(family = family_files, expenditure = expenditure_files)
}
