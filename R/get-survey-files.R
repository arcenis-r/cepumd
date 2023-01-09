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
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
#' @param zp_file Character indicating the zip file containing the CE PUMD for a
#' given year
#'
#' @importFrom purrr map map2_df
#' @importFrom dplyr contains filter
#'

get_survey_files <- function(year,
                             survey,
                             file_yrs,
                             qtrs,
                             ce_dir,
                             zp_file) {

  family_abbrev <- ifelse(survey %in% "diary", "fmld", "fmli")
  expenditure_abbrev <- ifelse(survey %in% "diary", "expd", "mtbi")
  survey_short <- ifelse(survey %in% "diary", "diary", "intrvw")

  if (!is.null(zp_file)) {
    print(zp_file)
    print(paste("Files exist?", file.exists(file.path(ce_dir, zp_file))))

    if (sum(file.exists(file.path(ce_dir, zp_file))) == 0) {
      zp_file <- NULL
    } else {
      # Make a dataframe of the required files from all of the zip files
      file_df <- purrr::map(
        file.path(ce_dir, zp_file),
        unzip,
        list = TRUE
      ) %>%
        purrr::map2_df(
          zp_file,
          ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
        )

      print("file_df part 1...")
      print(head(file_df))

      file_df <- file_df %>%
        dplyr::filter(
          stringr::str_detect(
            Name,
            stringr::str_c(qtrs, collapse = "|")
          )
        )

      family_files <- file_df %>%
        dplyr::filter(stringr::str_detect(Name, family_abbrev))

      expenditure_files <- file_df %>%
        dplyr::filter(stringr::str_detect(Name, expenditure_abbrev))

      if (
        nrow(family_files) != length(qtrs) |
        nrow(expenditure_files) != length(qtrs)
      ) {
        zp_file <- NULL
        rm(file_df, family_files, expenditure_files)
      }
    }
  } else {

    ce_download(year = year, survey = {{survey}}, ce_dir = ce_dir)

    # Store a vector of the zip files from which to extract data
    zip_files <- stringr::str_c(survey_short, file_yrs, ".zip")

    # Make a dataframe of the required files from all of the zip files
    file_df <- purrr::map(
      file.path(ce_dir, zip_files),
      unzip,
      list = TRUE
    ) %>%
      purrr::map2_df(
        zip_files,
        ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          Name,
          stringr::str_c(qtrs, collapse = "|")
        )
      )

    family_files <- file_df %>%
      dplyr::filter(stringr::str_detect(Name, family_abbrev))

    expenditure_files <- file_df %>%
      dplyr::filter(stringr::str_detect(Name, expenditure_abbrev))
  }

  print("Getting survey files complete...")

  return(list(family = family_files, expenditure = expenditure_files))
}
