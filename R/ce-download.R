#' Download CE zip files
#'
#' @description Downloads zip files directly from the CE PUMD page on the
#'     U.S. Bureau of Labor Statistics website to the path (zp) designated by
#'     the user.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey One of either "interview" or "diary" as a character or symbol.
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session. This argment gets passed to 'destfile' in
#' \code{\link[utils]{download.file}}.
#'
#' @return Stores a zip file containing CE data in the designated directory The
#' zip file is named using the survey label and year, so, for example, the zip
#' file for the 2020 Interview files will be named "intrvw20.zip" following the
#' convention of CE PUMD.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom utils download.file unzip
#'
#' @examples
#' # 'survey' can be entered as a string (not run)
#' \dontrun{
#' ce_download(2017, "diary", "./diary17.zip")
#' }
#'
#' # 'survey' can also be entered as a symbol (not run)
#' \dontrun{
#' ce_download(2017, interview, "./interview17.zip")
#' }

ce_download <- function(year, survey, ce_dir = NULL) {
  survey <- rlang::enquo(survey)
  survey_name <- rlang::as_name(survey) %>% tolower()

  ###### Check for bad arguments ######
  max_year <- max(ce_pumd_years())

  if (!year %in% 1997:max_year) {
    stop(
      stringr::str_c("'year' must be a number between 1997 and ", max_year, ".")
    )
  }

  if (!survey_name %in% c("interview", "diary")) {
    stop("'survey' must be one of 'interview' or 'diary'")
  }

  # Ensure that there's a directory to put files into
  if (is.null(ce_dir)) {
    if (!file.exists(file.path(tempdir(), "ce-data"))) {
      dir.create(file.path(tempdir(), "ce-data"))
    }

    ce_dir <- file.path(tempdir(), "ce-data")
  }

  survey_name <- ifelse(
    survey_name %in% "interview", "intrvw", "diary"
  )

  # Create a vector of years for which files need to be downloaded
  if (survey_name %in% "intrvw" & year >= 2020) {
    dl_yrs <- stringr::str_sub(c(year - 1, year), 3, 4)
  } else {
    dl_yrs <- stringr::str_sub(year, 3, 4)
  }

  # Create a vector of the required quarters for the given year
  if (survey_name %in% "intrvw") {
    qtrs <- c(
      stringr::str_c(stringr::str_sub(year, 3, 4), 1:4),
      stringr::str_c(stringr::str_sub((year + 1), 3, 4), 1)
    )
  } else {
    qtrs <- stringr::str_c(stringr::str_sub(year, 3, 4), 1:4)
  }

  # Store a vector of the files to download
  dl_files <- stringr::str_c(survey_name, dl_yrs, ".zip")

  # Store a vector of the links from which to download data
  zip_link <- stringr::str_c(
    "https://www.bls.gov/cex/pumd/data/comma/",
    dl_files
  )

  # Download the required CE files
  invisible(
    purrr::map2(
      zip_link,
      file.path(ce_dir, dl_files),
      download.file,
      method = "auto",
      mode = "wb"
    )
  )
}
