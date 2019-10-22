#' Download CE zip files
#'
#' @description Downloads zip files directly from the CE PUMD page on the
#'     U.S. Bureau of Labor Statistics website to the path (zp) designated by
#'     the user.
#'
#' @param year A year between 1996 and the last year of available CE PUMD.
#' @param survey One of either "interview" or "diary" as a string or symbol.
#' @param zp A string indicating the file path where you'd like to save the zip
#' file. This argment gets passed to 'destfile' in
#' \code{\link[utils]{download.file}}
#'
#' @return Stores a zip file containing CE data in the designated location.
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom stringr str_sub
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

ce_download <- function(year, survey, zp) {
  survey <- rlang::enquo(survey)
  survey_name <- rlang::as_name(survey) %>% tolower()

  ###### Check for bad arguments ######
  if (!year %in% 1996:2018) {
    stop("'year' must be a number between 1996 and 2018")
  }

  if (!survey_name %in% c("interview", "diary")) {
    stop("'survey' must be one of 'interview' or 'diary'")
  }

  survey_name <- ifelse(
    survey_name %in% "interview", "intrvw", "diary"
  )

  zip_link <- paste0(
    "https://www.bls.gov/cex/pumd/data/comma/",
    survey_name, stringr::str_sub(year, 3, 4),
    ".zip"
  )

  download.file(zip_link, zp)
}
