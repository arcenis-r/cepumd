#' Download CE Public-Use Microdata Dictionary
#'
#' @description A wrapper for \code{\link[utils]{download.file}} that downloads
#' the CE PUMD dictionary as and Excel file to the path indicated in the
#' \code{hg_zip_path} argument.
#'
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
#' @param dict_path A valid file path entered as a string to which to store a
#' the CE PUMD Excel dictionary. If \code{NULL} (the default) a file called
#' "ce-stubs.zip" will be created in \code{ce_dir}.
#'
#' @details The download comes from
#' \url{https://www.bls.gov/cex/pumd/ce_pumd_interview_diary_dictionary.xlsx}
#' to the path designated in \code{hg_zip_path} using \code{method = "wb"}
#'
#' @export
#'
#' @examples
#' # This will download the dictionary in the current working directory
#' \dontrun{
#' store_ce_dict("ce-dict.xlsx")
#'
#' # Check the file
#' file.info("ce-dict.xlsx")
#' }

store_ce_dict <- function(ce_dir = NULL, dict_path = NULL) {

  # Ensure that there's a directory to put files into
  if (is.null(ce_dir)) {
    if (!file.exists(file.path(tempdir(), "ce-data"))) {
      dir.create(file.path(tempdir(), "ce-data"))
    }

    ce_dir <- file.path(tempdir(), "ce-data")
  }

  if (is.null(dict_path)) {
    dict_path <- "ce-dict.xlsx"
  }

  download.file(
    "https://www.bls.gov/cex/pumd/ce_pumd_interview_diary_dictionary.xlsx",
    file.path(ce_dir, dict_path),
    method = "auto",
    mode = "wb"
  )
}
