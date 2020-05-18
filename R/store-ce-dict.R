#' Download CE Public-Use Microdata Dictionary
#'
#' @description A wrapper for \code{\link[utils]{download.file}} that downloads
#' the CE PUMD dictionary as and Excel file to the path indicated in the
#' \code{hg_zip_path} argument.
#'
#' @param dict_path A valid file path entered as a string to which to store a
#' the CE PUMD Excel dectionary.
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

store_ce_dict <- function(dict_path) {
  download.file(
    "https://www.bls.gov/cex/pumd/ce_pumd_interview_diary_dictionary.xlsx",
    dict_path,
    mode = "wb"
  )
}
