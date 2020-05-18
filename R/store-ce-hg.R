#' Download CE hierarchical grouping files
#'
#' @description A wrapper for \code{\link[utils]{download.file}} that downloads
#' a zip file containing all CE hieararchical grouping files (also known as
#' 'stub' files) to the path indicated in the \code{hg_zip_path} argument.
#'
#' @param hg_zip_path A valid file path entered as a string to which to store a
#' zip file containing the hierarchical grouping files.
#'
#' @details The download comes from
#' \url{https://www.bls.gov/cex/pumd/stubs.zip} to the path designated in
#' \code{hg_zip_path} using \code{method = "wb"}
#'
#' @export
#'
#' @examples
#' # This will download the zip file in the current working directory
#' \dontrun{
#' store_ce_hg("ce-hg-files.zip")
#'
#' # Check the file
#' file.info("ce-hg-files.zip")
#'
#' head(unzip("ce-hg-files.zip", list = TRUE))
#' }


store_ce_hg <- function(hg_zip_path) {
  download.file(
    "https://www.bls.gov/cex/pumd/stubs.zip",
    hg_zip_path,
    mode = "wb"
  )
}
