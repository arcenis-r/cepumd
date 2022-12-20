#' Download CE hierarchical grouping files
#'
#' @description A wrapper for \code{\link[utils]{download.file}} that downloads
#' a zip file containing all CE hieararchical grouping files (also known as
#' 'stub' files) to the path indicated in the \code{hg_zip_path} argument.
#'
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
#' @param hg_zip_path A valid file path entered as a character to which to store
#' a zip file containing the hierarchical grouping files. If \code{NULL} (the
#' default) a file called "ce-stubs.zip" will be created in \code{ce_dir}.
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


store_ce_hg <- function(ce_dir = NULL, hg_zip_path = NULL) {

  # Ensure that there's a directory to put files into
  if (is.null(ce_dir)) {
    if (!file.exists(file.path(tempdir(), "ce-data"))) {
      dir.create(file.path(tempdir(), "ce-data"))
    }

    ce_dir <- file.path(tempdir(), "ce-data")
  }

  if (is.null(hg_zip_path)) {
    hg_zip_path <- "ce-stubs.zip"
  }

  download.file(
    "https://www.bls.gov/cex/pumd/stubs.zip",
    file.path(ce_dir, hg_zip_path),
    method = "auto",
    mode = "wb"
  )
}
