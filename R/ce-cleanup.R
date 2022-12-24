#' Clean up directory used to store CE PUMD
#'
#' @description A function to delete the directory containing CE PUMD. It
#' will first check whether the provided directory exists and it will avoid
#' deleting the main temporary directory.
#'
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a \code{ce_cleanup} will look for a directory
#' called "ce_data" to delete it.
#'
#' @export
#'
#' @usage ce_cleanup()
#'

ce_cleanup <- function(ce_dir = NULL) {
  if (!is.null(ce_dir)) {
    if (ce_dir == tempdir()) {
      stop(
        stringr::str_c(
          "Deleting tempdir() can cause errors throughout your R session.",
          " No clean-up attempted."
        )
      )
    }

    if (!dir.exists(ce_dir)) {
      stop("Directory does not exist. No clean-up attempted.")
    }

    unlink(ce_dir, recursive = TRUE)
  } else if (file.exists(file.path(tempdir(), "ce-data"))) {
    unlink(file.path(tempdir(), "ce-data"), recursive = TRUE)
  } else {
    print("Nothing to clean up.")
  }
}
