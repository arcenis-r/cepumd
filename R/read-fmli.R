#' Read in and modify FMLI files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path within ce_dir
#' @param year Year
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
#' @param grp_var_names Variables to keep (intended for grouping)
#'
#' @importFrom readr read_csv
#' @importFrom rlang as_string
#' @importFrom dplyr contains
#'

read.fmli <- function(fp, zp, year, ce_dir, grp_var_names) {

  df <- suppressWarnings(
    readr::read_csv(
      unzip(zp, files = fp, exdir = ce_dir),
      na = c("NA", "", " ", "."),
      progress = FALSE
    )
  )

  names(df) <- tolower(names(df))

  if (length(grp_var_names) > 0) {

    for (g in grp_var_names) {
      if (!rlang::as_string(g) %in% names(df)) {
        stop(
          paste0(
            "'", g, "' is not a valid variable. ",
            "Please review the CE PUMD documentation"
          )
        )
      }
    }

    df <- df %>%
      dplyr::select(
        newid, qintrvyr, qintrvmo, finlwt21,
        dplyr::contains("wtrep"), grp_var_names
      )
  } else {
    df <- df %>%
      dplyr::select(
        newid, qintrvyr, qintrvmo, finlwt21,
        dplyr::contains("wtrep")
      )
  }

  df <- df %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        newid, width = 8, side = "left", pad = "0"
      ),
      qintrvmo = as.integer(qintrvmo),
      qintrvyr = as.integer(qintrvyr),
      mo_scope = ifelse(
        qintrvyr %in% (year + 1), 4 - qintrvmo,
        ifelse(qintrvmo %in% 1:3, qintrvmo - 1, 3)
      ),
      popwt = (finlwt21 / 4) * (mo_scope / 3)
    ) %>%
    dplyr::select(-c(qintrvyr, qintrvmo))

  return(df)
}
