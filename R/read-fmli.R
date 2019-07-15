#' Read in and modify FMLI files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param year Year
#' @param grp_var_names Variables to keep (intended for grouping)
#'
#' @importFrom readr read_csv
#' @importFrom rlang as_string
#' @importFrom dplyr contains

read.fmli <- function(fp, zp, year, grp_var_names) {

  df <- suppressWarnings(
    readr::read_csv(
      unzip(zp, files = fp, exdir = tempdir()),
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
            "Please review the CE survey documentation to ensure '", g,
            "' is a variable in your dataset (check by year)."
          )
        )
      }
    }

    df <- df %>%
      dplyr::select(
        .data$newid, .data$qintrvyr, .data$qintrvmo, .data$finlwt21,
        dplyr::contains("wtrep"), grp_var_names
      )
  } else {
    df <- df %>%
      dplyr::select(
        .data$newid, .data$qintrvyr, .data$qintrvmo, .data$finlwt21,
        dplyr::contains("wtrep")
      )
  }

  df <- df %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        .data$newid, width = 8, side = "left", pad = "0"
      ),
      qintrvmo = as.integer(.data$qintrvmo),
      qintrvyr = as.integer(.data$qintrvyr),
      mo_scope = ifelse(
        .data$qintrvyr %in% (year + 1), 4 - .data$qintrvmo,
        ifelse(.data$qintrvmo %in% 1:3, .data$qintrvmo - 1, 3)
      ),
      popwt = (.data$finlwt21 / 4) * (.data$mo_scope / 3)
    ) %>%
    dplyr::select(-c(.data$qintrvyr, .data$qintrvmo))

  return(df)
}
