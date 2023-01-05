#' Read in and modify FMLD files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional variables to keep
#' (intended for grouping)
#'

read.fmld <- function(fp, zp, ce_dir, ...) {
  grp_vars <- rlang::ensyms(...)

  df <- suppressWarnings(
    readr::read_csv(
      unzip(zp, files = fp, exdir = ce_dir),
      na = c("NA", "", " ", "."),
      progress = FALSE,
      show_col_types = FALSE
    )
  )

  names(df) <- tolower(names(df))

  if (length(grp_vars) > 0) {
    grp_var_names <- purrr::map_chr(
      grp_vars,
      ~ rlang::as_string(.x) %>% stringr::str_to_lower()
    )

    if (length(setdiff(grp_var_names, names(df))) > 0) {
      stop(
        stringr::str_c(
          "The following are not valid variable names: ",
          str_c(grp_var_names, collapse = ", ") %>% stringr::str_to_upper(),
          "\nPlease review the CE PUMD documentation and select valid variables."
        )
      )
    }
  }

  df <- df %>%
    dplyr::select(
      newid, finlwt21, dplyr::contains("wtrep"), !!!grp_vars
    ) %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        newid, width = 8, side = "left", pad = "0"
      ),
      mo_scope = 3,
      popwt = (finlwt21 / 4) * (mo_scope / 3)
    )

  return(df)
}
