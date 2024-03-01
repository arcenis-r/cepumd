#' Read in and modify FMLI files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path within ce_dir
#' @param year Year
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional variables to keep
#' (intended for grouping)
#'
#' @importFrom readr read_csv
#' @importFrom rlang as_string
#' @importFrom dplyr contains
#'

read.fmli <- function(fp, zp, year, ...) {
  grp_vars <- rlang::ensyms(...)

  grp_var_names <- purrr::map_chr(
    grp_vars,
    \(x) rlang::as_string(x) |> stringr::str_to_lower()
  )

  df <- suppressWarnings(
    readr::read_csv(
      unz(zp, fp),
      na = c("NA", "", " ", "."),
      progress = FALSE,
      show_col_types = FALSE
    )
  )

  names(df) <- tolower(names(df))

  if (length(grp_vars) > 0) {
    if (length(setdiff(grp_var_names, names(df))) > 0) {
      stop(
        stringr::str_c(
          "The following are not valid variable names: ",
          str_c(grp_var_names, collapse = ", ") |> stringr::str_to_upper(),
          "\nPlease review the CE PUMD documentation for valid variables."
        )
      )
    }
  }

  df |>
    dplyr::select(
      tidyselect::all_of(c("newid", "qintrvyr", "qintrvmo", "finlwt21")),
      tidyselect::contains("wtrep"),
      tidyselect::any_of(grp_var_names)
    ) |>
    dplyr::mutate(
      newid = stringr::str_pad(
        .data$newid,
        width = 8,
        side = "left",
        pad = "0"
      ),
      qintrvmo = as.integer(.data$qintrvmo),
      qintrvyr = as.integer(.data$qintrvyr),
      dplyr::across(
        c(tidyselect::one_of("finlwt21"), tidyselect::contains("wtrep")),
        as.numeric
      ),
      dplyr::across(tidyselect::any_of(grp_var_names), as.character),
      mo_scope = dplyr::case_when(
        .data$qintrvyr %in% (year + 1) ~ 4 - .data$qintrvmo,
        .data$qintrvmo %in% 1:3 ~ .data$qintrvmo - 1,
        .default = 3
      ),
      popwt = (.data$finlwt21 / 4) * (.data$mo_scope / 3),
      dplyr::across(tidyselect::contains("wtrep"), \(x) tidyr::replace_na(x, 0))
    ) |>
    dplyr::select(!tidyselect::any_of(c("qintrvyr", "qintrvmo")))
}
