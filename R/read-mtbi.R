#' Read in and modify MTBI files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param year Year
#' @param uccs Vector of UCC's to filter for
#' @param integrate_data Whether to prepare data for integrated estimates
#' @param hg Hierarchical grouping data
#'
#' @importFrom dplyr ungroup

read.mtbi <- function(fp, zp, year, uccs, integrate_data, hg) {

  df <- suppressWarnings(
    readr::read_csv(
      unz(zp, fp),
      na = c("NA", "", " ", "."),
      progress = FALSE,
      show_col_types = FALSE
    )
  )

  names(df) <- tolower(names(df))

  df <- df |>
    dplyr::select(
      tidyselect::all_of(
        c("newid", "ref_yr", "ref_mo", "ucc", "cost", "pubflag")
      )
    ) |>
    dplyr::mutate(
      newid = stringr::str_pad(
        .data$newid,
        width = 8,
        side = "left",
        pad = "0"
      ),
      ucc = stringr::str_pad(.data$ucc, width = 6, side = "left", pad = "0"),
      dplyr::across(tidyselect::all_of(c("ref_yr", "ref_mo")), as.numeric)
    )

  if (integrate_data) df <- df |> dplyr::filter(.data$pubflag %in% "2")

  df |>
    dplyr::filter(.data$ref_yr %in% year, .data$ucc %in% uccs) |>
    dplyr::left_join(
      hg |> dplyr::select(tidyselect::all_of(c("ucc", "factor"))),
      by = "ucc"
    ) |>
    dplyr::mutate(
      cost = .data$cost * as.numeric(as.character((.data$factor)))
    ) |>
    dplyr::group_by(.data$newid, .data$ucc, .data$ref_yr, .data$ref_mo) |>
    dplyr::summarise(cost = sum(.data$cost), .groups = "drop")
}
