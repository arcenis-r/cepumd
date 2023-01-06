#' Read in and modify EXPD files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param year Year
#' @param uccs Vector of UCC's to filter for
#' @param integrate_data Whether to prepare data for integrated estimates
#' @param hg Hierarchical grouping data

#' @importFrom dplyr left_join

read.expd <- function(fp, zp, year, uccs, integrate_data, hg, ce_dir) {

  if (is.null(hg) & integrate_data & year >= 2002) {
    hg <- ce_hg(year, "integrated")
  } else if (is.null(hg)) {
    hg <- ce_hg(year, "diary")
  }

  df <- suppressWarnings(
    readr::read_csv(
      unzip(zp, files = fp, exdir = ce_dir),
      na = c("NA", "", " ", "."),
      progress = FALSE,
      show_col_types = FALSE
    )
  )

  names(df) <- tolower(names(df))

  if (year >= 1996 & year <= 2011) {
    df <- df %>%
      dplyr::mutate(
        expnyr = as.integer(stringr::str_sub(qredate, 7, 10)),
        expnmo = as.integer(stringr::str_sub(qredate, 3, 4))
      )
  }

  df <- df %>%
    dplyr::select(
      newid, ref_yr = "expnyr", ref_mo = "expnmo", ucc, cost, pub_flag
    ) %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        newid, width = 8, side = "left", pad = "0"
      ),
      ucc = stringr::str_pad(ucc, width = 6, side = "left", pad = "0"),
      cost = cost * 13,
      dplyr::across(c(ref_yr, ref_mo), as.numeric)
    )

  if (integrate_data) df <- df %>% dplyr::filter(pub_flag %in% "2")

  df <- df %>%
    dplyr::filter(ref_yr %in% year, ucc %in% uccs) %>%
    dplyr::left_join(dplyr::select(hg, ucc, factor), by = "ucc") %>%
    dplyr::mutate(
      cost = cost * as.numeric(as.character(factor))
    ) %>%
    dplyr::group_by(newid, ucc, ref_yr, ref_mo) %>%
    dplyr::summarise(cost = sum(cost), .groups = "drop")

  return(df)
}
