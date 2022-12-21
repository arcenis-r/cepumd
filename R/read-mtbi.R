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

read.mtbi <- function(fp, zp, year, uccs, integrate_data, hg, ce_dir) {

  if (is.null(hg) & integrate_data & year >= 2002) {
    hg <- ce_hg(year, "integrated")
  } else if (is.null(hg)) {
    hg <- ce_hg(year, "interview")
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

  df <- df %>%
    dplyr::select(
      newid, ref_yr, ucc, cost, pubflag
    ) %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        newid, width = 8, side = "left", pad = "0"
      ),
      ucc = stringr::str_pad(ucc, width = 6, side = "left", pad = "0")
    )

  if (integrate_data) df <- df %>% dplyr::filter(pubflag %in% "2")

  df <- df %>%
    dplyr::filter(ref_yr %in% year, ucc %in% uccs) %>%
    dplyr::left_join(
      hg %>% dplyr::select(ucc, factor),
      by = "ucc"
    ) %>%
    dplyr::mutate(
      cost = cost * as.numeric(as.character((factor)))
    ) %>%
    dplyr::group_by(newid, ucc) %>%
    dplyr::summarise(cost = sum(cost), .groups = "drop")

  return(df)
}
