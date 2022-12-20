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
#'
#' @importFrom rlang .data
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
      progress = FALSE
    )
  )

  names(df) <- tolower(names(df))

  if (year >= 1996 & year <= 2011) {
    df <- df %>%
      dplyr::mutate(expnyr = as.integer(stringr::str_sub(.data$qredate, 7, 10)))
  }

  df <- df %>%
    dplyr::select(
      .data$newid, .data$expnyr, .data$ucc, .data$cost, .data$pub_flag
    ) %>%
    dplyr::mutate(
      newid = stringr::str_pad(
        .data$newid, width = 8, side = "left", pad = "0"
      ),
      ucc = stringr::str_pad(.data$ucc, width = 6, side = "left", pad = "0"),
      cost = .data$cost * 13
    )

  if (integrate_data) df <- df %>% dplyr::filter(.data$pub_flag %in% "2")

  df <- df %>%
    dplyr::filter(.data$ucc %in% uccs) %>%
    dplyr::left_join(dplyr::select(hg, .data$ucc, .data$factor), by = "ucc") %>%
    dplyr::mutate(
      cost = .data$cost * as.numeric(as.character(.data$factor))
    ) %>%
    dplyr::group_by(.data$newid, .data$ucc) %>%
    dplyr::summarise(cost = sum(.data$cost), .groups = "drop")

  return(df)
}
