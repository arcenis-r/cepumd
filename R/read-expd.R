#' Read in and modify EXPD files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param year Year
#' @param uccs Vector of UCC's to filter for
#' @param integrate_data Whether to prepare data for integrated estimates
#' @param stub Stub file data

read.expd <- function(fp, zp, year, uccs, integrate_data, stub) {

  if (is.null(stub) & integrate_data & year >= 2002) {
    stub <- ce_stub(year, "integrated")
  } else if (is.null(stub)) {
    stub <- ce_stub(year, "diary")
  }

  df <- suppressWarnings(
    readr::read_csv(
      unzip(zp, files = fp, exdir = tempdir()),
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

  if (integrate_data) df <- df %>% filter(.data$pub_flag %in% "2")

  df <- df %>%
    filter(.data$ucc %in% uccs) %>%
    left_join(stub %>% select(.data$ucc, .data$factor), by = "ucc") %>%
    dplyr::mutate(
      cost = .data$cost * as.numeric(as.character(.data$factor))
    ) %>%
    dplyr::group_by(.data$newid, .data$ucc) %>%
    dplyr::summarise(cost = sum(.data$cost)) %>%
    dplyr::ungroup()

  return(df)
}
