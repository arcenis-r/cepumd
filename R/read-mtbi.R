#' Read in and modify MTBI files
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param fp File to extract from zip file
#' @param zp Zip file path
#' @param year Year
#' @param uccs Vector of UCC's to filter for
#' @param integrate_data Whether to prepare data for integrated estimates
#' @param stub Stub file data
#'
#' @importFrom dplyr ungroup

read.mtbi <- function(fp, zp, year, uccs, integrate_data, stub) {

  if (is.null(stub) & integrate_data & year >= 2002) {
    stub <- ce_stub(year, "integrated")
  } else if (is.null(stub)) {
    stub <- ce_stub(year, "interview")
  }

  df <- readr::read_csv(
    unzip(zp, files = fp, exdir = tempdir()),
    na = c("NA", "", " ", "."),
    progress = FALSE
  ) %>%
    rlang::set_names(tolower(names(.))) %>%
    dplyr::select(newid, ref_yr, ucc, cost, pubflag) %>%
    dplyr::mutate(
      newid = stringr::str_pad(newid, width = 8, side = "left", pad = "0"),
      ucc = stringr::str_pad(ucc, width = 6, side = "left", pad = "0")
    )

  if (integrate_data) df <- df %>% dplyr::filter(pubflag %in% "2")

  df <- df %>%
    dplyr::filter(ref_yr %in% year, ucc %in% uccs) %>%
    dplyr::left_join(stub %>% dplyr::select(ucc, factor), by = "ucc") %>%
    dplyr::mutate(cost = cost * as.numeric(as.character((factor)))) %>%
    dplyr::group_by(newid, ucc) %>%
    dplyr::summarise(cost = sum(cost)) %>%
    dplyr::ungroup()

  return(df)
}
