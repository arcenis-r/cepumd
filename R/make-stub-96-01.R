#' Generate a stub from a year between 1996 and 2001
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param year Year corresponding to the data
#' @param stub_type_name Stub file type
#'
#' @importFrom readr read_fwf fwf_widths read_table
#' @importFrom rlang set_names
#' @importFrom rlang .data
#' @importFrom dplyr slice mutate summarise group_by select filter full_join
#'   left_join arrange
#' @importFrom stringr str_replace str_trim str_count
#' @importFrom tidyr drop_na

make.stub.96.01 <- function(year, stub_type_name) {
  if (.data$year %in% 2001) {
    crosswalk_path <- paste0(
      "https://www.bls.gov/cex/pumd/", year, "/pumd_", stub_type_name,
      "_labels.txt"
    )

    labels_path <- paste0(
      "https://www.bls.gov/cex/pumd/", year, "/pumd_", stub_type_name,
      "_ucc_label_crosswalk.txt"
    )
  } else {
    crosswalk_path <- paste0(
      "https://www.bls.gov/cex/pumd/", year, "/pumd_", stub_type_name,
      "_ucc_label_crosswalk.txt"
    )

    labels_path <- paste0(
      "https://www.bls.gov/cex/pumd/", year, "/pumd_", stub_type_name,
      "_labels.txt"
    )
  }

  crosswalk <- readr::read_fwf(
    crosswalk_path,
    readr::fwf_widths(c(6, 51), c("linenum", "header")), trim_ws = TRUE
  ) %>%
    rlang::set_names("linenum", "header") %>%
    dplyr::slice(
      grep("[Aa]nnual expenditures", .data$header):(
        grep("Money income before taxes", .data$header) - 1
      )
    ) %>%
    dplyr::mutate(
      header = stringr::str_pad(.data$header, width = 50, "left") %>%
        stringr::str_replace(" \\.+$", "") %>%
        stringr::str_trim("right"),
      level = ceiling(
        (
          stringr::str_count(.data$header) - stringr::str_count(
            stringr::str_trim(.data$header, "left")
          )
        ) / 3
      ) + 1,
      linenum = as.numeric(.data$linenum)
    )

  stub_labels <- read_table(labels_path, col_names = FALSE) %>%
    rlang::set_names("ucc", "x", "linenum") %>%
    dplyr::select(-.data$x) %>%
    unique() %>%
    dplyr::filter(.data$linenum <= max(crosswalk$linenum)) %>%
    dplyr::group_by(.data$ucc) %>%
    dplyr::summarise(linenum = max(.data$linenum)) %>%
    dplyr::mutate(
      linenum = ifelse(
        !.data$linenum %in% crosswalk$linenum, floor(.data$linenum / 100) * 100,
        .data$linenum
      )
    )

  ucc_data <- get(paste0(stub_type_name, "_uccs_", year), pos = globalenv())

  stub <- dplyr::full_join(
    crosswalk, stub_labels, by = "linenum"
  ) %>%
    dplyr::arrange(.data$linenum) %>%
    tidyr::drop_na(.data$header, .data$level) %>%
    dplyr::left_join(
      ucc_data,
      by = "ucc"
    ) %>%
    dplyr::filter(!(!is.na(.data$ucc) & is.na(.data$title))) %>%
    dplyr::mutate(
      title = ifelse(is.na(.data$ucc), .data$header, .data$title) %>%
        stringr::str_trim()
    ) %>%
    dplyr::mutate(survey = ifelse(stub_type_name == "diary", "D", "I")) %>%
    dplyr::select(
      .data$level, .data$title, .data$ucc, .data$survey, .data$factor
    )

  return(stub)
}
