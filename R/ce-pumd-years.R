#' Get a vector of years for which PUMD are available
#'
#' @description Scrapes the CE PUMD website for the table of CSV files and
#' pulls the column of that table containing the years.
#'
#' @usage ce_pumd_years()
#'
#' @return A numeric vector (years)
#'
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_attrs
#' @importFrom rvest html_table
#' @importFrom purrr flatten
#' @importFrom purrr pluck
#' @importFrom stringr str_which
#' @importFrom rlang .data

ce_pumd_years <- function() {

  # Read in the page containing the CE PUMD
  ce_html <- rvest::read_html("https://www.bls.gov/cex/pumd_data.htm")

  # Get the position of the CSV table
  csv_tbl_num <- ce_html %>%
    rvest::html_elements(".cex .panes h4") %>%
    rvest::html_attrs() %>%
    purrr::flatten_chr() %>%
    stringr::str_which("#csv")

  # Get the available years of CE PUMD
  ce_html %>%
    rvest::html_elements(".cex .panes h4+ table") %>%
    purrr::pluck(csv_tbl_num) %>%
    rvest::html_table() %>%
    dplyr::mutate(X1 = readr::parse_number(.data$X1)) %>%
    dplyr::filter(.data$X1 >= 1997) %>%
    dplyr::pull(.data$X1)
}
