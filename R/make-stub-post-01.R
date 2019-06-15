#' Generate a stub from a year after 2001
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param year Year corresponding to the data
#' @param stub_type_name Stub file type
#'
#' @importFrom readr read_lines write_lines
#' @importFrom dplyr mutate_all row_number case_when

make.stub.post.01 <- function(year, stub_type_name) {
  # Generate a stub for a year after 2001
  path <- switch(
    stub_type_name,
    "integrated" = paste0(
      "https://www.bls.gov/cex/pumd/", year, "/csxintstub.txt"
    ),
    "interview" = paste0(
      "https://www.bls.gov/cex/pumd/", year, "/csxistub.txt"
    ),
    "diary" = paste0(
      "https://www.bls.gov/cex/pumd/", year, "/csxdstub.txt"
    )
  )

  stub_lines <- readr::read_lines(path)

  removals <- which(
    stringr::str_sub(stub_lines, 1, 6) %in% c("*  UCC", "*  NEW")
  )

  if (length(removals > 0)) stub_lines <- stub_lines[-removals]

  tmp <- tempfile()

  readr::write_lines(stub_lines, tmp)

  first_line <- match("1", stringr::str_sub(readr::read_lines(tmp), 1, 1))

  stub <- readr::read_table(
    tmp, col_names = FALSE, guess_max = 300,
    skip = (first_line - 1)
  ) %>%
    dplyr::select(1:7) %>%
    rlang::set_names(
      c("linenum", "level", "title", "ucc", "type", "factor", "group")
    ) %>%
    dplyr::mutate_all(
      list(~(stringr::str_trim(.) %>% stringr::str_squish()))
    ) %>%
    dplyr::mutate(
      rnum = row_number(),
      title = case_when(
        rnum == max(rnum) & .data$linenum == 1 ~ title,
        dplyr::lead(linenum == 2) ~ paste(title, dplyr::lead(title)),
        TRUE ~ title
      ) %>%
        stringr::str_replace(" #$", "")
    ) %>%
    dplyr::filter(
      !.data$linenum %in% "2",
      .data$group %in% c("FOOD", "EXPEND")
    ) %>%
    dplyr::select(.data$level, .data$title, .data$ucc, .data$factor)

  return(stub)
}
