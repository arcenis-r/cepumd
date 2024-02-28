#' Find UCCs for expenditure categories
#'
#' @param hg A data frame that has, at least, the title, level, and ucc
#' columns of a CE HG file.
#' @param expenditure A string that is an expenditure category contained in a
#' CE HG file (exact match required). Either expenditure or ucc_group is
#' required. The default is NULL.
#' @param ucc_group A string indicating an expenditure category by UCC group in
#' a CE HG file (exact match required). Either expenditure or ucc_group is
#' required. The default is NULL.
#' @param uccs_only A logical indicating whether to return only the expenditure
#' category's component ucc's. If TRUE (default), a vector of UCC's will be
#' returned. If FALSE, a dataframe will be returned containing the section of
#' the HG file containing the expenditure category and its component sub-
#' categories
#'
#' @return A vector of Universal Classification Codes (UCC's) corresponding to
#' the lowest hierarchical level for that category.
#'
#' @details If both a valid expenditure and valid ucc_group are input, ucc_group
#' will be used.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First generate an HG file
#' my_hg <- ce_hg(2021, interview, hg_file_path = CE-HG-Inter-2021.txt)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(my_hg, "Pets")
#' pet_uccs
#' # [1] "610320" "620410" "620420"
#' }

ce_uccs <- function(hg,
                    expenditure = NULL,
                    ucc_group = NULL,
                    uccs_only = TRUE) {

  if (
    !is.data.frame(hg) |
    !all(c("title", "level", "ucc", "factor") %in% names(hg))
  ) {
    stop(
      stringr::str_c(
        "'hg' requires a valid HG dataframe.",
        "Please generate one using ce_hg().",
        sep = " "
      )
    )
  }

  if (!is.null(ucc_group)) {
    if (!ucc_group %in% hg$ucc) {ucc_group <- NULL}
  }

  if (!is.null(expenditure)) {
    if (!expenditure %in% hg$title) {expenditure <- NULL}
  }


  if (is.null(ucc_group)) {

    if (is.null(expenditure)) {
      stop(
        paste(
          "Either a valid 'expenditure' or valid 'ucc_group' is required and",
          "it must match exactly the spelling in the HG file corresponding",
          "column. Please see details in the ce_ucc() documentation."
        )
      )
    } else if (length(stringr::str_which(hg$title, expenditure)) > 1) {
      warning(
        stringr::str_c(
          "Multiple expenditure matches found. Either the expenditure that",
          "matches exactly or the first match in order will be used.",
          "To explicitly select the UCC group, please re-run ce_uccs() with",
          "a UCC group that corresponds to the expenditure to limit matches.",
          sep = " "
        )
      )

      title_row <- match(expenditure, hg$title)
    } else {
      title_row <- match(expenditure, hg$title)
    }

  } else if (length(stringr::str_which(hg$ucc, ucc_group)) > 1) {

    if (is.null(expenditure)) {
      warning(
        stringr::str_c(
          "Multiple expenditure matches found. Either the expenditure that",
          "matches exactly or the first match in order will be used.",
          "To explicitly select the UCC group, please re-run ce_uccs() with",
          "a UCC group that corresponds to the expenditure to limit matches.",
          sep = " "
        )
      )

      title_row <- match(expenditure, hg$title)
    } else {
      combos <- hg |>
        dplyr::mutate(hg_row = dplyr::row_number()) |>
        dplyr::filter(.data$ucc %in% ucc_group, .data$title %in% expenditure)

      if (nrow(combos) == 1) {
        title_row <- dplyr::pull(combos, .data$hg_row)
      } else {
        stop(
          stringr::str_c(
            "The combination of 'ucc_group' and 'expenditure' do not yield a",
            "unique combination. Please ensure that 'ucc_group' and",
            "'expenditure' come from the same line in the HG dataframe.",
            sep = " "
          )
        )
      }
    }

  } else {
    title_row <- match(ucc_group, hg$ucc)
  }


  title_row_level <- hg$level[title_row]
  stop_row <- match(
    TRUE,
    title_row_level >= hg$level[(title_row + 1):(nrow(hg))]
  ) + title_row - 1

  if (is.na(stop_row)) stop_row <- nrow(hg)

  uccs <- hg$ucc[title_row:stop_row]
  uccs <- suppressWarnings(uccs[!is.na(as.numeric(uccs))])

  if (uccs_only) return(uccs) else return(hg[title_row:stop_row, ])
}

