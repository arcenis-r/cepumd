#' Find UCCs for expenditure categories
#'
#' @param hg A data frame that has, at least, the title, level, and ucc
#' columns of a CE HG file.
#' @param expenditure A string that is an expenditure category contained in a
#' CE HG file (exact match required).
#' @param uccs_only A logical indicating whether to return only the expenditure
#' category's component ucc's. If TRUE (default), a vector of UCC's will be
#' returned. If FALSE, a dataframe will be returned containing the section of
#' the HG file containing the expenditure category and its component sub-
#' categories
#'
#' @return A vector of Universal Classification Codes (UCC's) corresponding to
#' the lowest hierarchical level for that category.
#'
#' @export
#'
#' @examples
#' # First generate an HG file
#' my_hg <- ce_hg(2017, interview)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(my_hg, "Pets")
#' pet_uccs
#' # [1] "610320" "620410" "620420"

ce_uccs <- function(hg, expenditure, uccs_only = TRUE) {
  if (
    !is.data.frame(hg) |
    !all(c("title", "level", "ucc", "factor") %in% names(hg))
  ) {
    stop(
      paste(
        "'hg' requires a valid HG dataframe.",
        "Please generate one using ce_hg()."
      )
    )
  }

  if (!expenditure %in% hg$title) {
    stop(
      paste(
        "The expenditure must be a valid expenditure category from the HG",
        "dataframe's 'title' column and must be spelled exactly as it is in",
        "the HG file."
      )
    )
  }

  title_row <- match(expenditure, hg$title)
  title_row_level <- hg$level[title_row]
  stop_row <- match(
    title_row_level,
    hg$level[(title_row + 1):(nrow(hg) - 1)]
  ) + title_row - 1

  if (is.na(stop_row)) stop_row <- nrow(hg)

  uccs <- hg$ucc[title_row:stop_row]
  uccs <- suppressWarnings(uccs[!is.na(as.numeric(uccs))])

  if (uccs_only) return(uccs) else return(hg[title_row:stop_row, ])
}

