#' Find UCCs for expenditure categories
#'
#' @param stub A data frame that has, at least, the title, level, and ucc
#' columns of a CE stub file.
#' @param expenditure A string that is an expenditure category contained in a
#' CE stub file (exact match required).
#' @param uccs_only A logical indicating whether to return only the expenditure
#' category's component ucc's. If TRUE (default), a vector of UCC's will be
#' returned. If FALSE, a dataframe will be returned containing the section of
#' the stub file containing the expenditure category and its component sub-
#' categories
#'
#' @return A vector of Universal Classification Codes (UCC's) corresponding to
#' the lowest hierarchical level for that category.
#'
#' @export
#'
#' @examples
#' # First generate a stub file
#' mystub <- ce_stub(2017, interview)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(mystub, "Pets")
#' pet_uccs
#' # [1] "610320" "620410" "620420"

ce_uccs <- function(stub, expenditure, uccs_only = TRUE) {
  if (
    !is.data.frame(stub) |
    !all(c("title", "level", "ucc", "factor") %in% names(stub))
  ) {
    stop(
      paste(
        "'stub' requires a valid stub dataframe.",
        "Please generate one using ce_stub()."
      )
    )
  }

  if (!expenditure %in% stub$title) {
    stop(
      paste(
        "The expenditure must be a valid expenditure category from the stub",
        "dataframe's 'title' column and must be spelled exactly as it is in",
        "the stub file."
      )
    )
  }

  title_row <- match(expenditure, stub$title)
  title_row_level <- stub$level[title_row]
  stop_row <- match(
    title_row_level,
    stub$level[(title_row + 1):(nrow(stub) - 1)]
  ) + title_row - 1

  if (is.na(stop_row)) stop_row <- nrow(stub)

  uccs <- stub$ucc[title_row:stop_row]
  uccs <- suppressWarnings(uccs[!is.na(as.numeric(uccs))])

  if (uccs_only) return(uccs) else return(stub[title_row:stop_row, ])
}

