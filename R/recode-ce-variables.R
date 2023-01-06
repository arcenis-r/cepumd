#' Recode variables in interview and diary data
#'
#' @details This is a hidden file called only by exported package functions.
#'
#' @param srvy_data A data frame containing either Interview or Diary data that
#' has been prepped
#' @param code_file A dataframe containing variable names, codes,
#' code descriptions, and other required columns for recoding variables
#' @param srvy The survey instrument to be recoded (this is for filtering
#' the codebook)
#'
#' @importFrom dplyr select filter
#'

recode_ce_variables <- function(srvy_data, code_file, srvy) {
  srvy <- stringr::str_to_upper(srvy)
  recode_vars <- names(srvy_data)[names(srvy_data) %in% code_file$variable]
  recode_vars <- recode_vars[!recode_vars %in% "ucc"]

  ce_codes_srvy <- code_file %>%
    dplyr::filter(survey == srvy, variable %in% recode_vars) %>%
    dplyr::select(variable, code_value, code_description)

  for (i in recode_vars) {
    code_col <- srvy_data[[i]]
    codes_df <- ce_codes_srvy %>%
      dplyr::filter(variable %in% i)

    srvy_data[, i] <- factor(
      code_col,
      levels = codes_df$code_value,
      labels = codes_df$code_description
    )
  }

  srvy_data
}
