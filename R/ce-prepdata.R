#' Prepare CE data for calculating an estimated mean or median
#'
#' @description Reads in the family characteristics (FMLI/-D) and expenditure
#' tabulation (MTBI/EXPD) files and merges the relevant data for calculating a
#' weighted mean or median.
#'
#' @param year A year between 1997 and the last year of available CE PUMD.
#' @param survey One of either interview, diary, or integrated as a string or
#' symbol.
#' @param uccs A character vector of UCCs corresponding to expenditure
#' categories in the hierarchical grouping (HG) for a given year and survey.
#' @param recode_variables A logical indicating whether to recode all coded
#' variables except 'UCC' using the codes in the CE's excel dictionary which can
#' be downloaded from the
#' \href{https://www.bls.gov/cex/pumd_doc.htm}{CE Documentation Page}
#' @param dict_path A string indicating the path where the CE PUMD dictionary
#' is stored if already stored. If the file does not exist and
#' \code{recode_variables = TRUE} the dictionary will be stored in this path.
#' The default is \code{NULL} which causes the zip file to be stored in
#' temporary memory during function operation.
#' @param int_zp String indicating the path of the Interview data zip file if
#' already stored. If the file does not exist its corresponding zip file will
#' be stored in that path. The default is \code{NULL} which causes the zip file
#' to be stored in temporary memory during function operation.
#' @param dia_zp Same as \code{int_zip} above, but for Diary data.
#' @param hg A data frame that has, at least, the title, level, ucc, and
#' factor columns of a CE HG file. Calling \code{\link{ce_hg}} will generate a
#' valid HG file.
#' @param ... Variables to include in the dataset from the family
#' characteristics file. This is intended to allow the user to calculate
#' estimates for subsets of the data.
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item newid - A consumer unit (CU), or household, identifier
#'   \item finlwt21 - CU weight variable
#'   \item wtrep01 through wtrep44 - CU replicate weight variables (see details)
#'   \item ... - Any family characteristics variables that were kept
#'   \item mo_scope - Months in scope (see details)
#'   \item popwt - An adjusted weight meant to account for the fact that a CUs
#'         value of finlwt21 is meant to be representative of only 1 quarter of
#'         data (see details)
#'   \item ucc - The UCC for a given expenditure
#'   \item cost - The value of the expenditure (in U.S. Dollars)
#'   \item survey - An indicator of which survey the data come from: "I" for
#'         Interview and "D" for Diary.
#' }
#'
#' @details
#' CE microdata include 45 weights. The primary weight that is used for
#' calculating estimated means and medians is finlwt21. The 44 replicate weights
#' are computed using Balanced Repeated Replication (BRR) and are used for
#' calculating weighted standard errors.
#'
#' "Months in scope" refers to the proportion of the data collection quarter for
#' which a CU reported expenditures. For the Diary survey the months in scope is
#' always 3 because the expenditure data collected are meant to be reported for
#' the quarter in which they are collected. The Interview Survey, on the other
#' hand, is a quarterly, rolling, recall survey and the CU's report expenditures
#' for the 3 months previous to the month in which the data are collected. For
#' example, if a CU was interviewed in February 2017, then they would be
#' providing data for November 2016, December 2016, and January 2017. If one is
#' calculating a weighted estimated mean for the 2017 calendar year, then only
#' the January 2017 data would be "in scope."
#'
#' CE data are reported quarterly, but the sum of the weights (finlwt21) is
#' for all CU's is meant to represent the total number of U.S. CU's for a given
#' year. Since a calculating a calendar year estimate requires the use of 4
#' quarters of data and the sum of the weights in each quarter equals the
#' number of households in the U.S. for a given year, adding up the sums of the
#' weights in the 4 quarters of data would yield a total number of households
#' that is approximately 4 times larger than the actual number of households in
#' the U.S. in the corresponding year.
#'
#' Since some UCC's can appear in both surveys, for the purposes of integration,
#' the CE has a source selection procedure by which to choose which source data
#' will be taken from for a given UCC. For example, of the 4 UCC's in the "Pets"
#' category in 2017 two were sourced for publication from the Diary and two from
#' the Interview. Please download the CE Source Selection Document for a
#' complete listing: \url{https://www.bls.gov/cex/ce_source_integrate.xlsx}.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom readxl excel_sheets
#' @importFrom readxl cell_cols
#' @importFrom dplyr select
#' @importFrom dplyr filter
#'
#' @examples
#' # The following workflow will prepare a dataset for calculating diary
#' # pet expenditures for 2017 keep the "sex_ref" variable in the data to
#' # potentially calculate means by sex of the reference person.
#'
#' # First generate an HG file
#' my_hg <- ce_hg(2017, diary)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(my_hg, "Pets")
#'
#' # Store the diary data (not run)
#' \dontrun{
#' pets_dia <- ce_prepdata(
#'   year = 2017,
#'   survey = diary,
#'   uccs = pet_uccs,
#'   integrate_data = TRUE,
#'   recode_variables,
#'   zp = NULL,
#'   hg = my_hg,
#'   sex_ref
#' )
#' }

# !diagnostics suppress = last_year, first_year, variable_name, code_value
# !diagnostics suppress = code_description
ce_prepdata <- function(year,
                        survey,
                        uccs,
                        recode_variables = FALSE,
                        dict_path = NULL,
                        int_zp = NULL,
                        dia_zp = NULL,
                        hg = NULL,
                        ...) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_string(survey) %>% tolower()

  grp_vars <- rlang::ensyms(...)
  grp_var_names <- purrr::map(grp_vars, rlang::as_string) %>% unlist()

  unlink_int_zp <- is.null(int_zp)
  unlink_dia_zp <- is.null(dia_zp)
  unlink_dict_path <- is.null(dict_path)

  if (!year %in% 1997:2019) {
    stop("'year' must be a number between 1997 and 2019")
  }

  if (!survey_name %in% c("interview", "diary", "integrated")) {
    stop("'survey' must be one of 'interview,' 'diary,' or 'integrated.'")
  }

  if (length(uccs) > 0 & is.character(uccs)) {
    for (u in uccs) {
      if (is.na(as.numeric(u)) | nchar(u) != 6) {
        stop(
          paste0(
            "'", u, "' is not a valid UCC. Please review the CE PUMD",
            " documentation."
          )
        )
      }
    }
  } else {
    stop("Please enter a valid UCC")
  }

  if (!is.null(hg)) {
    if (
      !is.data.frame(hg) |
      !all(c("title", "level", "ucc", "factor") %in% names(hg))
    ) {
      stop(
        paste(
          "'hg' requires a valid HG dataframe. Please generate one using",
          "ce_hg()."
        )
      )
    }
  }

  integrate_data <- ifelse(survey_name == "integrated", TRUE, FALSE)

  if (recode_variables == TRUE) {
    if (is.null(dict_path)) {
      dict_path <- tempfile()
      store_ce_dict(dict_path = dict_path)
    } else if (isFALSE(file.exists(dict_path))) {
      store_ce_dict(dict_path = dict_path)
    }
  }

  if (recode_variables) {
    code_sheet <- grep("^Codes", readxl::excel_sheets(dict_path), value = TRUE)

    ce_dict <- readxl::read_excel(
      dict_path,
      sheet = code_sheet,
      range = readxl::cell_cols("A:J"),
      guess_max = 4000
    )

    names(ce_dict) <- tolower(names(ce_dict)) %>%
      stringr::str_replace(" ", "_")

    ce_dict <- ce_dict %>%
      dplyr::mutate(
        survey = substr(.data$survey, 1, 1),
        variable = tolower(.data$variable),
        last_year = tidyr::replace_na(
          .data$last_year,
          max(.data$last_year, na.rm = TRUE)
        )
      ) %>%
      dplyr::filter(
        .data$first_year <= year,
        .data$last_year >= year,
      )
  }

  if (survey_name %in% c("interview", "integrated")) {
    if (is.null(int_zp)) {
      int_zp <- tempfile()
      ce_download(year = year, survey = "interview", zp = int_zp)
    } else if (isFALSE(file.exists(int_zp))) {
      ce_download(year = year, survey = "interview", zp = int_zp)
    }

    fmli_files <- grep(
      "fmli",
      unzip(int_zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    mtbi_files <- grep(
      "mtbi",
      unzip(int_zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    fmli <- purrr::map_df(
      fmli_files,
      read.fmli,
      zp = int_zp,
      year = year,
      grp_var_names = grp_var_names
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("wtrep")), list(~ replace(., is.na(.), 0))
      )

    mtbi <- purrr::map_df(
      mtbi_files,
      read.mtbi,
      zp = int_zp,
      year = year,
      uccs = uccs,
      integrate_data = integrate_data,
      hg = hg
    ) %>%
      dplyr::bind_rows()

    interview <- dplyr::left_join(fmli, mtbi, by = "newid") %>%
      dplyr::mutate(cost = replace(.data$cost, is.na(.data$cost), 0)) %>%
      dplyr::mutate(survey = "I")

    if (recode_variables) {
      recode_vars <- names(interview)[names(interview) %in% ce_dict$variable]
      recode_vars <- recode_vars[!recode_vars %in% "ucc"]

      ce_dict_int <- ce_dict %>%
        dplyr::filter(
          .data$survey == "I"
        )

      for (i in recode_vars) {
        code_col <- interview[[i]]
        codes_df <- ce_dict_int %>%
          dplyr::filter(.data$variable %in% i) %>%
          dplyr::select(.data$code_value, .data$code_description)

        interview[, i] <- factor(
          code_col,
          levels = codes_df$code_value,
          labels = codes_df$code_description
        )
      }
    }
  }

  if (survey_name %in% c("diary", "integrated")) {
    if (is.null(dia_zp)) {
      dia_zp <- tempfile()
      ce_download(year = year, survey = "diary", zp = dia_zp)
    } else if (isFALSE(file.exists(dia_zp))) {
      ce_download(year = year, survey = "diary", zp = dia_zp)
    }

    fmld_files <- grep(
      "fmld",
      unzip(dia_zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    expd_files <- grep(
      "expd",
      unzip(dia_zp, ".", list = TRUE)$Name,
      value = TRUE
    )

    fmld <- purrr::map_df(
      fmld_files,
      read.fmld,
      zp = dia_zp,
      grp_var_names = grp_var_names
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("wtrep")), list(~ replace(., is.na(.), 0))
      )

    expd <- purrr::map_df(
      expd_files,
      read.expd,
      zp = dia_zp,
      year = year,
      uccs = uccs,
      integrate_data = integrate_data,
      hg = hg
    ) %>%
      dplyr::bind_rows()

    diary <- dplyr::left_join(fmld, expd, by = "newid") %>%
      dplyr::mutate(cost = replace(.data$cost, is.na(.data$cost), 0)) %>%
      dplyr::mutate(survey = "D")

    if (recode_variables) {
      recode_vars <- names(diary)[names(diary) %in% ce_dict$variable]
      recode_vars <- recode_vars[!recode_vars %in% "ucc"]

      ce_dict_dia <- ce_dict %>%
        dplyr::filter(
          .data$survey == "D"
        )

      for (i in recode_vars) {
        code_col <- diary[[i]]
        codes_df <- ce_dict_dia %>%
          dplyr::filter(.data$variable %in% i) %>%
          dplyr::select(.data$code_value, .data$code_description)

        diary[, i] <- factor(
          code_col,
          levels = codes_df$code_value,
          labels = codes_df$code_description
        )
      }
    }
  }

  if (unlink_int_zp) unlink(int_zp)
  if (unlink_dia_zp) unlink(dia_zp)
  if (unlink_dict_path) unlink(dict_path)

  if (survey_name == "integrated") {
    return(dplyr::bind_rows(interview, diary))
  } else if (survey_name == "interview") {
    return(interview)
  } else if ( survey_name == "diary") {
    return(diary)
  }
}
