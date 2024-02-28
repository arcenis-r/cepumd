#' Prepare CE data for calculating an estimated mean or median
#'
#' @description Reads in the family characteristics (FMLI/-D) and expenditure
#' tabulation (MTBI/EXPD) files and merges the relevant data for calculating a
#' weighted mean or median.
#'
#' @param year A year between 1997 and the last year of available CE PUMD.
#' @param survey One of either interview, diary, or integrated as a character or
#' symbol.
#' @param uccs A character vector of UCCs corresponding to expenditure
#' categories in the hierarchical grouping (HG) for a given year and survey.
#' @param hg A data frame that has, at least, the title, level, ucc, and
#' factor columns of a CE HG file. Calling \code{\link{ce_hg}} will generate a
#' valid HG file.
#' @param ... Variables to include in the dataset from the family
#' characteristics file. This is intended to allow the user to calculate
#' estimates for subsets of the data.
#' @param recode_variables A logical indicating whether to recode all coded
#' variables except 'UCC' using the codes in the CE's excel dictionary which can
#' be downloaded from the
#' \href{https://www.bls.gov/cex/pumd_doc.htm}{CE Documentation Page}
#' @param int_zp String indicating the path of the Interview data zip file(s) if
#' already stored. If the file(s) does not exist its corresponding zip file will
#' be stored in that path. The default is \code{NULL} which causes the zip file
#' to be stored in temporary memory during function operation.
#' @param dia_zp Same as \code{int_zp} above, but for Diary data.
#' @param dict_path A string indicating the path where the CE PUMD dictionary
#' is stored if already stored. If the file does not exist and
#' \code{recode_variables = TRUE} the dictionary will be stored in this path.
#' The default is \code{NULL} which causes the zip file to be stored in
#' temporary memory during function operation. Automatically changed to
#' \code{NULL} if a valid input for \code{own_codebook} is given.
#' @param own_codebook An optional data frame containing a user-defined codebook
#' containing the same columns as the CE Dictionary "Codes " sheet. If the input
#' is not a data frame or does not have all of the required columns, the
#' function will give an error message. See details for the required columns.
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
#'   \item ref_yr - The year in which the corresponding expenditure occurred
#'   \item ref_mo - The month in which the corresponding expenditure occurred
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
#' @importFrom readxl excel_sheets
#' @importFrom readxl cell_cols
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' # The following workflow will prepare a dataset for calculating integrated
#' # pet expenditures for 2021 keep the "sex_ref" variable in the data to
#' # potentially calculate means by sex of the reference person.
#'
#' # First generate an HG file
#' my_hg <- ce_hg(2021, integrated, "CE-HG-Inter-2021.txt")
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(my_hg, "Pets")
#'
#' # Store the diary data (not run)
#' pets_dia <- ce_prepdata(
#'   year = 2021,
#'   survey = integrated,
#'   uccs = pet_uccs,
#'   integrate_data = FALSE,
#'   hg = my_hg,
#'   dia_zip = "diary21.zip"
#'   sex_ref
#' )
#' }

# !diagnostics suppress = last_year, first_year, variable_name, code_value
# !diagnostics suppress = code_description
ce_prepdata <- function(year,
                        survey,
                        hg,
                        uccs,
                        ...,
                        int_zp = NULL,
                        dia_zp = NULL,
                        recode_variables = FALSE,
                        dict_path = NULL,
                        own_codebook = NULL) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_string(survey) |> tolower()

  grp_vars <- rlang::ensyms(...)
  grp_var_names <- purrr::map(grp_vars, rlang::as_string) |>
    unlist() |>
    tolower()

  if (year < 1997) {
    stop("cepumd only works with data from 1997 onward.")
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

  if (recode_variables) {
    if(!is.null(own_codebook)) {
      if (
        !is.data.frame(own_codebook) |
        !all(
          c(
            "survey", "file", "variable", "code_value", "code_description",
            "first_year", "first_quarter", "last_year", "last_quarter"
          ) %in%
          names(janitor::clean_names(own_codebook))
        )
      ) {
        stop(
          stringr::str_c(
            "Your codebook either is not a data frame or does not have ",
            "the required columns. It should have:\n",
            "survey, file, variable, code_value, code_description, ",
            "first_year, first_quarter, last_year, last_quarter"
          )
        )
      }

      if(!all({{grp_var_names}} %in% tolower(own_codebook$variable))) {
        warning(
          "Some of your grouping variable(s) is (are) were not found in your.",
          "codebook. Only variables found in the codebook will be recoded."
        )
      }

      ce_codes <- own_codebook |>
        dplyr::mutate(
          variable = stringr::str_to_lower(.data$variable),
          survey = stringr::str_to_upper(.data$survey) |> stringr::str_sub(1, 1)
        )

      rm(dict_path)
    } else {
      if (is.null(dict_path)) {
        stop(
          "Please provide a valid file path to your codebook (CE Dictionary) ",
          "in order to recode variables."
        )
      } else if (isFALSE(file.exists(dict_path))) {
        stop(
          "Please provide a valid file path to your codebook (CE Dictionary) ",
          "in order to recode variables."
        )
      }

      code_sheet <- grep(
        "^Codes",
        readxl::excel_sheets(dict_path),
        value = TRUE
      )

      ce_codes <- suppressWarnings(
        readxl::read_excel(
          dict_path,
          sheet = code_sheet,
          range = readxl::cell_cols("A:J"),
          guess_max = 4000
        )
      ) |>
        janitor::clean_names() |>
        dplyr::mutate(
          survey = stringr::str_sub(.data$survey, 1, 1),
          variable = stringr::str_to_lower(.data$variable),
          last_year = dplyr::if_else(
            is.na(.data$last_year),
            max(.data$last_year, na.rm = TRUE),
            .data$last_year
          )
          # last_year = tidyr::replace_na(
          #   .data$last_year,
          #   max(.data$last_year, na.rm = TRUE)
          # )
        ) |>
        dplyr::filter(
          .data$first_year <= year,
          .data$last_year >= year,
        ) |>
        dplyr::group_by(
          .data$survey, .data$file, .data$variable, .data$code_value
        ) |>
        dplyr::slice_max(.data$first_year, n = 1, with_ties = FALSE) |>
        dplyr::slice_max(.data$first_quarter, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
    }
  }  # end "if (recode_variables)"

  if (is.null(int_zp) & is.null(dia_zp)) {
    stop(
      "You must provide at least 1 zip file with data for either 'dia_zip' or ",
      "'int_zip'. In previous versions of 'cepumd' can no longer download ",
      "data automatically."
    )
  }

  integrate_data <- ifelse(survey_name == "integrated", TRUE, FALSE)

  if (survey_name %in% c("interview", "integrated")) {
    # Create a vector of years for which data are required
    if (year >= 2020) {
      int_yrs <- stringr::str_sub(c(year - 1, year), 3, 4)
    } else {
      int_yrs <- stringr::str_sub(year, 3, 4)
    }

    # Create a vector of the required quarters for the given year(s)
    int_qtrs <- c(
      stringr::str_c(stringr::str_sub(year, 3, 4), 1:4),
      stringr::str_c(stringr::str_sub((year + 1), 3, 4), 1)
    )

    interview_files <- get_survey_files(
      year = year,
      survey = "interview",
      file_yrs = int_yrs,
      qtrs = int_qtrs,
      zp_file = int_zp
    )

    fmli <- purrr::map2_df(
      interview_files$family$Name,
      interview_files$family$zip_file,

      \(x, y) read.fmli(x, y, year, !!!grp_vars)
    ) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("wtrep"),
          \(x) dplyr::if_else(is.na(x), 0, x) # tidyr::replace_na(x, 0)
        )
      )

    mtbi <- purrr::map2_df(
      interview_files$expenditure$Name,
      interview_files$expenditure$zip_file,
      \(x, y) read.mtbi(
        x,
        y,
        year = year,
        uccs = uccs,
        integrate_data = integrate_data,
        hg = hg
      )
    ) |>
      dplyr::bind_rows()

    interview <- dplyr::left_join(fmli, mtbi, by = "newid") |>
      dplyr::mutate(cost = dplyr::if_else(is.na(.data$cost), 0, .data$cost)) |>
      dplyr::mutate(survey = "I")

    if (recode_variables) {
      interview <- recode_ce_variables(interview, ce_codes, "I")
    }
  }

  if (survey_name %in% c("diary", "integrated")) {
    dia_yrs <- stringr::str_sub(year, 3, 4)

    dia_qtrs <- stringr::str_c(stringr::str_sub(year, 3, 4), 1:4)

    diary_files <- get_survey_files(
      year = year,
      survey = "diary",
      file_yrs = dia_yrs,
      qtrs = dia_qtrs,
      zp_file = dia_zp
    )

    fmld <- purrr::map2_df(
      diary_files$family$Name,
      diary_files$family$zip_file,
      \(x, y) read.fmld(x, y, !!!grp_vars)
    ) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("wtrep"),
          \(x) dplyr::if_else(is.na(x), 0, x) # tidyr::replace_na(x, 0)
        )
      )

    expd <- purrr::map2_df(
      diary_files$expenditure$Name,
      diary_files$expenditure$zip_file,
      \(x, y) read.expd(
        x,
        y,
        year = year,
        uccs = uccs,
        integrate_data = FALSE,
        hg = hg
      )
    ) |>
      dplyr::bind_rows()

    diary <- dplyr::left_join(fmld, expd, by = "newid") |>
      dplyr::mutate(cost = dplyr::if_else(is.na(.data$cost), 0, .data$cost)) |>
      dplyr::mutate(survey = "D")

    if (recode_variables) diary <- recode_ce_variables(diary, ce_codes, "D")
  }

  if (survey_name == "integrated") {
    return(dplyr::bind_rows(interview, diary))
  } else if (survey_name == "interview") {
    return(interview)
  } else if (survey_name == "diary") {
    return(diary)
  }
}
