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
#' @param ... Variables to include in the dataset from the family
#' characteristics file. This is intended to allow the user to calculate
#' estimates for subsets of the data.
#' @param recode_variables A logical indicating whether to recode all coded
#' variables except 'UCC' using the codes in the CE's excel dictionary which can
#' be downloaded from the
#' \href{https://www.bls.gov/cex/pumd_doc.htm}{CE Documentation Page}
#' @param ce_dir The directory in which CE PUMD data and metadata are stored. If
#' \code{NULL} (the default) a directory called "ce-data" will be created in the
#' temporary directory of the session.
#' @param dict_path A string indicating the path where the CE PUMD dictionary
#' is stored if already stored. If the file does not exist and
#' \code{recode_variables = TRUE} the dictionary will be stored in this path.
#' The default is \code{NULL} which causes the zip file to be stored in
#' temporary memory during function operation.
#' @param int_zp String indicating the path of the Interview data zip file(s) if
#' already stored. If the file(s) does not exist its corresponding zip file will
#' be stored in that path. The default is \code{NULL} which causes the zip file
#' to be stored in temporary memory during function operation.
#' @param dia_zp Same as \code{int_zp} above, but for Diary data.
#' @param hg A data frame that has, at least, the title, level, ucc, and
#' factor columns of a CE HG file. Calling \code{\link{ce_hg}} will generate a
#' valid HG file.
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
#' @importFrom readxl excel_sheets
#' @importFrom readxl cell_cols
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#'
#' @examples
#' # The following workflow will prepare a dataset for calculating integrated
#' # pet expenditures for 2021 keep the "sex_ref" variable in the data to
#' # potentially calculate means by sex of the reference person.
#'
#' # First generate an HG file
#' my_hg <- ce_hg(2021, integrated)
#'
#' # Store a vector of UCC's in the "Pets" category
#' pet_uccs <- ce_uccs(my_hg, "Pets")
#'
#' # Store the diary data (not run)
#' \dontrun{
#' pets_dia <- ce_prepdata(
#'   year = 2021,
#'   survey = integrated,
#'   uccs = pet_uccs,
#'   integrate_data = TRUE,
#'   recode_variables = TRUE,
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
                        ce_dir = NULL,
                        dict_path = NULL,
                        int_zp = NULL,
                        dia_zp = NULL,
                        hg = NULL,
                        ...
                        ) {

  survey <- rlang::ensym(survey)
  survey_name <- rlang::as_string(survey) %>% tolower()

  grp_vars <- rlang::ensyms(...)
  grp_var_names <- purrr::map(grp_vars, rlang::as_string) %>%
    unlist() %>%
    tolower()

  # Ensure that there's a directory to put files into
  if (is.null(ce_dir)) {
    if (!file.exists(file.path(tempdir(), "ce-data"))) {
      dir.create(file.path(tempdir(), "ce-data"))
    }

    ce_dir <- file.path(tempdir(), "ce-data")
  }

  max_year <- max(ce_pumd_years())

  if (!year %in% 1997:max_year) {
    stop(
      stringr::str_c("'year' must be a number between 1997 and ", max_year, ".")
    )
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
      dict_path <- "ce-dict.xlsx"
      store_ce_dict(dict_path = dict_path, ce_dir = ce_dir)
    } else if (isFALSE(file.exists(file.path(ce_dir, dict_path)))) {
      store_ce_dict(dict_path = dict_path, ce_dir = ce_dir)
    }
  }

  if (recode_variables) {
    code_sheet <- grep(
      "^Codes",
      readxl::excel_sheets(file.path(ce_dir, dict_path)),
      value = TRUE
    )

    ce_dict <- readxl::read_excel(
      file.path(ce_dir, dict_path),
      sheet = code_sheet,
      range = readxl::cell_cols("A:J"),
      guess_max = 4000
    )

    names(ce_dict) <- tolower(names(ce_dict)) %>%
      stringr::str_replace(" ", "_")

    ce_dict <- ce_dict %>%
      dplyr::mutate(
        survey = substr(survey, 1, 1),
        variable = tolower(variable),
        last_year = tidyr::replace_na(
          last_year,
          max(last_year, na.rm = TRUE)
        )
      ) %>%
      dplyr::filter(
        first_year <= year,
        last_year >= year,
      )
  }

  if (survey_name %in% c("interview", "integrated")) {
    # Create a vector of years for which data are required
    if (year >= 2020) {
      int_yrs <- stringr::str_sub(c(year - 1, year), 3, 4)
    } else {
      int_yrs <- stringr::str_sub(year, 3, 4)
    }

    # Create a vector of the required quarters for the given year
    int_qtrs <- c(
      stringr::str_c(stringr::str_sub(year, 3, 4), 1:4),
      stringr::str_c(stringr::str_sub((year + 1), 3, 4), 1)
    )

    if (!is.null(int_zp)) {

      if (sum(file.exists(file.path(ce_dir, int_zp))) == 0) {
        int_zp <- NULL
      } else {
        # Make a dataframe of the required files from all of the zip files
        int_file_df <- purrr::map(
          file.path(ce_dir, int_zp),
          unzip,
          list = TRUE
        ) %>%
          purrr::map2_df(
            int_zp,
            ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
          ) %>%
          dplyr::filter(
            stringr::str_detect(
              Name,
              stringr::str_c(int_qtrs, collapse = "|")
            )
          )

        fmli_files <- int_file_df %>%
          dplyr::filter(stringr::str_detect(Name, "fmli"))

        mtbi_files <- int_file_df %>%
          dplyr::filter(stringr::str_detect(Name, "mtbi"))

        if (
          nrow(fmli_files) != length(int_qtrs) |
          nrow(mtbi_files) != length(int_qtrs)
        ) {
          int_zp <- NULL
          rm(int_file_df, fmli_files, mtbi_files)
        }
      }
    }

    if (is.null(int_zp)) {
      ce_download(year = year, survey = "interview", ce_dir = ce_dir)

      # Store a vector of the zip files from which to extract data
      int_zip_files <- stringr::str_c("intrvw", int_yrs, ".zip")

      # Make a dataframe of the required files from all of the zip files
      int_file_df <- purrr::map(
        file.path(ce_dir, int_zip_files),
        unzip,
        list = TRUE
      ) %>%
        purrr::map2_df(
          int_zip_files,
          ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
        ) %>%
        dplyr::filter(
          stringr::str_detect(
            Name,
            stringr::str_c(int_qtrs, collapse = "|")
          )
        )

      fmli_files <- int_file_df %>%
        dplyr::filter(stringr::str_detect(Name, "fmli"))

      mtbi_files <- int_file_df %>%
        dplyr::filter(stringr::str_detect(Name, "mtbi"))
    }

    fmli <- purrr::map2_df(
      fmli_files$Name,
      fmli_files$zipfile,
      read.fmli,
      year = year,
      ce_dir = ce_dir,
      grp_var_names = grp_var_names
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        dplyr::across(dplyr::contains("wtrep"), ~ tidyr::replace_na(.x, 0))
      )

    mtbi <- purrr::map2_df(
      mtbi_files$Name,
      mtbi_files$zipfile,
      read.mtbi,
      year = year,
      uccs = uccs,
      integrate_data = integrate_data,
      hg = hg,
      ce_dir = ce_dir
    ) %>%
      dplyr::bind_rows()

    interview <- dplyr::left_join(fmli, mtbi, by = "newid") %>%
      dplyr::mutate(cost = replace(cost, is.na(cost), 0)) %>%
      dplyr::mutate(survey = "I")

    if (recode_variables) {
      recode_vars <- names(interview)[names(interview) %in% ce_dict$variable]
      recode_vars <- recode_vars[!recode_vars %in% "ucc"]

      ce_dict_int <- ce_dict %>%
        dplyr::filter(survey == "I")

      for (i in recode_vars) {
        code_col <- interview[[i]]
        codes_df <- ce_dict_int %>%
          dplyr::filter(variable %in% i) %>%
          dplyr::select(code_value, code_description)

        interview[, i] <- factor(
          code_col,
          levels = codes_df$code_value,
          labels = codes_df$code_description
        )
      }
    }
  }

  if (survey_name %in% c("diary", "integrated")) {
    dia_yrs <- stringr::str_sub(year, 3, 4)

    dia_qtrs <- stringr::str_c(stringr::str_sub(year, 3, 4), 1:4)

    if (!is.null(dia_zp)) {
      if (sum(file.exists(file.path(ce_dir, dia_zp))) == 0) {
        dia_zp <- NULL
      } else {
        # Make a dataframe of the required files from all of the zip files
        dia_file_df <- purrr::map(
          file.path(ce_dir, dia_zp),
          unzip,
          list = TRUE
        ) %>%
          purrr::map2_df(
            dia_zp,
            ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
          ) %>%
          dplyr::filter(
            stringr::str_detect(Name, stringr::str_c(dia_qtrs, collapse = "|"))
          )

        fmld_files <- dia_file_df %>%
          dplyr::filter(stringr::str_detect(Name, "fmld"))

        expd_files <- dia_file_df %>%
          dplyr::filter(stringr::str_detect(Name, "expd"))

        if (
          nrow(fmld_files) != length(dia_qtrs) |
          nrow(expd_files) != length(dia_qtrs)
        ) {
          dia_zp <- NULL
          rm(dia_file_df, fmld_files, expd_files)
        }
      }
    }

    if (is.null(dia_zp)) {
      ce_download(year = year, survey = "diary", ce_dir = ce_dir)

      # Store a vector of the zip files from which to extract data
      dia_zip_files <- stringr::str_c("diary", dia_yrs, ".zip")

      # Make a dataframe of the required files from all of the zip files
      dia_file_df <- purrr::map(
        file.path(ce_dir, dia_zip_files),
        unzip,
        list = TRUE
      ) %>%
        purrr::map2_df(
          dia_zip_files,
          ~ .x %>% dplyr::mutate(zipfile = file.path(ce_dir, .y))
        ) %>%
        dplyr::filter(
          stringr::str_detect(Name, stringr::str_c(dia_qtrs, collapse = "|"))
        )

      fmld_files <- dia_file_df %>%
        dplyr::filter(stringr::str_detect(Name, "fmld"))

      expd_files <- dia_file_df %>%
        dplyr::filter(stringr::str_detect(Name, "expd"))
    }

    fmld <- purrr::map2_df(
      fmld_files$Name,
      fmld_files$zipfile,
      read.fmld,
      grp_var_names = grp_var_names,
      ce_dir = ce_dir
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        dplyr::across(dplyr::contains("wtrep"), ~ tidyr::replace_na(.x, 0))
      )

    expd <- purrr::map2_df(
      expd_files$Name,
      expd_files$zipfile,
      read.expd,
      year = year,
      uccs = uccs,
      integrate_data = integrate_data,
      hg = hg,
      ce_dir = ce_dir
    ) %>%
      dplyr::bind_rows()

    diary <- dplyr::left_join(fmld, expd, by = "newid") %>%
      dplyr::mutate(cost = replace(cost, is.na(cost), 0)) %>%
      dplyr::mutate(survey = "D")

    if (recode_variables) {
      recode_vars <- names(diary)[names(diary) %in% ce_dict$variable]
      recode_vars <- recode_vars[!recode_vars %in% "ucc"]

      ce_dict_dia <- ce_dict %>%
        dplyr::filter(survey == "D")

      for (i in recode_vars) {
        code_col <- diary[[i]]
        codes_df <- ce_dict_dia %>%
          dplyr::filter(variable %in% i) %>%
          dplyr::select(code_value, code_description)

        diary[, i] <- factor(
          code_col,
          levels = codes_df$code_value,
          labels = codes_df$code_description
        )
      }
    }
  }

  if (survey_name == "integrated") {
    return(dplyr::bind_rows(interview, diary))
  } else if (survey_name == "interview") {
    return(interview)
  } else if ( survey_name == "diary") {
    return(diary)
  }
}
