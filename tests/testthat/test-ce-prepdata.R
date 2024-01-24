test_that("check that ucc's are valid", {
  expect_error(
    ce_prepdata(
      2021,
      interview,
      ce_hg(2021, "interview", hg_zip_path = "../testdata/stubs.zip"),
      c("0048", "Juice"),
      int_zp = "../testdata/intrvw21.zip"
    ),
    "'0048' is not a valid UCC. Please review the CE PUMD documentation.",
    fixed = TRUE
  )
})

test_that("'survey' accepts only valid survey titles", {
  expect_error(
    ce_prepdata(
      2021,
      intv,
      ce_hg(2021, "interview", hg_zip_path = "../testdata/stubs.zip"),
      uccs = "610310",
      int_zp = "../testdata/intrvw21.zip"
    ),
    "'survey' must be one of 'interview,' 'diary,' or 'integrated.'"
  )
})

test_that("'year' accepts only valid years", {
  expect_error(
    ce_prepdata(
      1990,
      intv,
      ce_hg(2021, "interview", hg_zip_path = "../testdata/stubs.zip"),
      uccs = "610310",
      int_zp = "../testdata/intrvw21.zip"
    ),
    "'year' must be a number between 1997 and 2022"
  )
})

test_that("'grp_vars' only accepts valid variables", {
  expect_error(
    ce_prepdata(
      year = 2021,
      survey = interview,
      hg = ce_hg(2021, "interview", hg_zip_path = "../testdata/stubs.zip"),
      uccs = "610310",
      sex,
      int_zp = c("../testdata/intrvw20.zip", "../testdata/intrvw21.zip")
    ),
    stringr::str_c(
      "The following are not valid variable names: ",
      "\nSEX",
      "\nPlease review the CE PUMD documentation and select valid variables."
    ),
    fixed = TRUE
  )
})

test_that("'hg' is a data.frame", {
  expect_error(
    ce_prepdata(
      2021,
      diary,
      "610310",
      hg = list(a = 45, d = "joe"),
      sex
    ),
    paste(
      "'hg' requires a valid HG dataframe.",
      "Please generate one using ce_hg()."
    ),
    fixed = TRUE
  )
})

test_that("'hg' is valid data frame", {
  expect_error(
    ce_prepdata(
      2021,
      diary,
      "610310",
      hg = data.frame(a = 1:10, b = letters[1:10]),
      sex
    ),
    paste(
      "'hg' requires a valid HG dataframe.",
      "Please generate one using ce_hg()."
    )
  )
})
