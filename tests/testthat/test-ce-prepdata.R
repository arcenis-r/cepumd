test_that("check that ucc's are valid", {
  expect_error(
    ce_prepdata(2015, interview, c("0048", "Juice")),
    paste0(
      "'0048' is not a valid UCC. ",
      "Please review the CE survey documentation to ensure '0048'",
      " is a UCC in your dataset (check by year)."
    ),
    fixed = TRUE
  )
})

test_that("'survey' accepts only valid survey titles", {
  expect_error(
    ce_prepdata(2016, intv, "610310"),
    "'survey' must be one of 'interview' or 'diary'"
  )
})

test_that("'year' accepts only valid years", {
  expect_error(
    ce_prepdata(1990, intv, "610310"),
    "'year' must be a number between 1996 and 2017"
  )
})

test_that("'grp_vars' only accepts valid variables", {
  expect_error(
    ce_prepdata(2015, diary, "610310", TRUE, FALSE, NULL, NULL, sex),
    paste0(
      "'sex' is not a valid variable. ",
      "Please review the CE survey documentation to ensure 'sex'",
      " is a variable in your dataset (check by year)."
    ),
    fixed = TRUE
  )
})

test_that("'stub' is a data.frame", {
  expect_error(
    ce_prepdata(
      2015,
      diary,
      "610310",
      TRUE,
      FALSE,
      NULL,
      stub = list(a = 45, d = "joe"),
      sex
    ),
    paste(
      "'stub' requires a valid stub dataframe.",
      "Please generate one using ce_stub()."
    ),
    fixed = TRUE
  )
})

test_that("'stub' is valid data frame", {
  expect_error(
    ce_prepdata(
      2015,
      diary,
      "610310",
      TRUE,
      FALSE,
      NULL,
      stub = data.frame(a = 1:10, b = letters[1:10]),
      sex
    ),
    paste(
      "'stub' requires a valid stub dataframe.",
      "Please generate one using ce_stub()."
    )
  )
})
