test_that("check that ucc's are valid", {
  expect_error(
    ce_prepdata(2015, interview, c("0048", "Juice")),
    "'0048' is not a valid UCC. Please review the CE PUMD documentation.",
    fixed = TRUE
  )
})

test_that("'survey' accepts only valid survey titles", {
  expect_error(
    ce_prepdata(2016, intv, "610310"),
    "'survey' must be one of 'interview,' 'diary,' or 'integrated.'"
  )
})

test_that("'year' accepts only valid years", {
  expect_error(
    ce_prepdata(1990, intv, "610310"),
    "'year' must be a number between 1997 and 2018"
  )
})

test_that("'grp_vars' only accepts valid variables", {
  expect_error(
    ce_prepdata(2015, diary, "610310", TRUE, NULL, NULL, NULL, NULL, sex),
    paste0(
      "'sex' is not a valid variable. ",
      "Please review the CE PUMD documentation."
    ),
    fixed = TRUE
  )
})

test_that("'hg' is a data.frame", {
  expect_error(
    ce_prepdata(
      2015,
      diary,
      "610310",
      TRUE,
      NULL,
      NULL,
      NULL,
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
      2015,
      diary,
      "610310",
      TRUE,
      NULL,
      NULL,
      NULL,
      hg = data.frame(a = 1:10, b = letters[1:10]),
      sex
    ),
    paste(
      "'hg' requires a valid HG dataframe.",
      "Please generate one using ce_hg()."
    )
  )
})
