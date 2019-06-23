test_that("zip download stops with invalid zip path", {
  expect_error(
    ce_download(2016, interview, 40),
    "invalid 'destfile' argument"
  )
})

test_that("zip downloads successfully", {
  expect_silent(ce_download(2016, diary, "dia16.zip"))
})

test_that("'survey' accepts only valid survey titles", {
  expect_error(
    ce_download(2016, intv, "intv16.zip"),
    "'survey' must be one of 'interview' or 'diary'"
  )
})

test_that("'year' accepts only valid years", {
  expect_error(
    ce_download(1990, intv, "intv16.zip"),
    "'year' must be a number between 1996 and 2017"
  )
})
