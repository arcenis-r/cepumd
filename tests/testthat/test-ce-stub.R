test_that("'year' accepts only valid years", {
  expect_error(
    ce_stub(1990, "interview"),
    "'year' must be a number between 1996 and 2017"
  )
})

test_that("'stub_type' accepts only valid stub types", {
  expect_error(
    ce_stub(2007, "inter"),
    "'stub_type' must be one of interview, diary, or integrated"
  )
})

test_that("integrated stubs not available before 2002", {
  expect_error(
    ce_stub(1999, "integrated"),
    "Integrated stub files are only available for 2002 through 2017"
  )
})

test_that("ce_stub results ouputs data frame", {
  expect_s3_class(ce_stub(2015, "integrated"), "data.frame")
})

test_that("ce_stub has correct columns", {
  expect_true(
    all(c("level", "title", "ucc", "factor") %in% names(ce_stub(2012, "diary")))
  )
})
