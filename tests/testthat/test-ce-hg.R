test_that("'year' accepts only valid years", {
  expect_error(
    ce_hg(1990, "interview"),
    "'year' must be a number between 1997 and 2022"
  )
})

test_that("'survey' accepts only valid HG types", {
  expect_error(
    ce_hg(2007, "inter"),
    "'survey' must be one of interview, diary, or integrated"
  )
})

test_that("ce_hg results ouputs data frame", {
  expect_s3_class(
    ce_hg(2015, "integrated", hg_zip_path = "../testdata/stubs.zip"),
    "data.frame"
  )
})

test_that("ce_hg has correct columns", {
  expect_true(
    all(
      c("level", "title", "ucc", "factor") %in%
        names(ce_hg(2012, "diary", hg_zip_path = "../testdata/stubs.zip"))
    )
  )
})
