test_that("'year' accepts only valid years", {
  expect_error(
    ce_hg(1990, "interview"),
    "'year' must be a number between 1997 and 2018"
  )
})

test_that("'hg_type' accepts only valid HG types", {
  expect_error(
    ce_hg(2007, "inter"),
    "'hg_type' must be one of interview, diary, or integrated"
  )
})

test_that("ce_hg results ouputs data frame", {
  expect_s3_class(ce_hg(2015, "integrated"), "data.frame")
})

test_that("ce_hg has correct columns", {
  expect_true(
    all(c("level", "title", "ucc", "factor") %in% names(ce_hg(2012, "diary")))
  )
})
