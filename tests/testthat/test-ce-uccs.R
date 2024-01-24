test_df <- data.frame(a = 1:10, b = letters[1:10])
test_hg <- ce_hg(2021, "integrated", hg_zip_path = "../testdata/stubs.zip")

test_that("'hg' is valid data frame", {
  expect_error(
    ce_uccs(test_df, "Pets"),
    paste(
      "'hg' requires a valid HG dataframe.",
      "Please generate one using ce_hg()."
    )
  )
})

test_that("'hg' is a data.frame", {
  expect_error(
    ce_uccs(list(a = 45, d = "joe"), "Pets"),
    paste(
      "'hg' requires a valid HG dataframe.",
      "Please generate one using ce_hg()."
    )
  )
})

test_that("'expenditure' is valid", {
  expect_error(
    ce_uccs(test_hg, "Funnies"),
    paste(
      "Either a valid 'expenditure' or valid 'ucc_group' is required and",
      "it must match exactly the spelling in the HG file corresponding",
      "column. Please see details in the ce_ucc() documentation."
    )
  )
})

test_that("output is a character vector", {
  expect_type(ce_uccs(test_hg, "Pets", ucc_group = "PETS"), "character")
})

test_that("all ucc's are 6 characters long", {
  expect_true(all(nchar(ce_uccs(test_hg, "Pets", ucc_group = "PETS")) == 6))
})

test_that("all ucc's can be converted to numeric", {
  expect_true(
    all(!is.na(as.numeric(ce_uccs(test_hg, "Ham", ucc_group = "HAM"))))
  )
})
