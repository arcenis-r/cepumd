test_df <- data.frame(a = 1:10, b = letters[1:10])
test_stub <- ce_stub(2015, "integrated")

test_that("'stub' is valid data frame", {
  expect_error(
    ce_uccs(test_df, "Pets"),
    paste(
      "'stub' requires a valid stub dataframe.",
      "Please generate one using ce_stub()."
    )
  )
})

test_that("'stub' is a data.frame", {
  expect_error(
    ce_uccs(list(a = 45, d = "joe"), "Pets"),
    paste(
      "'stub' requires a valid stub dataframe.",
      "Please generate one using ce_stub()."
    )
  )
})

test_that("'expenditure' is valid", {
  expect_error(
    ce_uccs(ce_stub(2015, "integrated"), "Funnies"),
    paste(
      "The expenditure must be a valid expenditure category from the stub",
      "dataframe's 'title' column and must be spelled exactly as it is in",
      "the stub file."
    )
  )
})

test_that("output is a character vector", {
  expect_type(ce_uccs(test_stub, "Pets"), "character")
})

test_that("all ucc's are 6 characters long", {
  expect_true(all(nchar(ce_uccs(test_stub, "Pets")) == 6))
})

test_that("all ucc's can be converted to numeric", {
  expect_true(all(!is.na(as.numeric(ce_uccs(test_stub, "Ham")))))
})
