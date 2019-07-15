test_that("dataset has all required variables", {
  expect_error(
    ce_diary_pets17 %>% select(-cost) %>% ce_quantiles(),
    "Your dataset needs to include 'finlwt21' and the 'cost' variable"
  )
})

test_that("dataset all required variables are numeric", {
  expect_error(
    ce_diary_pets17 %>% mutate(cost = as.character(cost)) %>% ce_quantiles(),
    "'finlwt21' and the 'cost' variable must be numeric."
  )
})

test_that("correct median is calculated", {
  expect_equal(ce_quantiles(ce_diary_pets17, 0.5)$quantile, 0)
})
