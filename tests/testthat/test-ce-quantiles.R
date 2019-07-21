pet_stub <- ce_stub(2017, integrated) %>%
  ce_uccs("Pets", uccs_only = FALSE)

ce_diary_pets17 <- ce_prepdata(
  year = 2017,
  survey = diary,
  uccs = ce_uccs(pet_stub, "Pets", uccs_only = TRUE),
  zp = NULL,
  integrate_data = FALSE,
  stub = pet_stub
)

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
