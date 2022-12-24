pet_hg <- ce_hg(2017, diary) %>%
  ce_uccs("Pets", uccs_only = FALSE)

ce_diary_pets17 <- ce_prepdata(
  year = 2017,
  survey = diary,
  uccs = ce_uccs(pet_hg, "Pets", uccs_only = TRUE),
  hg = pet_hg
)

test_that("dataset has all required variables", {
  expect_error(
    ce_diary_pets17 %>% dplyr::select(-cost) %>% ce_quantiles(),
    "Your dataset needs to include 'finlwt21' and the 'cost' variable"
  )
})

test_that("dataset all required variables are numeric", {
  expect_error(
    ce_diary_pets17 %>%
      dplyr::mutate(cost = as.character(cost)) %>% ce_quantiles(),
    "'finlwt21' and the 'cost' variable must be numeric."
  )
})

test_that("correct median is calculated", {
  expect_equal(ce_quantiles(ce_diary_pets17, 0.5)$quantile, 0)
})
