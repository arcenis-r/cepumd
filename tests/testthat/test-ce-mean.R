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
    ce_diary_pets17 %>% select(-wtrep04) %>% ce_mean,
    paste(
      "Your dataset needs to include 'finlwt21', all 44 replicate weights,",
      "i.e., 'wtrep01' to 'wtrep44', the 'cost' variable, and the 'survey'",
      "variable."
    ),
    fixed = TRUE
  )
})

test_that("dataset all required variables are numeric", {
  expect_error(
    ce_diary_pets17 %>% mutate(wtrep04 = as.character(wtrep04)) %>% ce_mean,
    paste(
      "'finlwt21', all replicate weight variables, i.e., 'wtrep01' to",
      "'wtrep44', and the 'cost' variable must be numeric."
    ),
    fixed = TRUE
  )
})

test_that("correct mean is calculated", {
  expect_equal(round(ce_mean(ce_diary_pets17)$mean_exp, 2), 688.47)
})
