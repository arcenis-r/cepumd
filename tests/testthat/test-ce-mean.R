test_that("dataset has all required variables", {
  expect_error(
    ce_diary_pets17 %>% select(-wtrep04) %>% ce_mean,
    paste(
      "Your dataset needs to include 'finlwt21', all 44 replicate weights,",
      "i.e., 'wtrep01' to 'wtrep44', and the 'cost' variable"
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
  expect_equal(ce_mean(ce_diary_pets17)$mean_exp, 580.3878)
})
