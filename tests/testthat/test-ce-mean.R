pet_hg <- ce_hg(2021, diary, hg_zip_path = "../testdata/stubs.zip") |>
  ce_uccs("Pets", ucc_group = "PETS", uccs_only = FALSE)

ce_diary_pets21 <- ce_prepdata(
  year = 2021,
  survey = diary,
  hg = pet_hg,
  uccs = ce_uccs(pet_hg, "Pets", uccs_only = TRUE),
  dia_zp = "../testdata/diary21.zip"
)

test_that("dataset has all required variables", {
  expect_error(
    ce_diary_pets21 %>% select(-wtrep04) %>% ce_mean,
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
    ce_diary_pets21 %>%
      dplyr::mutate(wtrep04 = as.character(wtrep04)) %>% ce_mean,
    paste(
      "'finlwt21', all replicate weight variables, i.e., 'wtrep01' to",
      "'wtrep44', and the 'cost' variable must be numeric."
    ),
    fixed = TRUE
  )
})

test_that("correct mean is calculated", {
  expect_equal(round(ce_mean(ce_diary_pets21)$mean_exp, 2), 663.54)
})
