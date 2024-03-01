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
    ce_diary_pets21 |> dplyr::select(-cost) |> ce_quantiles(),
    "Your dataset needs to include 'finlwt21' and the 'cost' variable"
  )
})

test_that("dataset all required variables are numeric", {
  expect_error(
    ce_diary_pets21 |>
      dplyr::mutate(cost = as.character(cost)) |>
        ce_quantiles(),
    "'finlwt21' and the 'cost' variable must be numeric."
  )
})

test_that("correct median is calculated", {
  expect_equal(ce_quantiles(ce_diary_pets21, 0.5)$quantile, 0)
})
