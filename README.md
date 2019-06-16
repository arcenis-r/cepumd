
<!-- README.md is generated from README.Rmd. Please edit that file -->
cepumd
======

<!-- badges: start -->
<!-- badges: end -->
cepumd facilitates the calculation of Consumer Expenditure Survey (CE) annual, weighted, estimated mean expenditures using CE Public-Use Microdata (PUMD) by addressing some unique challenges that exist in working with CE PUMD. Some examples are:

-   Downloading CE PUMD from within R
-   Converting stub files to data tables
-   Accounting for the factor (annual vs. quarterly expenditure)
-   Accounting for the "months in scope" of a given consumer unit (CU)
-   Annualizing expenditures for either Diary or Interview expenditures
-   Integrating Interview and Diary data as necessary
-   Calculating weighted CE medians

There are 4 functions that help the user download and wrangle the data and necessary documentation, such as the stub files:

-   `ce_download()` downloads zip files for a given year and survey instrument directly from the CE website
-   `ce_stub()` pulls the requested type of stub file (Interview, Diary, or Integrated) for a specified year.
-   `ce_uccs()` collects the Universal Classification Codes (UCC) for a specified type of expenditure from a specified stub file.
-   `ce_prepdata` merges the household characteristics file (FMLI/-D) with the corresponding expenditure tabulation file (MTBI/EXPD) for a specified year, adjusts weights for months-in-scope and the number of collection quarters, adjusts some cost values by their periodicity factor (some cost categories are represented as annual figures and others as quarterly).

There are 2 functions that the user can use to calculate CE summary statistics:

-   `ce_mean()` calculates a mean expenditure, standard error of the mean, coefficient of variation, and an aggregate expenditure.

-   `ce_median()` calculates a median expenditure. It is important to note that calculating means for integrated expenditures is not recommended because the calculation involves using weights from both the Diary and Survey instruments.

Installation
------------

You can install the released version of cepumd from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cepumd")
```

Example Workflow
----------------

The following is an example of how someone might go about using cepumd to calculate a 2017 annual, weighted estimate of mean expenditures on pets using CE integrated data.

The first step is to find out what UCC's represent the detailed expenditure categories that make up pet expenditures by downloading the stub and reviewing those categories.

``` r
# install.packages(c("cepumd", "dplyr", "tidyr", "purrr", "ggplot2"))
library(cepumd)
library(dplyr)

pet_stub <- ce_stub(2017, integrated)

pet_stub %>% slice(grep("[P|p]ets", pet_stub$title))
#> # A tibble: 2 x 4
#>   level title                                         ucc    factor
#>   <chr> <chr>                                         <chr>  <chr> 
#> 1 3     Pets, toys, hobbies, and playground equipment PETSPL 1     
#> 2 4     Pets                                          PETS   1
```

We see that there are two categories containing the word "Pets,", though the "level" column indicates that "Pets", level 4, falls under "Pets, toys, hobbies, and playground equipment," level 3. We're only interested strictly in "Pet" expenditures and not toys, hobbies, or playground equipment.

``` r
pet_uccs <- ce_uccs(pet_stub, "Pets")
pet_uccs
#> [1] "610310" "610320" "620410" "620420"

pet_stub %>% filter(ucc %in% pet_uccs)
#> # A tibble: 4 x 4
#>   level title                            ucc    factor
#>   <chr> <chr>                            <chr>  <chr> 
#> 1 5     Pet food                         610310 1     
#> 2 5     Pet purchase, supplies, medicine 610320 1     
#> 3 5     Pet services                     620410 1     
#> 4 5     Vet services                     620420 1
```

We can see that there are 4 detailed expenditure categories under the category of pet expenditures. Next we'll want to prepare a dataset to calculate an integrated weighted mean expenditure estimate. To do that, though, we'll need both the Diary and Interview data for pet expenditures. We will include the "bls\_urbn" variable to calculate estimated means by group later.

``` r
pet_int <- ce_prepdata(
  year = 2017, survey = interview, uccs = pet_uccs, zp = NULL, 
  integrate_data = TRUE, stub = pet_stub, bls_urbn
)

pet_dia <- ce_prepdata(
  year = 2017, survey = diary, uccs = pet_uccs, zp = NULL, 
  integrate_data = TRUE, stub = pet_stub, bls_urbn
)

pet_integrated <- bind_rows(pet_int, pet_dia)
```

The Interview and Diary datasets have each been prepared and combined. Next we'll calculate estimated, weighted, mean pet expenditures for 2017.

``` r
pet_mean <- ce_mean(pet_integrated)
knitr::kable(pet_mean)
```

|     agg\_exp|  mean\_exp|        se|         cv|
|------------:|----------:|---------:|----------:|
|  92175930043|   709.7265|  65.00804|  0.0915959|

The median expenditure on pets We can also calculate estimated, weighted, means by group. In this case we'll use the bls\_urbn variable. We'll also generate a plot of the those means.

``` r
pet_mean_by_urbn <- pet_integrated %>%
  group_by(bls_urbn) %>%
  tidyr::nest() %>%
  mutate(
    ce_mn_df = purrr::map(data, ce_mean),
    bls_urbn = ifelse(bls_urbn %in% 1, "Urban", "Rural")
  ) %>% 
  select(-data) %>% 
  tidyr::unnest(ce_mn_df) %>%
  mutate(
    lower = mean_exp - (qnorm(0.975) * se),
    upper = mean_exp + (qnorm(0.975) * se)
  )

knitr::kable(pet_mean_by_urbn)
```

| bls\_urbn |     agg\_exp|  mean\_exp|        se|         cv|      lower|     upper|
|:----------|------------:|----------:|---------:|----------:|----------:|---------:|
| Urban     |  84028916748|  646.99000|  62.33042|  0.0963391|  524.82462|  769.1554|
| Rural     |   8147013295|   62.73651|  20.26470|  0.3230129|   23.01843|  102.4546|

``` r

library(ggplot2)

ggplot(pet_mean_by_urbn, aes(x = bls_urbn, y = mean_exp, fill = bls_urbn)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +
  theme_light() +
  theme(title = element_text(hjust = 0.5)) +
  labs(
    title = "2017 estimated mean pet expenditures by urbanicity",
    x = "Urbanicity",
    y = "Estimated mean expenditure ($)",
    fill = "Urbanicity"
  )
```

<img src="man/figures/README-calc_means_by_urbn-1.png" width="100%" />

We can also calculate estimated, weighted median expenditures. Because the integrated data come from both the Interview and Diary surveys, we'll display them by UCC.

``` r
pet_mean_by_ucc <- pet_integrated %>%
  group_by(ucc) %>%
  tidyr::nest() %>%
  mutate(ce_med_df = purrr::map(data, ce_median)) %>% 
  select(-data) %>% 
  tidyr::unnest(ce_med_df)

knitr::kable(pet_mean_by_ucc)
```

| ucc    |  ce\_med\_df|
|:-------|------------:|
| NA     |         0.00|
| 610320 |        90.00|
| 620410 |        90.00|
| 610310 |       170.30|
| 620420 |      1593.67|

Another option is to use unintegrated data from only 1 survey instrument.

``` r
pet_dia_only <- ce_prepdata(
  year = 2017, survey = diary, uccs = pet_uccs, zp = NULL, 
  integrate_data = FALSE, stub = pet_stub, bls_urbn
)

ce_median(pet_dia_only)
#> [1] 0
```

We can also use data from the Diary only to calculate medians by bls\_urbn.

``` r
pet_median_by_urbn <- pet_dia_only %>%
  group_by(bls_urbn) %>%
  tidyr::nest() %>%
  mutate(ce_med_df = purrr::map(data, ce_median)) %>% 
  select(-data) %>% 
  tidyr::unnest(ce_med_df)

knitr::kable(pet_median_by_urbn)
```

|  bls\_urbn|  ce\_med\_df|
|----------:|------------:|
|          1|            0|
|          2|            0|
