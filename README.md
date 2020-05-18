
[![Travis build
status](https://travis-ci.com/arcenis-r/cepumd.svg?branch=master)](https://travis-ci.com/arcenis-r/cepumd)
[![codecov](https://codecov.io/gh/arcenis-r/cepumd/branch/master/graph/badge.svg)](https://codecov.io/gh/arcenis-r/cepumd)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# cepumd

`cepumd` facilitates the calculation of Consumer Expenditure Survey (CE)
annual, weighted, estimated mean expenditures using CE Public-Use
Microdata (PUMD) by addressing some unique challenges that exist in
working with CE PUMD. Some examples are:

  - Downloading CE PUMD from within R
  - Converting hierarchical grouping (HG) files to data tables
  - Accounting for the factor (annual vs. quarterly expenditure)
  - Accounting for the “months in scope” of a given consumer unit (CU)
  - Annualizing expenditures for either Diary or Interview expenditures
  - Integrating Interview and Diary data as necessary
  - Calculating weighted CE quantiles

For more information on the CE, please visit <https://www.bls.gov/cex/>.

There are four functions that help the user download and wrangle the
data and necessary documentation, such as the HG files:

  - `ce_download()` downloads zip files for a given year and survey
    instrument directly from the CE website
  - `ce_hg()` pulls the requested type of HG file (Interview, Diary, or
    Integrated) for a specified year.
  - `ce_uccs()` filters the HG file for the specified expenditure
    category and returns either a data frame with only that section of
    the HG file or the Universal Classification Codes (UCCs) that make
    up that expenditure category.
  - `ce_prepdata` merges the household characteristics file (FMLI/-D)
    with the corresponding expenditure tabulation file (MTBI/EXPD) for a
    specified year, adjusts weights for months-in-scope and the number
    of collection quarters, adjusts some cost values by their
    periodicity factor (some cost categories are represented as annual
    figures and others as quarterly).

There are two functions that the user can use to calculate CE summary
statistics:

  - `ce_mean()` calculates a mean expenditure, standard error of the
    mean, coefficient of variation, and an aggregate expenditure.

  - `ce_quantiles()` calculates weighted expenditure quantiles. It is
    important to note that calculating means for integrated expenditures
    is not recommended because the calculation involves using weights
    from both the Diary and Survey instruments.

Finally, there are two functions that allow the user to download
documentation that’s useful in preparing data:

  - `store_ce_hg()` downloads the zip file containing all HG files
    maintained by the CE to the specified location.

  - `store_ce_dict()` downloades the CE PUMD dictionary from CE’s
    website to the specified location.

## Installation

You can install the development version of `cepumd` from
[GitHub](https://github.com) with:

``` r
devtools::install_github("/arcenis-r/cepumd")
```

## Example Workflow

The following is an example of how someone might go about using `cepumd`
to calculate a 2018 annual, weighted estimate of mean expenditures on
pets by urbanicity using CE integrated data.

The first step is to load the necessary packages into the environment.

``` r

# Store a vector of names of additional packages to be used
pkgs <- c("tidyverse", "devtools", "rlang", "readxl")

# Install packages from CRAN
sapply(pkgs, function(x) if (!x %in% installed.packages()) install.packages(x))
#> $tidyverse
#> NULL
#> 
#> $devtools
#> NULL
#> 
#> $rlang
#> NULL
#> 
#> $readxl
#> NULL

# Load 'cepumd' and 'tidyverse' to the workspace
library(cepumd)
library(tidyverse)
```

Since we’re interested in pet expenditures, we’ll find out what titles
in the HG contain the word “Pets.” We’ll use the the  function to load
an HG file into the workspace, but notice that first we save the zip
files to temporary memory using the  function.

``` r
# Store the HG zip file folder to temporary memory
tmp_hg <- tempfile()
store_ce_hg(tmp_hg)

# Download the HG file
hg_file <- ce_hg(2018, integrated, hg_zip_path = tmp_hg)

# Pull out the titles that contain the word "Pets"
hg_file %>%
  slice(grep("[P|p]ets", title)) %>%
  knitr::kable(booktabs = TRUE)
```

| level | title                                   | ucc    | survey | factor |
| :---- | :-------------------------------------- | :----- | :----- | :----- |
| 3     | Pets, toys, hobbies, and playground equ | PETSPL | G      | 1      |
| 4     | Pets                                    | PETS   | G      | 1      |

We see that there are two categories containing the word “Pets,”, though
the “level” column indicates that “Pets”, level 4, falls under “Pets,
toys, hobbies, and playground equipment,” level 3. We’re only interested
strictly in “Pets” expenditures and not toys, hobbies, or playground
equipment.

Now we can look at the section of the HG file containing the UCC’s that
make up pet expenditures with .

``` r
# Filter the HG file for pet related UCCs
pet_hg <- ce_uccs(hg_file, "Pets", uccs_only = FALSE)
```

| level | title                            | ucc    | survey | factor |
| :---- | :------------------------------- | :----- | :----- | :----- |
| 4     | Pets                             | PETS   | G      | 1      |
| 5     | Pet food                         | 610310 | D      | 1      |
| 5     | Pet purchase, supplies, medicine | 610320 | I      | 1      |
| 5     | Pet services                     | 620410 | I      | 1      |
| 5     | Vet services                     | 620420 | D      | 1      |

We can also take a quick look at the CE PUMD dictionary to see the codes
associated with urbanicity in the 2018 data. The dictionary has 3
sheets: “Cover,” “Variables,” and “Codes.” We’re interested in the
“Variables” sheet to review the variable definition and the “Codes”
sheet to view the code definitions associated with urbanicity.

NB: In the below code we use a little regex to pull the names of the
sheets we need to try to avoid typographical issues. At the time of this
writing the name of the sheet containing the codes is actually spelled
“Codes”, with a space at the end.

``` r
library(readxl)

# Store the CE dictionary
tmp_dict <- tempfile()
store_ce_dict(tmp_dict)

# Get the name of the sheet containing variable definitions
var_sheet <- grep("^Variable", excel_sheets(tmp_dict), value = TRUE)

# Read in the variables sheet and filter for the BLS_URBN variable
urbn_var <- read_excel(
  tmp_dict,
  sheet = var_sheet,
  guess_max = 4000
) %>%
  set_names(tolower(names(.)) %>% str_replace_all(" ", "_")) %>%
  mutate(
    last_year = replace_na(
      last_year,
      max(last_year, na.rm = TRUE)
    )
  ) %>%
  filter(
    first_year <= 2018,
    last_year >= 2018,
    variable_name %in% "BLS_URBN"
  ) %>%
  select(survey, file, variable_name, variable_description)

# Get the name of the sheet containing code definitions
code_sheet <- grep("^Codes", excel_sheets(tmp_dict), value = TRUE)

# Read in the codes sheet and filter for the BLS_URBN variable
urbn_codes <- read_excel(
  tmp_dict,
  sheet = code_sheet,
  range = cell_cols("A:J"),
  guess_max = 4000
) %>%
  set_names(tolower(names(.)) %>% str_replace_all(" ", "_")) %>%
  mutate(
    last_year = replace_na(
      last_year,
      max(last_year, na.rm = TRUE)
    )
  ) %>%
  filter(
    first_year <= 2018,
    last_year >= 2018,
    variable %in% "BLS_URBN"
  ) %>%
  select(survey, file, variable, code_value, code_description)
```

The definition of the “BLS\_URBN” variable
is:

| survey    | file | variable\_name | variable\_description                        |
| :-------- | :--- | :------------- | :------------------------------------------- |
| INTERVIEW | FMLI | BLS\_URBN      | Is this CU located in an urban or rural area |
| DIARY     | FMLD | BLS\_URBN      | Urban/Rural                                  |

The “BLS\_URBN” codes are:

| survey    | file | variable  | code\_value | code\_description |
| :-------- | :--- | :-------- | :---------- | :---------------- |
| DIARY     | FMLD | BLS\_URBN | 2           | Rural             |
| DIARY     | FMLD | BLS\_URBN | 1           | Urban             |
| INTERVIEW | FMLI | BLS\_URBN | 2           | Rural             |
| INTERVIEW | FMLI | BLS\_URBN | 1           | Urban             |

Next we’ll download both the 2018 Interview and Diary files to temporary
file paths.

``` r
tmp_interview <- tempfile()
ce_download(2018, interview, tmp_interview)

tmp_diary <- tempfile()
ce_download(2018, diary, tmp_diary)
```

Now we’ll want to prepare a dataset to calculate an integrated weighted
mean expenditure estimate. To do that, though, we’ll need both the Diary
and Interview data for pet expenditures. We will include the “BLS\_URBN”
variable and replace variable codes with more meaningful labels from the
CE PUMD dictionary. We will use this variable to calculate estimated
means by group later.

``` r
# Prepare the integrated data
pets_integrated <- ce_prepdata(
  year = 2018, 
  survey = integrated, 
  uccs = ce_uccs(pet_hg, "Pets", uccs_only = TRUE),
  recode_variables = TRUE,
  dict_path = tmp_dict,
  int_zp = tmp_interview,
  dia_zp = tmp_diary, 
  hg = pet_hg, 
  bls_urbn
)
```

Next we’ll calculate estimated, weighted, mean pet expenditures for
2018.

``` r
pet_mean <- ce_mean(pets_integrated)
```

|    agg\_exp | mean\_exp |       se |       cv |
| ----------: | --------: | -------: | -------: |
| 86944752374 |  662.3786 | 25.73933 | 3.885893 |

We can also calculate estimated, weighted, means by group using  and .
In this case we’ll use the “bls\_urbn” variable. We’ll also add “lower”
and “upper” variables to represent the upper and lower bounds of the 95%
confidence interval around each mean.

``` r
pet_mean_by_urbn <- pets_integrated %>%
  group_by(bls_urbn) %>%
  nest() %>%
  mutate(ce_mn_df = purrr::map(data, ce_mean)) %>% 
  select(-data) %>% 
  unnest(ce_mn_df) %>%
  mutate(
    lower = mean_exp - (qnorm(0.975) * se),
    upper = mean_exp + (qnorm(0.975) * se)
  )
```

| bls\_urbn |    agg\_exp | mean\_exp |       se |       cv |    lower |    upper |
| :-------- | ----------: | --------: | -------: | -------: | -------: | -------: |
| Urban     | 80961564058 |  661.6515 | 25.71857 | 3.887027 | 611.2440 | 712.0589 |
| Rural     |  5983188316 |  672.4210 | 26.02392 | 3.870182 | 621.4151 | 723.4269 |

We can also generate a barplot with error bars using the above
estimates.

``` r
ggplot(pet_mean_by_urbn, aes(x = bls_urbn, y = mean_exp, fill = bls_urbn)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +
  theme_light() +
  theme(title = element_text(hjust = 0.5)) +
  labs(
    title = "2018 estimated mean pet expenditures by urbanicity",
    x = "Urbanicity",
    y = "Estimated mean expenditure ($)",
    fill = "Urbanicity"
  )
```

<img src="man/figures/README-plot_pet_means-1.png" width="100%" />

We can also calculate weighted quantiles. Because the integrated data
come from both the Interview and Diary surveys, we’ll get Interview data
without preparing it for integration to look at only Interview survey
medians. We’ll look at the 25%, 50%, 75%, 90%, and 95% quantiles.

``` r
pet_hg_int <- ce_hg(2018, interview, tmp_hg)

pets_interview_only <- ce_prepdata(
  year = 2018, 
  survey = interview, 
  uccs = ce_uccs(pet_hg_int, "Pets", uccs_only = TRUE),
  recode_variables = TRUE,
  dict_path = tmp_dict,
  int_zp = tmp_interview,
  dia_zp = NULL, 
  hg = pet_hg_int, 
  bls_urbn
)

pet_quantiles_by_urbn <- pets_interview_only %>%
  nest(-bls_urbn) %>%
  mutate(
    ce_quant_df = map(data, ce_quantiles, c(0.25, 0.5, 0.75, 0.9, 0.95))
  ) %>% 
  select(-data) %>% 
  unnest(ce_quant_df) %>%
  pivot_wider(names_from = bls_urbn, values_from = quantile)
```

| probs | Urban | Rural |
| :---- | ----: | ----: |
| 25%   |     0 |     0 |
| 50%   |     0 |     0 |
| 75%   |     0 |     0 |
| 90%   |   191 |   175 |
| 95%   |   400 |   317 |

Using these tools allows the user the flexibility to generate weighted
CE summary statistics for any expenditure variable for just about any
demographic cross-section possible with the CE demographic variables.
