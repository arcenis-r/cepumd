
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

The purpose of `cepumd` is to make working with Consumer Expenditure
Surveys (CE) Public-Use Microdata (PUMD) easier toward calculating mean,
weighted, annual expenditures (henceforth “mean expenditures”). The
challenges `cepumd` seeks to address deal primarily with pulling
together the necessary data toward this end. Some of the overarching
ideas underlying the package are as follows:

- Use a [Tidyverse](https://www.tidyverse.org/) framework for most
  operations and be (hopefully) generally
  [Tidyverse](https://www.tidyverse.org/) friendly
- The best practice is to put all required data in one place, which
  `cepumd` does by default
- Balance the effort to make the end user’s experience with CE PUMD
  easier while being flexible enough to allow that user to perform any
  analysis with the data they wish
- Only designed to help users calculate mean **expenditures** on and of
  the consumer unit (CU), i.e., not income, not assets, not liabilities,
  not gifts, at least for now

For a fuller description of `cepumd` and some sample workflows please
visit <https://arcenis-r.github.io/ajr-portfolio/cepumd.html>

And for more information on the CE, please visit
<https://www.bls.gov/cex/>.

## Challenges addressed by {cepumd}

`cepumd` seeks to address challenges in three categories: data
gathering/organization; managing data inconsistencies; and calculating
weighted, annual metrics.

- **Data gathering/organization**
  - Facilitate data and metadata downloads with `ce_download()`,
    `store_ce_hg()`, and `store_ce_dict()`
  - Convert hierarchical grouping (HG) files to data tables using
    `ce_hg()`
  - Help the user identify the Universal Classification Codes (UCCs)
    related to their analysis using a combination of `ce_hg()` and
    `ce_uccs()`
  - Combine all required files and variables using `ce_prepdata()`
- **Managing data inconsistencies**
  - Provide the ability to recode variable categories using the CE
    Dictionary for Interview and Diary Surveys
  - Resolve some inconsistencies such as differences code definitions
    between the Interview and Diary (check the definitions of the
    “FAM_TYPE” variable categories in 2015 for an example)
  - Provide useful errors or warnings when there are multiple categories
    of something the user is trying to access, e.g., some titles in the
    hierarchical grouping files (“stub” or “HG” files) repeat and
    requires more careful selection of UCCs
- **Calculating weighted, annual metrics**
  - Calculate a mean expenditure with `ce_mean()` or expenditure
    quantile with `ce_quantile()`
  - Account for the factor (annual vs. quarterly expenditure)
  - Account for the “months in scope” of a given consumer unit (CU)
  - Annualize expenditures for either Diary or Interview expenditures
  - Integrate Interview and Diary data as necessary

## Installation

You can install the development version of `cepumd` from
[GitHub](https://github.com), but you’ll first need the `devtools`
package:

``` r
if (!"devtools" %in% installed.packages()[, "Package"]) {
  install.packages("devtools", dependencies = TRUE)
}

devtools::install_github("arcenis-r/cepumd")
```

## Key cepumd functions

- The workhorse of `cepumd` is `ce_prepdata()`. It merges the household
  characteristics file (FMLI/-D) with the corresponding expenditure
  tabulation file (MTBI/EXPD) for a specified year, adjusts weights for
  months-in-scope and the number of collection quarters, adjusts some
  cost values by their periodicity factor (some cost categories are
  represented as annual figures and others as quarterly). With the
  recent update it only requires the first 3 arguments to function: the
  year, the survey type, and one or more valid UCCs. `ce_prepdata()` now
  creates all of the other necessary objects within the function if not
  provided.

- There are three functions for downloading the data and necessary
  documentation:

  - `ce_download()` downloads zip files for a given year and survey
    instrument directly from the CE website
  - `store_ce_hg()` downloads the zip file containing all HG files
    maintained by the CE to the specified location.
  - `store_ce_dict()` downloades the CE PUMD dictionary from CE’s
    website to the specified location.

- There are two functions for wrangling hierarchical grouping data into
  more useable formats:

  - `ce_hg()` pulls the requested type of HG file (Interview, Diary, or
    Integrated) for a specified year.
  - `ce_uccs()` filters the HG file for the specified expenditure
    category and returns either a data frame with only that section of
    the HG file or the Universal Classification Codes (UCCs) that make
    up that expenditure category.

- There are two functions that the user can use to calculate CE summary
  statistics:

  - `ce_mean()` calculates a mean expenditure, standard error of the
    mean, coefficient of variation, and an aggregate expenditure.
  - `ce_quantiles()` calculates weighted expenditure quantiles. It is
    important to note that calculating medians for integrated
    expenditures is not recommended because the calculation involves
    using weights from both the Diary and Survey instruments.

- There are two utility functions to make the workflow a bit easier:

  - `ce_pumd_years()` scrapes the main PUMD website to get a vector of
    years for which PUMD are available. The vector is limited to the
    years for which there are also HG files available.
  - `ce_cleanup()` deletes a file containing CE data that may only be
    necessary temporarily.
