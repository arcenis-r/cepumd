# cepumd 2.0.0
* Hard deprecated functions for downloading data and metadata from the BLS website due to a policy change that no longer allows webscraping.
* Using readr::read_csv() and readr::read_lines() funcitonality for reading directly from a zip file.
* No longer requiring all data to be in one single place, but allowing the flexibility to refer to zip files and other metadata files by individual path names.
* Changed "%>%" pipes to "|>" as well as anonymous function references from "~ .x" notation to "\\(x) " notation

# cepumd 1.4.1
Corrected the filter conditions in reading in Diary monthly tabulation files. In computing Diary expenditures the CE does not filter by year. Also added Github CI, a GPL3 License, {pkgdown} files, and files for a vignette.

# cepumd 1.4.0
* Refactored ce_prepdata to make `...` more useful and allow the user to accept default arguments.
* Added the "ref_yr" and "ref_mo" variables to the output of `ce_prepdata()` to allow users to make inflation or other adjustments to the "cost" values
* Refactored `ce_means()` to conform with newer Tidyverse functions and to summarize input data by "survey", "ucc", and "newid" (as opposed to down to the level of reference month and year as done by `ce_prepdata()`)

# cepumd 1.3.0
Added the `ucc_group` parameter to ce_uccs() to deal with the fact that some UCC titles are identical. Combinations of `ucc_group` and `title` are identical in the stub files. `ucc_group` is the primary parameter. Also, in previous versions `ce_hg()` only unwrapped titles that wrapped over 2 lines, thereby leaving titles that wrapped over more lines incomplete. That method is now updated to be able to deal with multi-line titles of more than 2 lines.

# cepumd 1.2.1
Patched an issue with ce_uccs() such that it coutldn't handle cases where the level decreases by more than one in finding only the UCCs directly under the title row of the requested expenditure category.

# cepumd 1.2.0
<ul>Broke ce_prepdata() down to use helper functions to make it easier to refactor</ul>
<ul>Added "own_codebook" argument to ce_prepdata() to allow the user to use a customized codebook.</ul>
<ul>Suppressed column type messages from {readr} functions.</ul>

# cepumd 1.1.0
I added a dependency on \code{tidyselect 1.2.0} which does not require the `.data$` prefix for data objects used in Tidyverse pipelines. I also corrected some of the fixed width position conditions `ce_hg()` for converting HG files to tables as the Diary stubs from 1997 through 2012 and a few others were being read incorrectly. I also made some changes to the example workflows in the README file.

# cepumd 1.0.1
This version of \code{cepumd} incorporates one important change from the perspective of design philosophy and adds a couple of utility functions to help the user.

First, it imposes a bit more strict structure on file management. One of the biggest challenges of using Consumer Expenditure Survey (CE) Public-Use Microdata (PUMD) is getting the data. The first major advantage of using this package is that it does this for the user with just a few simple functions. However, in the first version of this package there was very little structure given to how files should be stored once downloaded. In 2021 the CE changed the way it uploads data so that Interview zip files no longer contain all 5 quarters of data necessary to calculate estimated means for a given calendar year. Now, for weighted estimates of annual expenditures any calendar year after 2020, the user has to download both the zip file for the year of interest and the previous year. As written, the previous version of \code{cepumd} was generating incorrect estimates because it would only pull data for the year of interest. In adapting the code for the new way the CE stores data, I found it necessary to impose a bit more structure on the storage of the data in order to keep the coding burden on myself and the end user low. This actually resulted in a set of functions that are now easier to work with. A notable change is that the functions for downloading data now have an argument in which the user can provide the path to a directory that they want to use to store data. Alternatively, \code{cepumd} will create a sub-directory in the R session's temporary directory.

The two utility functions are `ce_pumd_years()` and `ce_cleanup()`. `ce_pumd_years()` allows the user to get a vector of the years of data available to them for download. This function is also used for validity checks across the functions in case a user asks to download data for a year for which no data is available `ce_cleanup()` simply removes the directory the user provides to it. Alternatively, if run with no argument, it will look for a subdirectory called "ce-data" in the R session's temporary directory (`tempdir()`) and delete it if it is there.

# cepumd 0.2.2
This is the first version of \code{cepumd} being released on CRAN   which is designed to help Consumer Expenditure Survey (CE) Public-Use Microdata (PUMD) users generate weighted means and medians following the same methods used by the CE.
