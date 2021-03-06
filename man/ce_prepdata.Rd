% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ce-prepdata.R
\name{ce_prepdata}
\alias{ce_prepdata}
\title{Prepare CE data for calculating an estimated mean or median}
\usage{
ce_prepdata(year, survey, uccs, recode_variables = FALSE,
  dict_path = NULL, int_zp = NULL, dia_zp = NULL, hg = NULL, ...)
}
\arguments{
\item{year}{A year between 1997 and the last year of available CE PUMD.}

\item{survey}{One of either interview, diary, or integrated as a string or
symbol.}

\item{uccs}{A character vector of UCCs corresponding to expenditure
categories in the hierarchical grouping (HG) for a given year and survey.}

\item{recode_variables}{A logical indicating whether to recode all coded
variables except 'UCC' using the codes in the CE's excel dictionary which can
be downloaded from the
\href{https://www.bls.gov/cex/pumd_doc.htm}{CE Documentation Page}}

\item{dict_path}{A string indicating the path where the CE PUMD dictionary
is stored if already stored. If the file does not exist and
\code{recode_variables = TRUE} the dictionary will be stored in this path.
The default is \code{NULL} which causes the zip file to be stored in
temporary memory during function operation.}

\item{int_zp}{String indicating the path of the Interview data zip file if
already stored. If the file does not exist its corresponding zip file will
be stored in that path. The default is \code{NULL} which causes the zip file
to be stored in temporary memory during function operation.}

\item{dia_zp}{Same as \code{int_zip} above, but for Diary data.}

\item{hg}{A data frame that has, at least, the title, level, ucc, and
factor columns of a CE HG file. Calling \code{\link{ce_hg}} will generate a
valid HG file.}

\item{...}{Variables to include in the dataset from the family
characteristics file. This is intended to allow the user to calculate
estimates for subsets of the data.}
}
\value{
A data frame containing the following columns:
\itemize{
  \item newid - A consumer unit (CU), or household, identifier
  \item finlwt21 - CU weight variable
  \item wtrep01 through wtrep44 - CU replicate weight variables (see details)
  \item ... - Any family characteristics variables that were kept
  \item mo_scope - Months in scope (see details)
  \item popwt - An adjusted weight meant to account for the fact that a CUs
        value of finlwt21 is meant to be representative of only 1 quarter of
        data (see details)
  \item ucc - The UCC for a given expenditure
  \item cost - The value of the expenditure (in U.S. Dollars)
  \item survey - An indicator of which survey the data come from: "I" for
        Interview and "D" for Diary.
}
}
\description{
Reads in the family characteristics (FMLI/-D) and expenditure
tabulation (MTBI/EXPD) files and merges the relevant data for calculating a
weighted mean or median.
}
\details{
CE microdata include 45 weights. The primary weight that is used for
calculating estimated means and medians is finlwt21. The 44 replicate weights
are computed using Balanced Repeated Replication (BRR) and are used for
calculating weighted standard errors.

"Months in scope" refers to the proportion of the data collection quarter for
which a CU reported expenditures. For the Diary survey the months in scope is
always 3 because the expenditure data collected are meant to be reported for
the quarter in which they are collected. The Interview Survey, on the other
hand, is a quarterly, rolling, recall survey and the CU's report expenditures
for the 3 months previous to the month in which the data are collected. For
example, if a CU was interviewed in February 2017, then they would be
providing data for November 2016, December 2016, and January 2017. If one is
calculating a weighted estimated mean for the 2017 calendar year, then only
the January 2017 data would be "in scope."

CE data are reported quarterly, but the sum of the weights (finlwt21) is
for all CU's is meant to represent the total number of U.S. CU's for a given
year. Since a calculating a calendar year estimate requires the use of 4
quarters of data and the sum of the weights in each quarter equals the
number of households in the U.S. for a given year, adding up the sums of the
weights in the 4 quarters of data would yield a total number of households
that is approximately 4 times larger than the actual number of households in
the U.S. in the corresponding year.

Since some UCC's can appear in both surveys, for the purposes of integration,
the CE has a source selection procedure by which to choose which source data
will be taken from for a given UCC. For example, of the 4 UCC's in the "Pets"
category in 2017 two were sourced for publication from the Diary and two from
the Interview. Please download the CE Source Selection Document for a
complete listing: \url{https://www.bls.gov/cex/ce_source_integrate.xlsx}.
}
\examples{
# The following workflow will prepare a dataset for calculating diary
# pet expenditures for 2017 keep the "sex_ref" variable in the data to
# potentially calculate means by sex of the reference person.

# First generate an HG file
my_hg <- ce_hg(2017, diary)

# Store a vector of UCC's in the "Pets" category
pet_uccs <- ce_uccs(my_hg, "Pets")

# Store the diary data (not run)
\dontrun{
pets_dia <- ce_prepdata(
  year = 2017,
  survey = diary,
  uccs = pet_uccs,
  integrate_data = TRUE,
  recode_variables,
  zp = NULL,
  hg = my_hg,
  sex_ref
)
}
}
