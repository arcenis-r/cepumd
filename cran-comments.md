## Added articles
I have added some articles to publish on GitHub pages. These articles are not appropriate as vignettes because they do not focus only on the functions of the package, but on larger workflows that use the functions to show how they might be used in a project. This required suggesting three additional packages in the DESCRIPTION file, but no other substantive changes to the {cepumd} package.

I also edited the example in ce_prepdata() to avoid an error in one of CRAN's automatic checks.

The changes in this submission are purely in the documentation and no substantive changes have been made to the functionality.

## Arguments with Non-Standard Evaluation
The exported functions of the package are designed to allow unquoted arguments, i.e., non-standard evaluation. For example, the last argument in the example in ce_prepdata() (shown below) will be read as a "..." argument and be quoted inside of the function. If the example were run with a valid file path for the "dia_zip" argument the function would run as intended.

 pets_dia <- ce_prepdata(
   year = 2021,
   survey = integrated,
   uccs = pet_uccs,
   integrate_data = FALSE,
   hg = my_hg,
   dia_zip = "diary21.zip"
   sex_ref
 )


## Calls to \dontrun{}
The functions `ce_uccs()` and `ce_prepdata()` both contain examples that are
wrapped in `\dontrun{}` because in both cases the examples include calls to file
names that may be different on the user's machine.

## Reduced size of tarball
UPDATE: Upon further review the tarball was still a little over 11MB and is now
below the 5MB threshold after reducing the size of the test data further.

The manual review of this package found the tarball to be 5MB and the maintainer
was asked to reduce the size and it is now about 1/5 of the original size.

─  Building package
─  Uploading package
─  Preparing build, see status at
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-f41f1c2f767346918ea01d60a24f8722
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-d7c25235505f4c9289da4ce932a0788d
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-4f4d0b24442f44a69e595fa5e2f3cc91
─  Build started
─  Creating new user
─  Downloading and unpacking package file
─  Querying package dependencies
─  Installing package dependencies
─  Running R CMD check
   setting _R_CHECK_FORCE_SUGGESTS_ to false
   setting R_COMPILE_AND_INSTALL_PACKAGES to never
   setting R_REMOTES_STANDALONE to true
   setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
   setting _R_CHECK_FORCE_SUGGESTS_ to true
   setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
   'getOption("repos")' replaces Bioconductor standard repositories, see
   'help("repositories", package = "BiocManager")' for details.
   Replacement repositories:
       CRAN: https://cloud.r-project.org
─  using log directory 'C:/Users/USERmjgpWxkuNt/cepumd.Rcheck'
─  using R Under development (unstable) (2024-03-01 r86033 ucrt)
─  using platform: x86_64-w64-mingw32 (1.6s)
─  R was compiled by
       gcc.exe (GCC) 12.3.0
       GNU Fortran (GCC) 12.3.0
─  running under: Windows Server 2022 x64 (build 20348) (817ms)
─  using session charset: UTF-8
─  using option '--as-cran'
✔  checking for file 'cepumd/DESCRIPTION'
─  checking extension type ... Package (818ms)
─  this is package 'cepumd' version '2.1.0'
─  package encoding: UTF-8
─  checking CRAN incoming feasibility ... [11s] NOTE
   Maintainer: 'Arcenis Rojas <arcenis.rojas@gmail.com>'
   
   New submission
   
   Possibly misspelled words in DESCRIPTION:
     BLS (12:63)
     Microdata (10:5)
     PUMD (7:59, 10:16, 14:34)
   
   Found the following (possibly) invalid URLs:
     URL: https://www.bls.gov/cex
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/ce_source_integrate.xlsx
       From: man/ce_prepdata.Rd
       Status: 403
       Message: Forbidden
       From: man/ce_hg.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd.htm
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd_disclosure.htm
       From: man/ce_mean.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd_doc.htm
       From: man/ce_prepdata.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/opub/hom/cex/calculation.htm
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
   
   Size of tarball: 11695018 bytes
     URL: https://www.bls.gov/cex/csxguide.pdf
✔  checking package namespace information
✔  checking package dependencies (906ms)
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files
✔  checking for hidden files and directories (833ms)
✔  checking for portable file names
✔  checking whether package 'cepumd' can be installed
✔  checking installed package size
✔  checking package directory (822ms)
✔  checking for future file timestamps
✔  checking 'build' directory
✔  checking DESCRIPTION meta-information
✔  checking top-level files (824ms)
✔  checking for left-over files
✔  checking index information
✔  checking package subdirectories
✔  checking R files for non-ASCII characters (1.6s)
✔  checking R files for syntax errors
✔  checking whether the package can be loaded
✔  checking whether the package can be loaded with stated dependencies
✔  checking whether the package can be unloaded cleanly (818ms)
✔  checking whether the namespace can be loaded with stated dependencies
✔  checking whether the namespace can be unloaded cleanly
✔  checking loading without being on the library search path
✔  checking use of S3 registration (820ms)
✔  checking dependencies in R code
✔  checking S3 generic/method consistency
✔  checking replacement functions
✔  checking foreign function calls (820ms)
✔  checking R code for possible problems
✔  checking Rd files
✔  checking Rd metadata
✔  checking Rd line widths (814ms)
✔  checking Rd cross-references
✔  checking for missing documentation entries
✔  checking for code/documentation mismatches
✔  checking Rd \usage sections (816ms)
✔  checking Rd contents
✔  checking for unstated dependencies in examples
✔  checking installed files from 'inst/doc'
✔  checking files in 'vignettes' (1.6s)
✔  checking examples
✔  checking for unstated dependencies in 'tests'
─  checking tests
✔  Running 'spelling.R' (815ms)
    [69s] OK
   * checking for unstated dependencies in vignettes ... OK
   * checking package vignettes ... OK
   * checking re-building of vignette outputs ... OK
   * checking PDF version of manual ... [12s] OK
   * checking HTML version of manual ... OK
   * checking for non-standard things in the check directory ... NOTE
   Found the following files/directories:
     ''NULL''
   * checking for detritus in the temp directory ... NOTE
   Found the following files/directories:
     'lastMiKTeXException'
   * DONE
   
   Status: 3 NOTEs
─  Done with R CMD check
─  Cleaning up files and user

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Explanation of notes from devtools::check_rhub()
I ran both the standard RMD check and the check for Windows. The two sections below each show the same three notes. The first of the three notes relates to word spelling and the possible problems with validity of hyperlinks. I have checked all of these and the words are either acronyms, proper nouns, or words that are common in the domain and the hyperlinks are all valid. The next two notes are about files or directories that were found (''NULL'' and 'lastMiKTeXException'). I cannot actually find these files anywhere within the package files, so I'm not sure how to address this issue, but it does not seem to hamper functionality.


## devtools::check_rhub() results

✔  checking for file 'C:\r-projects\cepumd/DESCRIPTION' ... 
─  preparing 'cepumd': (766ms)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (4.8s)
─  checking for LF line-endings in source and make files and shell scripts (337ms)
─  checking for empty or unneeded directories
   Omitted 'LazyData' from DESCRIPTION
─  building 'cepumd_2.1.0.tar.gz'
   
─  Uploading package
─  Preparing build, see status at
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-faec93ed305d443fa186d722ce18fc37
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-0d211c8856df46519f245b75dcdb9310
   https://builder.r-hub.io/status/cepumd_2.1.0.tar.gz-2078a72a8130461fbb75e86f605a063f
─  Build started
─  Creating new user
─  Downloading and unpacking package file
─  Querying package dependencies
─  Installing package dependencies
─  Running R CMD check
   setting _R_CHECK_FORCE_SUGGESTS_ to false
   setting R_COMPILE_AND_INSTALL_PACKAGES to never
   setting R_REMOTES_STANDALONE to true
   setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
   setting _R_CHECK_FORCE_SUGGESTS_ to true
   setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
   'getOption("repos")' replaces Bioconductor standard repositories, see
   'help("repositories", package = "BiocManager")' for details.
   Replacement repositories:
       CRAN: https://cloud.r-project.org
─  using log directory 'C:/Users/USERnqwWEYOSBZ/cepumd.Rcheck'
─  using R Under development (unstable) (2023-11-18 r85554 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 12.3.0
       GNU Fortran (GCC) 12.3.0
─  running under: Windows Server 2022 x64 (build 20348)
─  using session charset: UTF-8
─  using option '--as-cran'
✔  checking for file 'cepumd/DESCRIPTION'
─  checking extension type ... Package
─  this is package 'cepumd' version '2.1.0'
─  package encoding: UTF-8
─  checking CRAN incoming feasibility ... [12s] NOTE (7.7s)
   Maintainer: 'Arcenis Rojas <arcenis.rojas@gmail.com>'
   
   New submission
   
   Possibly misspelled words in DESCRIPTION:
     BLS (12:63)
     Microdata (10:5)
     PUMD (7:59, 10:16, 14:34)
   
   Found the following (possibly) invalid URLs:
     URL: https://www.bls.gov/cex
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/ce_source_integrate.xlsx
       From: man/ce_prepdata.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/csxguide.pdf
       From: man/ce_hg.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd.htm
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd_disclosure.htm
       From: man/ce_mean.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/cex/pumd_doc.htm
       From: man/ce_prepdata.Rd
       Status: 403
       Message: Forbidden
     URL: https://www.bls.gov/opub/hom/cex/calculation.htm
       From: DESCRIPTION
             man/cepumd-package.Rd
       Status: 403
       Message: Forbidden
   
   Size of tarball: 49069291 bytes
✔  checking package namespace information
✔  checking package dependencies
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files
✔  checking for hidden files and directories
✔  checking for portable file names
─  checking whether package 'cepumd' can be installed ... [10s] OK
✔  checking installed package size
✔  checking package directory
✔  checking for future file timestamps (6.8s)
✔  checking 'build' directory
✔  checking DESCRIPTION meta-information
✔  checking top-level files
✔  checking for left-over files
✔  checking index information
✔  checking package subdirectories
✔  checking R files for non-ASCII characters
✔  checking R files for syntax errors
✔  checking whether the package can be loaded
✔  checking whether the package can be loaded with stated dependencies
✔  checking whether the package can be unloaded cleanly
✔  checking whether the namespace can be loaded with stated dependencies
✔  checking whether the namespace can be unloaded cleanly
✔  checking loading without being on the library search path
✔  checking use of S3 registration
✔  checking dependencies in R code
✔  checking S3 generic/method consistency
✔  checking replacement functions
✔  checking foreign function calls
✔  checking R code for possible problems (3m 6.6s)
✔  checking Rd files
✔  checking Rd metadata
✔  checking Rd line widths
✔  checking Rd cross-references
✔  checking for missing documentation entries
✔  checking for code/documentation mismatches
✔  checking Rd \usage sections
✔  checking Rd contents
✔  checking for unstated dependencies in examples
✔  checking installed files from 'inst/doc'
✔  checking files in 'vignettes'
✔  checking examples
✔  checking for unstated dependencies in 'tests'
─  checking tests
✔  Running 'spelling.R' (1.6s)
    [97s] OK
   * checking for unstated dependencies in vignettes ... OK
   * checking package vignettes in 'inst/doc' ... OK
   * checking re-building of vignette outputs ... OK
   * checking PDF version of manual ... [12s] OK
   * checking HTML version of manual ... OK
   * checking for non-standard things in the check directory ... NOTE
   Found the following files/directories:
     ''NULL''
   * checking for detritus in the temp directory ... NOTE
   Found the following files/directories:
     'lastMiKTeXException'
   * DONE
   
   Status: 3 NOTEs
─  Done with R CMD check
─  Cleaning up files and user
    

── cepumd 2.1.0: NOTE

  Build ID:   cepumd_2.1.0.tar.gz-faec93ed305d443fa186d722ce18fc37
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  10m 1.6s ago
  Build time: 7m 47s

❯ checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Arcenis Rojas <arcenis.rojas@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    BLS (12:63)
    Microdata (10:5)
    PUMD (7:59, 10:16, 14:34)
  
  Found the following (possibly) invalid URLs:
    URL: https://www.bls.gov/cex
      From: DESCRIPTION
            man/cepumd-package.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/cex/ce_source_integrate.xlsx
      From: man/ce_prepdata.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/cex/csxguide.pdf
      From: man/ce_hg.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/cex/pumd.htm
      From: DESCRIPTION
            man/cepumd-package.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/cex/pumd_disclosure.htm
      From: man/ce_mean.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/cex/pumd_doc.htm
      From: man/ce_prepdata.Rd
      Status: 403
      Message: Forbidden
    URL: https://www.bls.gov/opub/hom/cex/calculation.htm
      From: DESCRIPTION
            man/cepumd-package.Rd
      Status: 403
      Message: Forbidden
  
  Size of tarball: 49069291 bytes

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

── cepumd 2.1.0: IN-PROGRESS

  Build ID:   cepumd_2.1.0.tar.gz-0d211c8856df46519f245b75dcdb9310
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  10m 1.9s ago


── cepumd 2.1.0: IN-PROGRESS

  Build ID:   cepumd_2.1.0.tar.gz-2078a72a8130461fbb75e86f605a063f
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  10m 1.9s ago

## RMD check results for Windows

Build ID:	cepumd_2.1.0.tar.gz-faec93ed305d443fa186d722ce18fc37
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	8 minutes 9.7 seconds ago
Build time:	7 minutes 47 seconds
NOTES:
* checking CRAN incoming feasibility ... [12s] NOTE
Maintainer: 'Arcenis Rojas <arcenis.rojas@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  BLS (12:63)
  Microdata (10:5)
  PUMD (7:59, 10:16, 14:34)

Found the following (possibly) invalid URLs:
  URL: https://www.bls.gov/cex
    From: DESCRIPTION
          man/cepumd-package.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/cex/ce_source_integrate.xlsx
    From: man/ce_prepdata.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/cex/csxguide.pdf
    From: man/ce_hg.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/cex/pumd.htm
    From: DESCRIPTION
          man/cepumd-package.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/cex/pumd_disclosure.htm
    From: man/ce_mean.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/cex/pumd_doc.htm
    From: man/ce_prepdata.Rd
    Status: 403
    Message: Forbidden
  URL: https://www.bls.gov/opub/hom/cex/calculation.htm
    From: DESCRIPTION
          man/cepumd-package.Rd
    Status: 403
    Message: Forbidden

Size of tarball: 49069291 bytes
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
