## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

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
