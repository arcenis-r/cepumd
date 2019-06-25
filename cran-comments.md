## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)
* macos-elcapitan-release (r-release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

checking R code for possible problems ... NOTE
  ce_mean: no visible binding for global variable ‘.’
  make.stub.96.01: no visible binding for global variable ‘.’
  read.expd: no visible binding for global variable ‘.’
  read.fmld: no visible binding for global variable ‘.’
  read.fmli: no visible binding for global variable ‘.’
  read.mtbi: no visible binding for global variable ‘.’
  Undefined global functions or variables:
  
  There are no visible bindings for the global variable '.' because I used various 'tidyverse' functions in the code with the '%>%' pipe and the '.data$' construct does not work with the '.' placeholder.
