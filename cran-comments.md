## Test environments

Github actions environments
* ubuntu-20-04 with R 4.1.0
* ubuntu-20-04 with R 3.6.1
* ubuntu-20-04 with R 4.2.0
* macOS-11 with R 4.1.0
* windows-2022 with R 4.1.0
* ubuntu-18.04 with R r-devel

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 
(see https://github.com/terminological/dtrackr/actions for details)

── dtrackr 0.2.3: OK

  Build ID:   dtrackr_0.2.3.tar.gz-20b3f099c3e649f9b5ce94daae5ad779
  Platform:   macOS 10.13.6 High Sierra, R-release, CRAN's setup
  Submitted:  1m 46.5s ago
  Build time: 1m 42s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Other info

The project is hosted on github - https://github.com/terminological/dtrackr
This is a re-submission of an as-yet unpublished package correcting the following issues:

29th June 2022

* Please only ship the CRAN template for the MIT license. 
     - DONE.
* Please single quote software names in the Description field. 
     - DONE.
* Please change http --> https, add trailing slashes, or follow moved content as appropriate. 
     - DONE.
* The title field should be in title case. 
     - DONE.
* The Description field should not start with the package name, 'This package' or similar. Please fix and resubmit. 
     - DONE.
* additionally fixed some typos.

1st July 2022

* Please do not start the description with "This package", package name,
title or "R package for". 
     - DONE.
* Please write your full name into the LICENCE file. -> instead of Rob,
write Robert. 
     - DONE.
* Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar). 
Missing Rd-tags in up to 13 .Rd files,  ...)
     - DONE. APOLS - THOUGHT THESE WOULD BE PICKED UP FROM THE DPLYR DOCUMENTATION. HAVE ALSO 
FULLY REVIEWED REST OF DOCUMENTATION AND MADE EXAMPLES CLEARER.
* Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace. 
     - DONE. I THINK THERE WERE TWO EXAMPLES OF THIS WHICH HAVE BEEN COMMENTED OUT.
* Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos. 
     - DONE.

4th July 2022

* I think you missed to add the \value in one of your .Rd files. Please 
also add it there as you did in the others.
     - DONE. APOLOGIES OLD VERSION OF DEVTOOLS GENERATED FILE. 
