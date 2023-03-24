## Test environments

Github actions environments

* os: macOS-latest,   r: 'release'
* os: windows-latest, r: 'release'
* os: ubuntu-latest,   r: 'devel'
* os: ubuntu-latest,   r: 'release'
* os: ubuntu-latest,   r: 'oldrel-1'
* os: ubuntu-latest,   r: 'oldrel-2'

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 
(see https://github.com/terminological/dtrackr/actions/ for 
details)
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN.

## Other info
This version 0.4.0 updates previous CRAN release 0.2.4

Version 0.4.0 includes updates to 
* respond to dplyr 1.1.0 version release at end of Jan.
* work around anomalous behaviour of upstream bug in rsvg
* Updates following JOSS code review, primarily in documentation although some 
API updates

detailed changes described in NEWS.md

Version 0.2.5 got rejected as my maintainer email has changed. I have sent an
email on 10:20 on 19th Decemeber to CRAN-submissions@R-project.org from the old
maintainer address (rc538@exeter.ac.uk) explaining the change.

Many thanks.
