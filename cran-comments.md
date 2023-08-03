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
(see https://github.com/terminological/dtrackr/actions/ for details)
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies
There are currently no downstream dependencies for this package in CRAN.

## Other info
This version 0.4.4 updates previous CRAN release 0.4.0

Version 0.4.4 includes updates to 
* fix regression bugs from migration to dplyr 1.1.0
* improve error messages, remove annoying messages and resume tracking from
paused dataframes automatically.
* support additional function from dplyr 1.1.0 (reframe)

detailed changes described in NEWS.md

Many thanks.
