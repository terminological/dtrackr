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

This is a resubmission of 0.5.0 due to broken URL.

This version adds 2 new functions that provide S3 methods for `tidyr::nest` and
`tidyr::unnest` for tracked data frames. These allow better handling of long
format data within a tracked data pipeline (see
[https://github.com/terminological/dtrackr/issues/36]) by allowing multi-row
data to be regarded as a single unit. This is described in detail in a new
vignette "grouping-and-nesting".

The update also includes updated options for formatting flowcharts, allowing
control of fonts, colours and orientation.

Improvements in the handling of tracked dataframes with excessive number of 
groups, through pausing and resuming tracking.

detailed changes described in NEWS.md

Many thanks.
