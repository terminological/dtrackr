
# dtrackr: Track your Data Pipelines <a href='https://dplyr.tidyverse.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/terminological/dtrackr/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/dtrackr/actions)
[![DOI](https://zenodo.org/badge/335974323.svg)](https://zenodo.org/badge/latestdoi/335974323)
[![dtrackr status
badge](https://terminological.r-universe.dev/badges/dtrackr)](https://terminological.r-universe.dev)
<!-- [![DOI](https://joss.theoj.org/papers/10.21105/joss.04707/status.svg)](https://doi.org/) -->
<!-- badges: end -->

## Overview

Accurate documentation of a data pipeline is a first step to
reproducibility, and a flow chart describing the steps taken to prepare
data is a useful part of this documentation. In analyses that relies on
data that is frequently updated, documenting a data flow by copying and
pasting row counts into flowcharts in PowerPoint becomes quickly
tedious. With interactive data analysis, and particularly using
RMarkdown, code execution sometimes happens in a non-linear fashion, and
this can lead to, at best, confusion and at worst erroneous analysis.
Basing such documentation on what the code does when executed
sequentially can be inaccurate when the data has being analysed
interactively.

The goal of `dtrackr` is to take away this pain by instrumenting and
monitoring a dataframe through a `dplyr` pipeline, creating a
step-by-step summary of the important parts of the wrangling as it
actually happened to the dataframe, right into dataframe metadata
itself. This metadata can be used to generate documentation as a
flowchart, and allows both a quick overview of the data and also a
visual check of the actual data processing.

## Installation

In general use `dtrackr` is expected to be installed alongside the
`idyverse` set of packages. It is recommended to install `tidyverse`
first.

Binary packages of `dtrackr` are available on CRAN and r-universe for
`macOS` and `Windows`. `dtrackr` can be installed from source on Linux.
`dtrackr` has been tested on R versions 3.6, 4.0, 4.1 and 4.2.

You can install the released version of `dtrackr` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dtrackr")
```

### System dependencies for installation from source

For installation from source on in Linux, `dtrackr` has required
transitive dependencies on a few system libraries. These can be
installed with the following commands:

``` bash
# Ubuntu 20.04 and other debian based distributions:
sudo apt-get install libcurl4-openssl-dev libssl-dev librsvg2-dev \
  libicu-dev libnode-dev libpng-dev libjpeg-dev libpoppler-cpp-dev

# Centos 8
sudo dnf install libcurl-devel openssl-devel librsvg2-devel \
  libicu-devel libpng-devel libjpeg-turbo-devel poppler-devel

# for other linux distributions I suggest using the R pak library:
# install.packages("pak")
# pak::pkg_system_requirements("dtrackr")

# N.B. There are additional suggested R package dependencies on 
# the `tidyverse` and `rstudioapi` packages which have a longer set of dependencies. 
# We suggest you install them individually first if required.
```

### Alternative versions of `dtrackr`

Early release versions are available on the `r-universe`. This will
typically be more up to date than CRAN.

``` r
# Enable repository from terminological
options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install dtrackr in R
install.packages('dtrackr')
```

The unstable development version is available from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("terminological/dtrackr")
```

## Example usage

Suppose we are constructing a data set with out initial input being the
`iris` data. Our analysis depends on some `cutOff` parameter and we want
to prepare a stratified data set that excludes flowers with narrow
petals, and those with the biggest petals of each Species. With
`dtrackr` we can mix regular `dplyr` commands with additional `dtrackr`
commands such as `comment` and `status`, and an enhanced implementation
of `dplyr::filter`, called `exclude_all`, and `include_any`.

``` r
# a pipeline parameter
cutOff = 3

# the pipeline
dataset = iris %>% 
  track() %>%
  status() %>%
  group_by(Species) %>%
  status(
    short = p_count_if(Sepal.Width<cutOff), 
    long= p_count_if(Sepal.Width>=cutOff), 
    .messages=c("consisting of {short} short sepal <{cutOff}","and {long} long sepal >={cutOff}")
  )  %>%
  exclude_all(
    Petal.Width<0.3 ~ "excluding {.excluded} with narrow petals",
    Petal.Width == max(Petal.Width) ~ "and {.excluded} outlier"
  ) %>%
  comment("test message") %>%
  status(.messages = "{.count} of type {Species}") %>%
  ungroup() %>%
  status(.messages = "{.count} together with cutOff {cutOff}") 
```

Having prepared our dataset we conduct our analysis, and want to write
it up and prepare it for submission. As a key part of documenting the
data pipeline a visual summary is useful, and for bio-medical journals
or clinical trials often a requirement.

``` r
dataset %>% flowchart()
```

![](man/figures/README-flowchart.png?raw=true)

And your publication ready data pipeline, with any assumptions you care
to document, is creates in a format of your choice (as long as that
choice is one of `pdf`, `png`, `svg` or `ps`), ready for submission to
Nature.

This is a trivial example, but the more complex the pipeline, the bigger
benefit you will get.

Check out the [main documentation for detailed
examples](https://terminological.github.io/dtrackr/)

## Testing and integration

For testing `dtrackr` uses the `testthat` framework. It is configured to
run both the unit tests and the functional tests in the code examples.

``` r
# assuming dtrackr has been cloned from github into the working directory 
# location

devtools::load_all()

# Long list of system dependencies in Ubuntu 20.04 including all suggested 
# dependencies:
# librsvg2-dev libicu-dev libcurl4-openssl-dev libssl-dev libnode-dev make 
# pandoc imagemagick libmagick++-dev gsfonts default-jdk libxml2-dev 
# zlib1g-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev 
# libjpeg-dev libpng-dev libtiff-dev git libgit2-dev

pak::local_system_requirements("ubuntu","20.04")
install.packages(c("here","tidyverse","devtools","testthat","pkgdown"))

# Examples:
devtools::run_examples()

# automated testing with testthat (also runs all examples):
devtools::test()

# pkgdown site building (which executes all the vignettes):
pkgdown::build_site()
```

Github workflows are enabled on this repository for continuous
integration. These perform an `R CMD check` on code commits, which will
run all `testthat` unit tests, run all man page examples, and build all
the vignettes.

For vignette building there are dependencies on the `tidyverse` package,
as well as other system libraries required for vignette building with
`pandoc`. The CI tests check the library can be installed on macOs,
windows, and Ubuntu, with R versions 3.6.1, 4.1, 4.2 and the R
development branch.

On tagged releases, additional Github workflows are triggered by in the
`r-universe` repository, to build binary releases on a range of
platforms.
