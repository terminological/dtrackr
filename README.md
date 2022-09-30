
# dtrackr: Track your Data Pipelines <a href='https://dplyr.tidyverse.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/terminological/dtrackr/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/dtrackr/actions)
[![DOI](https://zenodo.org/badge/335974323.svg)](https://zenodo.org/badge/latestdoi/335974323)
[![dtrackr status
badge](https://terminological.r-universe.dev/badges/dtrackr)](https://terminological.r-universe.dev)
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

### System dependencies for installatio from source

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

<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 2.40.1 (20161225.0304)
 -->
<!-- Title: %0 Pages: 1 -->
<svg width="809pt" height="350pt"
 viewBox="0.00 0.00 809.47 350.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 346)">
<title>%0</title>
<polygon fill="#ffffff" stroke="transparent" points="-4,4 -4,-346 805.4712,-346 805.4712,4 -4,4"/>
<!-- 16&#45;&gt;17 -->
<g id="edge1" class="edge">
<title>16:s&#45;&gt;17</title>
<path fill="none" stroke="#000000" d="M332.906,-41.4C332.906,-41.4 332.906,-28.4987 332.906,-28.4987"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-28.4987 332.906,-23.4987 331.1561,-28.4987 334.6561,-28.4987"/>
</g>
<!-- 13&#45;&gt;16 -->
<g id="edge4" class="edge">
<title>13:s&#45;&gt;16</title>
<path fill="none" stroke="#000000" d="M58.906,-83.2C58.906,-83.2 58.906,-53 58.906,-53 58.906,-53 303.5903,-53 303.5903,-53"/>
<polygon fill="#000000" stroke="#000000" points="303.5904,-54.7501 308.5903,-53 303.5903,-51.2501 303.5904,-54.7501"/>
</g>
<!-- 14&#45;&gt;16 -->
<g id="edge3" class="edge">
<title>14:s&#45;&gt;16</title>
<path fill="none" stroke="#000000" d="M332.906,-83.2C332.906,-83.2 332.906,-70.1777 332.906,-70.1777"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-70.1777 332.906,-65.1777 331.1561,-70.1778 334.6561,-70.1777"/>
</g>
<!-- 15&#45;&gt;16 -->
<g id="edge2" class="edge">
<title>15:s&#45;&gt;16</title>
<path fill="none" stroke="#000000" d="M604.906,-83.2C604.906,-83.2 604.906,-53 604.906,-53 604.906,-53 362.5239,-53 362.5239,-53"/>
<polygon fill="#000000" stroke="#000000" points="362.524,-51.2501 357.5239,-53 362.5239,-54.7501 362.524,-51.2501"/>
</g>
<!-- 10&#45;&gt;13 -->
<g id="edge7" class="edge">
<title>10:s&#45;&gt;13</title>
<path fill="none" stroke="#000000" d="M58.906,-131.2C58.906,-131.2 58.906,-118.5407 58.906,-118.5407"/>
<polygon fill="#000000" stroke="#000000" points="60.6561,-118.5407 58.906,-113.5407 57.1561,-118.5407 60.6561,-118.5407"/>
</g>
<!-- 11&#45;&gt;14 -->
<g id="edge6" class="edge">
<title>11:s&#45;&gt;14</title>
<path fill="none" stroke="#000000" d="M332.906,-131.2C332.906,-131.2 332.906,-118.5407 332.906,-118.5407"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-118.5407 332.906,-113.5407 331.1561,-118.5407 334.6561,-118.5407"/>
</g>
<!-- 12&#45;&gt;15 -->
<g id="edge5" class="edge">
<title>12:s&#45;&gt;15</title>
<path fill="none" stroke="#000000" d="M604.906,-131.2C604.906,-131.2 604.906,-118.5407 604.906,-118.5407"/>
<polygon fill="#000000" stroke="#000000" points="606.6561,-118.5407 604.906,-113.5407 603.1561,-118.5407 606.6561,-118.5407"/>
</g>
<!-- 4&#45;&gt;10 -->
<g id="edge10" class="edge">
<title>4:s&#45;&gt;10</title>
<path fill="none" stroke="#000000" d="M58.906,-179.2C58.906,-179.2 58.906,-166.5407 58.906,-166.5407"/>
<polygon fill="#000000" stroke="#000000" points="60.6561,-166.5407 58.906,-161.5407 57.1561,-166.5407 60.6561,-166.5407"/>
</g>
<!-- 4&#45;&gt;7 -->
<g id="edge13" class="edge">
<title>4:e&#45;&gt;7</title>
<path fill="none" stroke="#000000" d="M117.906,-198C117.906,-198 127.029,-198 127.029,-198"/>
<polygon fill="#000000" stroke="#000000" points="127.029,-199.7501 132.029,-198 127.029,-196.2501 127.029,-199.7501"/>
</g>
<!-- 5&#45;&gt;11 -->
<g id="edge9" class="edge">
<title>5:s&#45;&gt;11</title>
<path fill="none" stroke="#000000" d="M332.906,-179.2C332.906,-179.2 332.906,-166.5407 332.906,-166.5407"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-166.5407 332.906,-161.5407 331.1561,-166.5407 334.6561,-166.5407"/>
</g>
<!-- 5&#45;&gt;8 -->
<g id="edge12" class="edge">
<title>5:e&#45;&gt;8</title>
<path fill="none" stroke="#000000" d="M393.906,-198C393.906,-198 403.0129,-198 403.0129,-198"/>
<polygon fill="#000000" stroke="#000000" points="403.013,-199.7501 408.0129,-198 403.0129,-196.2501 403.013,-199.7501"/>
</g>
<!-- 6&#45;&gt;12 -->
<g id="edge8" class="edge">
<title>6:s&#45;&gt;12</title>
<path fill="none" stroke="#000000" d="M604.906,-179.2C604.906,-179.2 604.906,-166.5407 604.906,-166.5407"/>
<polygon fill="#000000" stroke="#000000" points="606.6561,-166.5407 604.906,-161.5407 603.1561,-166.5407 606.6561,-166.5407"/>
</g>
<!-- 6&#45;&gt;9 -->
<g id="edge11" class="edge">
<title>6:e&#45;&gt;9</title>
<path fill="none" stroke="#000000" d="M665.906,-198C665.906,-198 675.0129,-198 675.0129,-198"/>
<polygon fill="#000000" stroke="#000000" points="675.013,-199.7501 680.0129,-198 675.0129,-196.2501 675.013,-199.7501"/>
</g>
<!-- 3&#45;&gt;4 -->
<g id="edge16" class="edge">
<title>3:s&#45;&gt;4</title>
<path fill="none" stroke="#000000" d="M332.906,-247C332.906,-247 58.906,-247 58.906,-247 58.906,-247 58.906,-222.2071 58.906,-222.2071"/>
<polygon fill="#000000" stroke="#000000" points="60.6561,-222.2071 58.906,-217.2071 57.1561,-222.2071 60.6561,-222.2071"/>
</g>
<!-- 3&#45;&gt;5 -->
<g id="edge15" class="edge">
<title>3:s&#45;&gt;5</title>
<path fill="none" stroke="#000000" d="M332.906,-235C332.906,-235 332.906,-222.4622 332.906,-222.4622"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-222.4622 332.906,-217.4622 331.1561,-222.4623 334.6561,-222.4622"/>
</g>
<!-- 3&#45;&gt;6 -->
<g id="edge14" class="edge">
<title>3:s&#45;&gt;6</title>
<path fill="none" stroke="#000000" d="M332.906,-247C332.906,-247 604.906,-247 604.906,-247 604.906,-247 604.906,-222.2071 604.906,-222.2071"/>
<polygon fill="#000000" stroke="#000000" points="606.6561,-222.2071 604.906,-217.2071 603.1561,-222.2071 606.6561,-222.2071"/>
</g>
<!-- 2&#45;&gt;3 -->
<g id="edge17" class="edge">
<title>2:s&#45;&gt;3</title>
<path fill="none" stroke="#000000" d="M332.906,-276.6C332.906,-276.6 332.906,-263.6987 332.906,-263.6987"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-263.6987 332.906,-258.6987 331.1561,-263.6987 334.6561,-263.6987"/>
</g>
<!-- 1&#45;&gt;2 -->
<g id="edge18" class="edge">
<title>1:s&#45;&gt;2</title>
<path fill="none" stroke="#000000" d="M332.906,-318.2C332.906,-318.2 332.906,-305.2987 332.906,-305.2987"/>
<polygon fill="#000000" stroke="#000000" points="334.6561,-305.2987 332.906,-300.2987 331.1561,-305.2987 334.6561,-305.2987"/>
</g>
<!-- 17 -->
<g id="node1" class="node">
<title>17</title>
<polygon fill="#ffffff" stroke="#000000" points="385.4948,-23.4033 280.3172,-23.4033 280.3172,-.1967 385.4948,-.1967 385.4948,-23.4033"/>
<text text-anchor="start" x="287.112" y="-9.4" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">111 together with cutOff 3</text>
</g>
<!-- 16 -->
<g id="node2" class="node">
<title>16</title>
<polygon fill="#ffffff" stroke="#000000" points="357.0807,-65.0033 308.7313,-65.0033 308.7313,-41.7967 357.0807,-41.7967 357.0807,-65.0033"/>
<text text-anchor="start" x="315.5692" y="-51" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">111 items</text>
</g>
<!-- 13 -->
<g id="node3" class="node">
<title>13</title>
<polygon fill="#ffffff" stroke="#000000" points="96.3228,-113.2 21.4892,-113.2 21.4892,-83.2 96.3228,-83.2 96.3228,-113.2"/>
<text text-anchor="start" x="28.4476" y="-100" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:setosa</text>
<text text-anchor="start" x="28.4476" y="-92" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">15 of type setosa</text>
</g>
<!-- 14 -->
<g id="node4" class="node">
<title>14</title>
<polygon fill="#ffffff" stroke="#000000" points="375.4775,-113.2 290.3345,-113.2 290.3345,-83.2 375.4775,-83.2 375.4775,-113.2"/>
<text text-anchor="start" x="297.1208" y="-100" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:versicolor</text>
<text text-anchor="start" x="297.1208" y="-92" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">49 of type versicolor</text>
</g>
<!-- 15 -->
<g id="node5" class="node">
<title>15</title>
<polygon fill="#ffffff" stroke="#000000" points="645.089,-113.2 564.723,-113.2 564.723,-83.2 645.089,-83.2 645.089,-113.2"/>
<text text-anchor="start" x="571.5648" y="-100" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:virginica</text>
<text text-anchor="start" x="571.5648" y="-92" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">47 of type virginica</text>
</g>
<!-- 10 -->
<g id="node6" class="node">
<title>10</title>
<polygon fill="#ffffff" stroke="#000000" points="93.1464,-161.2 24.6656,-161.2 24.6656,-131.2 93.1464,-131.2 93.1464,-161.2"/>
<text text-anchor="start" x="31.786" y="-148" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:setosa</text>
<text text-anchor="start" x="31.786" y="-140" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">test message</text>
</g>
<!-- 11 -->
<g id="node7" class="node">
<title>11</title>
<polygon fill="#ffffff" stroke="#000000" points="372.2997,-161.2 293.5123,-161.2 293.5123,-131.2 372.2997,-131.2 372.2997,-161.2"/>
<text text-anchor="start" x="300.4592" y="-148" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:versicolor</text>
<text text-anchor="start" x="300.4592" y="-140" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">test message</text>
</g>
<!-- 12 -->
<g id="node8" class="node">
<title>12</title>
<polygon fill="#ffffff" stroke="#000000" points="641.9116,-161.2 567.9004,-161.2 567.9004,-131.2 641.9116,-131.2 641.9116,-161.2"/>
<text text-anchor="start" x="574.9032" y="-148" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:virginica</text>
<text text-anchor="start" x="574.9032" y="-140" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">test message</text>
</g>
<!-- 4 -->
<g id="node9" class="node">
<title>4</title>
<polygon fill="#ffffff" stroke="#000000" points="117.7181,-217.2 .0939,-217.2 .0939,-179.2 117.7181,-179.2 117.7181,-217.2"/>
<text text-anchor="start" x="7" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:setosa</text>
<text text-anchor="start" x="7" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">consisting of 2 short sepal &lt;3</text>
<text text-anchor="start" x="7" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 48 long sepal &gt;=3</text>
</g>
<!-- 5 -->
<g id="node10" class="node">
<title>5</title>
<polygon fill="#ffffff" stroke="#000000" points="394.1655,-217.2 271.6465,-217.2 271.6465,-179.2 394.1655,-179.2 394.1655,-217.2"/>
<text text-anchor="start" x="278.7764" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:versicolor</text>
<text text-anchor="start" x="278.7764" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">consisting of 34 short sepal &lt;3</text>
<text text-anchor="start" x="278.7764" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 16 long sepal &gt;=3</text>
</g>
<!-- 6 -->
<g id="node11" class="node">
<title>6</title>
<polygon fill="#ffffff" stroke="#000000" points="666.1655,-217.2 543.6465,-217.2 543.6465,-179.2 666.1655,-179.2 666.1655,-217.2"/>
<text text-anchor="start" x="550.7764" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:virginica</text>
<text text-anchor="start" x="550.7764" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">consisting of 21 short sepal &lt;3</text>
<text text-anchor="start" x="550.7764" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 29 long sepal &gt;=3</text>
</g>
<!-- 7 -->
<g id="node12" class="node">
<title>7</title>
<polygon fill="#cccccc" stroke="#000000" points="257.4843,-217.2 132.3277,-217.2 132.3277,-179.2 257.4843,-179.2 257.4843,-217.2"/>
<text text-anchor="start" x="139.1172" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:setosa</text>
<text text-anchor="start" x="139.1172" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">excluding 34 with narrow petals</text>
<text text-anchor="start" x="139.1172" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 1 outlier</text>
</g>
<!-- 8 -->
<g id="node13" class="node">
<title>8</title>
<polygon fill="#cccccc" stroke="#000000" points="529.5365,-217.2 408.2755,-217.2 408.2755,-179.2 529.5365,-179.2 529.5365,-217.2"/>
<text text-anchor="start" x="415.3408" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:versicolor</text>
<text text-anchor="start" x="415.3408" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">excluding 0 with narrow petals</text>
<text text-anchor="start" x="415.3408" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 1 outlier</text>
</g>
<!-- 9 -->
<g id="node14" class="node">
<title>9</title>
<polygon fill="#cccccc" stroke="#000000" points="801.5365,-217.2 680.2755,-217.2 680.2755,-179.2 801.5365,-179.2 801.5365,-217.2"/>
<text text-anchor="start" x="687.3408" y="-204" font-family="Helvetica,sans-Serif" font-weight="bold" font-size="8.00" fill="#000000">Species:virginica</text>
<text text-anchor="start" x="687.3408" y="-196" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">excluding 0 with narrow petals</text>
<text text-anchor="start" x="687.3408" y="-188" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">and 3 outlier</text>
</g>
<!-- 3 -->
<g id="node15" class="node">
<title>3</title>
<polygon fill="#ffffff" stroke="#000000" points="372.3069,-258.6033 293.5051,-258.6033 293.5051,-235.3967 372.3069,-235.3967 372.3069,-258.6033"/>
<text text-anchor="start" x="300.4556" y="-244.6" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">stratify by Species</text>
</g>
<!-- 2 -->
<g id="node16" class="node">
<title>2</title>
<polygon fill="#ffffff" stroke="#000000" points="357.0807,-300.2033 308.7313,-300.2033 308.7313,-276.9967 357.0807,-276.9967 357.0807,-300.2033"/>
<text text-anchor="start" x="315.5692" y="-286.2" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">150 items</text>
</g>
<!-- 1 -->
<g id="node17" class="node">
<title>1</title>
<polygon fill="#ffffff" stroke="#000000" points="357.0807,-341.8033 308.7313,-341.8033 308.7313,-318.5967 357.0807,-318.5967 357.0807,-341.8033"/>
<text text-anchor="start" x="315.5692" y="-327.8" font-family="Helvetica,sans-Serif" font-size="8.00" fill="#000000">150 items</text>
</g>
</g>
</svg>

And your publication ready data pipeline, with any assumptions you care
to document, is creates in a format of your choice (as long as that
choice is one of `pdf`, `png`, `svg` or `ps`), ready for submission to
Nature.

This is a trivial example, but the more complex the pipeline, the bigger
benefit you will get.

Check out the [main documentation for detailed
examples](https://terminological.github.io/dtrackr/)

## Developers

For automated testing `dtrackr` uses the `testthat` framework. For
vignette building there are dependencies on the `tidyverse` package, as
well as other system libraries required for vignette building with
`pandoc`.

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

pak::::local_system_requirements("ubuntu","20.04")
install.packages(c("here","tidyverse","devtools","testthat","pkgdown"))

# Examples:
devtools::run_examples()

# automated testing with testthat:
devtools::test()

# pkgdown site building (which runs all the vignettes:
pkgdown::build_site()
```
