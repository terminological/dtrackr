library(tidyverse)
library(dtrackr)

# test_that("examples run to completion", {
#   devtools::run_examples()
#
# })

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_examples(path = "../..")
}
