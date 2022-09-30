library(tidyverse)
library(dtrackr)

test_that("group by works", {

  # expect_silent({
  iris %>%
    dtrackr::track() %>%
    group_by(Species) %>%
    summarise(across(ends_with("Width"), mean), .messages = "sepals: {Sepal.Width}; petals: {Petal.Width}") %>%
    dtrackr::flowchart()
  # })

  expect_error({
    iris %>%
      dtrackr::track() %>%
      group_by(NotPresent) %>%
      summarise(across(ends_with("Width"), mean )) %>%
      dtrackr::flowchart()
  },regexp = "Must group by variables found in `.data`")

  expect_message({
    iris %>%
      dtrackr::track() %>%
      group_by(across(ends_with("Length"))) %>%
      summarise(across(ends_with("Width"), mean )) %>%
      dtrackr::flowchart()
  },regexp = "This group_by\\(\\) has created more than the maximum number of supported groupings")


})


# test_that("filtering works", {
#
#   library(tidyverse)
#
#   expect_silent({
#     iris %>%
#       dtrackr::track() %>%
#       dtrackr::filter(across(ends_with("Width"), ~ .x < 3 )) %>%
#       dtrackr::flowchart()
#   })
#
#
# })
