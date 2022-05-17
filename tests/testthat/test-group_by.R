test_that("group by works", {

  library(tidyverse)

  expect_silent({
  iris %>%
    dtrackr::track() %>%
    group_by(Species) %>%
    summarise(across(ends_with("Width"), mean )) %>%
    dtrackr::flowchart()
  })

  expect_error({
    iris %>%
      dtrackr::track() %>%
      group_by(NotPresent) %>%
      summarise(across(ends_with("Width"), mean )) %>%
      dtrackr::flowchart()
  },regexp = "Must group by variables found in `.data`")

  expect_error({
    iris %>%
      dtrackr::track() %>%
      group_by(across(ends_with("Height"))) %>%
      summarise(across(ends_with("Width"), mean )) %>%
      dtrackr::flowchart()
  },regexp = "dtrackr does not yet support grouping by things that are not column names.*")


})


test_that("filtering works", {

  library(tidyverse)

  expect_silent({
    iris %>%
      dtrackr::track() %>%
      dtrackr::filter(across(ends_with("Width"), ~ .x < 3 )) %>%
      dtrackr::flowchart()
  })


})
