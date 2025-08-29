library(dplyr)
library(dtrackr)

graphContains = function(g, .strata, .message) {
  return(
    g$nodes %>%
      filter(
        .strata == .strata,
        .label %>%
          stringr::str_detect(paste0("(^|>)", stringr::fixed(.message), "<"))
      ) %>%
      nrow() >
      0
  )
}

graphContainsExactly = function(g, .strata, .message, .count) {
  return(
    g$nodes %>%
      filter(
        .strata == .strata,
        .label %>%
          stringr::str_detect(paste0("(^|>)", stringr::fixed(.message), "<"))
      ) %>%
      nrow() ==
      .count
  )
}


test_that("group by works", {
  # expect_silent({
  iris %>%
    dtrackr::track() %>%
    group_by(Species) %>%
    summarise(
      across(ends_with("Width"), mean),
      .messages = "sepals: {Sepal.Width}; petals: {Petal.Width}"
    ) %>%
    dtrackr::flowchart()
  # })

  expect_error(
    {
      iris %>%
        dtrackr::track() %>%
        group_by(NotPresent) %>%
        summarise(across(ends_with("Width"), mean)) %>%
        dtrackr::flowchart()
    },
    regexp = "Must group by variables found in `.data`"
  )

  expect_message(
    {
      opt = options(dtrackr.verbose = TRUE)
      iris %>%
        dtrackr::track() %>%
        group_by(across(ends_with("Length"))) %>%
        summarise(across(ends_with("Width"), mean)) %>%
        dtrackr::comment("test") %>%
        dtrackr::flowchart()
      options(opt)
    },
    regexp = "This group_by\\(\\) has created more than the maximum number of supported groupings"
  )
})

test_that("group by not nested structure ok", {
  # Check graph grouping
  tmp = ggplot2::diamonds %>%
    dtrackr::track() %>%
    group_by(cut) %>%
    comment("test_comment_1")

  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "stratify by cut", .count = 1)
  })

  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "test_comment_1", .count = 5)
  })

  # Check graph grouping - second level not nested
  tmp2 = tmp %>%
    group_by(color) %>%
    comment("test_comment_2")

  expect_true({
    tmp2 %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "stratify by color", .count = 1)
  })

  expect_true({
    tmp2 %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "test_comment_2", .count = 7)
  })
})


test_that("group by nested structure ok", {
  tmp = iris %>%
    mutate(longer = Petal.Length > mean(Petal.Length)) %>%
    dtrackr::track() %>%
    group_by(Species) %>%
    comment("test_comment_1") %>%
    group_by(longer, .add = TRUE) %>%
    comment("test_comment_2")

  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "stratify by Species", .count = 1)
  })

  # this shoudl not appear due to the .add in the group by.
  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "stratify by longer", .count = 0)
  })

  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "stratify by Species, longer", .count = 3)
  })

  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "test_comment_1", .count = 3)
  })

  # Not all combinations are present hence this is 4 not 6.
  expect_true({
    tmp %>%
      dtrackr::p_get() %>%
      graphContainsExactly(.message = "test_comment_2", .count = 4)
  })
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

## Nesting ----

#
# library(tidyverse)
# library(dtrackr)
#
# iris_long = iris %>% mutate(id = row_number()) %>%
#   pivot_longer(cols = -c(Species,id)) %>%
#   group_by(id) %>%
#   mutate(total = sum(value)) %>%
#   ungroup() %>%
#   glimpse()
#
# # The counts here are not unique per item in long format
# iris_filtered = iris_long %>% track("Start") %>%
#   group_by(Species) %>%
#   comment("group wise processing") %>%
#   exclude_all(total > 15 ~ "{.excluded} items with combination > 10") %>%
#   ungroup()
#
# iris_filtered %>% flowchart()
#
# # This does what is expected. Nesting handles long format in a flexible way
# iris_nested = iris_long %>%
#   nest(data = c(name,value)) %>%
#   track("Start") %>%
#   group_by(Species) %>%
#   comment("group wise processing") %>%
#   exclude_all(total > 15 ~ "{.excluded} items with combination > 10") %>%
#   ungroup()
#
# iris_nested %>% flowchart()
