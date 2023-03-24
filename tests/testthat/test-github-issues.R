library(tidyverse)
library(dtrackr)

graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}


test_that("Issue #25 fixed", {
  tibble(id = 1:20, x = rnorm(20)) %>% track() %>% inner_join(
    tibble(id = 1:20, y = runif(20)) %>% track()
  ) %>%
    p_get() %>%
    graphContains("", "Inner join by id") %>%
    testthat::expect_true()
  # pre fixing this would output "Inner join by " without `id`
})


test_that("Issue #26 fixed", {
  expected = iris %>% track() %>% group_by(Species) %>% filter(Species == "setosa") %>% untrack()
  # pre fixing this throws error.
  actual = try(iris %>% track() %>% group_by(Species) %>% include_any(Species == "setosa" ~ "{.included}") %>% untrack())
  actual2 = try(iris %>% track() %>% group_by(Species) %>% exclude_all(Species != "setosa" ~ "{.excluded}") %>% untrack())
  testthat::expect_equal(actual2,expected)
  testthat::expect_equal(actual,expected)
})


test_that("Issue #33 fixed", {
  # distinct was using the wrong function signature and not passing it on properly
  correct = mtcars %>% dplyr::distinct(carb) %>% dim()
  was_wrong = mtcars %>% dtrackr::track() %>% dtrackr::p_distinct(carb) %>% dim()
  testthat::expect_equal(correct, was_wrong)
})
