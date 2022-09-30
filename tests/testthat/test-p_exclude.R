library(tidyverse)
library(dtrackr)
graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}


test_that("exclusions works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_exclude_all(c%%2==0 ~ "removed {.excluded} even values") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(g %>% graphContains("a:1","removed 1 even values"))
  testthat::expect_true(g %>% graphContains("a:2","removed 2 even values"))
  testthat::expect_true(nrow(h) == 3)
})

test_that("exclusions works groupwise", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_exclude_all(c==max(c) ~ "removed {.excluded} max values") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","removed 1 max values") &
      g %>% graphContains("a:2","removed 1 max values") &
      nrow(h) == 4
  )

})


test_that("exclusions works groupwise when nothing is excluded", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_exclude_all(c>=5 ~ "removed {.excluded} c values lt 5") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","removed 0 c values lt 5") &
      g %>% graphContains("a:2","removed 2 c values lt 5") &
      nrow(h) == 4
  )
})

test_that("exclusions can reference variable in function", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  fn = function(.df) {
    someVar = "xyz"
    h = .df %>% p_clear() %>% p_comment("test") %>% p_exclude_all(c==max(c) ~ "removed {.excluded} {someVar} values") %>% p_comment("test2")
    return(h)
  }
  k = fn(df)
  g = k %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","removed 1 xyz values") &
      g %>% graphContains("a:2","removed 1 xyz values") &
      nrow(k) == 4
  )

})

test_that("missing values exclusions works", {
  dfNa = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,NA,4,5,6)) %>% group_by(a)

  h = dfNa %>% p_clear() %>% p_comment("test") %>% p_exclude_all(c%%2==0 ~ "removed {.matched} even values and {.missing} missing", na.rm = TRUE) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","removed 1 even values and 1 missing") &
      g %>% graphContains("a:2","removed 2 even values and 0 missing") &
      nrow(h) == 2
  )
})
