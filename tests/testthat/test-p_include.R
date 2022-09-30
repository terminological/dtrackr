library(tidyverse)
library(dtrackr)

graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}

test_that("inclusions works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_include_any(c%%2==0 ~ "including {.included} even values") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","including 1 even values") &
      g %>% graphContains("a:2","including 2 even values") &
      nrow(h) == 3
  )

})

test_that("inclusions works groupwise", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_include_any(c==max(c) ~ "kept {.included} max values") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","kept 1 max values") &
      g %>% graphContains("a:2","kept 1 max values") &
      nrow(h) == 2
  )

})


test_that("inclusions works groupwise when nothing is included", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_include_any(c>=5 ~ "kept {.included} c values gte 5") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","kept 0 c values gte 5") &
      g %>% graphContains("a:2","kept 2 c values gte 5") &
      nrow(h) == 2
  )
})

test_that("inclusions can reference variable in function", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  fn = function(.df) {
    someVar = "xyz"
    h = .df %>% p_clear() %>% p_comment("test") %>% p_include_any(c==max(c) ~ "kept {.included} {someVar} values") %>% p_comment("test2")
    return(h)
  }
  k = fn(df)
  g = k %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","kept 1 xyz values") &
      g %>% graphContains("a:2","kept 1 xyz values") &
      nrow(k) == 2
  )

})

test_that("missing values inclusions works", {
  dfNa = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,NA,4,5,6)) %>% group_by(a)

  h = dfNa %>% p_clear() %>% p_comment("test") %>% p_include_any(c%%2==0 ~ "kept {.matched} even values and {.missing} missing", na.rm = FALSE) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","kept 1 even values and 1 missing") &
      g %>% graphContains("a:2","kept 2 even values and 0 missing") &
      nrow(h) == 4
  )
})
