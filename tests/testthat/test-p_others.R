library(tidyverse)
graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}

test_that("ungrouping works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_ungroup(.messages="{.count} items") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("","6 items") &
      g %>% graphContains("","test2") &
      nrow(h) == 6
  )

})


test_that("summarise works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_summarise(mean_c=mean(c), count=n(), .messages=c("{mean_c} average c","{count} items")) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","3 items") &
      g %>% graphContains("","test2") &
      nrow(h) == 2
  )

})

test_that("default mutate is not recorded", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_mutate(x="hello") %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","test") &
      g %>% graphContains("a:1","test2") &
      nrow(g$nodes) == 4
  )

})


test_that("filter works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_filter(b!=3) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","excluded 1 items") &
      g %>% graphContains("","test2") &
      nrow(h) == 4
  )

})


# df %>% p_clear() %>% p_status(c%%2==0 ~ "consisting of {count} even items",c%%2!=0 ~ "and {count} odd items") %>% p_ungroup() %>% p_get()
