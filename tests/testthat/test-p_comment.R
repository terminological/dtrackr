library(tidyverse)
library(dtrackr)

graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}


test_that("commenting works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("test") %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","test") &
    g %>% graphContains("a:1","test2")
  )
})

test_that("commenting on empty df does not crash", {
  dfempty = tibble(a=integer(), b=integer(), c=integer()) %>% group_by(a)

  g = dfempty %>% p_clear() %>% p_comment("test") %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    nrow(g$nodes)==0
  )
})

test_that("can suppress headline", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("test",.headline = "") %>% p_get()
  testthat::expect_true(nrow(g$nodes) == 2)
  testthat::expect_true(!(g %>% graphContains("a:1","a:1")))

})

test_that("empty comments removed", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("",.headline = "") %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    nrow(g$nodes) == 2 & g %>% graphContains("a:1","test2")
  )

})

test_that("messages can reference groups removed", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment(.messages=c("value {a}","{.strata} strata")) %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","value 1"),
    g %>% graphContains("a:1","a:1 strata")
  )

})


test_that("messages can reference things from current envionment", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)
  randomVar = "sadas"
  g = df %>% p_clear() %>% p_comment(.messages="{randomVar}") %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1",randomVar),
    g %>% graphContains("a:1","test2")
  )

})


test_that("messages can reference things from calling envionment", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)
  fn = function(.df) {
    randomVar = "sadas2"
    .df %>% p_clear() %>% p_comment(.messages="{randomVar}") %>% p_comment("test2") %>% p_get()
  }
  g = fn(df)
  testthat::expect_true(
    g %>% graphContains("a:1","sadas2"),
    g %>% graphContains("a:1","test2")
  )
})



test_that("graph offshoot works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)
  g = df %>% p_clear() %>% p_comment("test") %>% p_comment("test2",.asOffshoot = TRUE) %>% p_comment("test3") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","test") &
    g %>% graphContains("a:1","test2")
  )
  testthat::expect_true(!any(c(3,4) %in% g$edges$.from))
})

test_that("node typing works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)
  g = df %>% p_clear() %>% p_comment("test",.type="one") %>% p_comment("test2",.asOffshoot = TRUE,.type="two") %>% p_comment("test3",.type="three") %>% p_get()
  testthat::expect_true(
    all(c("one","two","three") %in% g$nodes$.type)
  )

})
