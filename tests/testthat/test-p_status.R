library(tidyverse)
graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% stringr::str_detect(paste0("(^|>)",stringr::fixed(.message),"<"))) %>% nrow() > 0)
}

test_that("basic status works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("test2") %>% p_status(count=n(),.messages = "{count} items") %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","3 items")
  )

})

test_that("status on empty df does not crash", {
  dfempty = tibble(a=integer(), b=integer(), c=integer()) %>% group_by(a)

  g = dfempty %>% p_clear() %>% p_comment("test") %>% p_status() %>% p_get()
  testthat::expect_true(
    nrow(g$nodes)==0
  )
})

test_that("no args status works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("test2") %>% p_status() %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","3 items")
  )

})


test_that("more complex status works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  g = df %>% p_clear() %>% p_comment("test2") %>% p_status(count=n(),m=mean(b),z=max(c),.messages = c("{count} items","{m} mean b","{z} max c")) %>% p_comment("test2") %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","3 items") &
    g %>% graphContains("a:1","3 max c") &
    g %>% graphContains("a:1","2 mean b")
  )

})

test_that("subgroup counts work", {
  g = ILPD %>%
    p_comment() %>%
    p_group_by(Case_or_Control) %>%
    p_comment() %>%
    p_count_subgroup(
        .subgroup = Gender,
        .messages="{.name}: {.count}/{.subtotal}",
        .headline="{Case_or_Control}: {.subtotal}/{.total}"
    ) %>%
    p_comment() %>%
    p_get()
  testthat::expect_true(
    g %>% graphContains("case: 416/583","Female: 92/416") &
    g %>% graphContains("control: 167/583","Male: 117/167")
  )
})
