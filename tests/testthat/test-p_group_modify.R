library(tidyverse)
graphContains = function(g, .strata, .message) {
  return(g$nodes %>% filter(.strata==.strata,.label %>% str_detect(paste0("(^|>)",fixed(.message),"<"))) %>% nrow() > 0)
}


test_that("default mutate is not recorded", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_group_modify(
    function(d,g,...) { tibble(e=c(4,8)*g$a,f=c(4,8)+g$a) },
    .messages="was {.count.in}, now {.count.out}"
  ) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","test") &
      g %>% graphContains("a:1","was 3, now 2")
  )

})

# df %>% p_clear() %>% p_modify(function(d) { d %>% filter(c==2) }, .message="was {.count.in}, now {.count.out}") %>% p_get()
# TODO: FAILS: df %>% p_clear() %>% p_modify(function(d) { d %>% filter(c==2) }, .headline="was {nrow(df)}") %>% p_get()
# df %>% p_clear() %>% p_modify(function(d) { d %>% filter(c==2) }) %>% p_get() # NULL

test_that("use of global expressions in headline", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)

  h = df %>% p_clear() %>% p_comment("test") %>% p_group_modify(
      function(d,g,...) { d %>% filter(c==2) },
      .messages="was {.count.in}, now {.count.out}",
      .headline="was {nrow(df)}"
  ) %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(
    g %>% graphContains("a:1","test") &
      g %>% graphContains("a:1","was 6")
  )

})



test_that("distinct works", {
  df = tibble(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3), c=c(1,2,3,4,5,6)) %>% group_by(a)
  df = bind_rows(df,df)

  h = df %>% p_clear() %>% p_comment("test") %>% p_distinct() %>% p_comment("test2")
  g = h %>% p_get()
  testthat::expect_true(g %>% graphContains("a:1","removing 3 duplicates"))
  testthat::expect_true(nrow(g$head) == 2)
  testthat::expect_true(groups(h) %>% sapply(as_label) %>% as.character() == groups(df) %>% sapply(as_label) %>% as.character())


})
