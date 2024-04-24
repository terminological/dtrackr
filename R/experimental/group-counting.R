#TODO:
# Configure a countable items named list of expressions
# or maybe better a formula like the exclusion spec
# Use a n_distinct call to generate a set of additional .count type columns
# maybe this is able to use status.

# iris %>% dplyr::group_by(Species) %>% summarise(n = n_distinct(Petal.Length+Sepal.Length))
# iris %>% dplyr::group_by(Species) %>% dplyr::summarise(n = dplyr::n_distinct(Petal.Length+Sepal.Length))
# iris %>% dplyr::group_by(Species) %>% dplyr::summarise(n = dplyr::n_distinct(Petal.Length+Sepal.Length, Sepal.Width+Petal.Width))
# x = function(...) {dots = rlang::enexprs(...); browser()}
# x(a = c(Petal.Length+Sepal.Length, Sepal.Wdith), b= c(asdasda) )
# class(dots$a)
# as.expression(dots$a)
# as.expression(unlist(dots$a))
# x(a = c(Petal.Length+Sepal.Length, Sepal.Wdith), b= c(asdasda) )
# x(a = c(Petal.Length+Sepal.Length, Sepal.Width), b= c(asdasda) )
# iris %>% dplyr::summarise(n = !!dots$a)
# iris %>% dplyr::summarise(n = dplyr::n_distinct(!!dots$a))
# iris %>% dplyr::summarise(n = dplyr::n_distinct(!!dots$b))
# iris %>% dplyr::summarise(n = dplyr::n_distinct(Petal.Length + Sepal.Length, Sepal.Width))
# iris %>% dplyr::summarise(n = dplyr::n_distinct(c(Petal.Length + Sepal.Length, Sepal.Width)))
