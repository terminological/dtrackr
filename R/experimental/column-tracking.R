# Prooof of concept / experimental

# TODO:
# https://github.com/ironcamel/Graph-Easy

# If your attributes that are vectorised over columns, implement
# dplyr_col_modify(), 1d [, and ⁠names<-⁠ methods. All of these methods know
# which columns are being modified, so you can update the column attribute
# according . You'll also need to think carefully about how to recompute the
# attribute in dplyr_reconstruct(), and you will need to carefully verify the
# behaviour of each verb, and provide additional methods as needed.


# tmp = iris %>% .mutate_wrapper(newval = Petal.Width+Sepal.Width, newval2 = newval/Sepal.Width)
# iris %>% .mutate_wrapper(across(-Species, ~ .x+1))
#
.mutate_wrapper = function(df, ...) {
  dots = .interceptor(...)
  dplyr::mutate(df, !!!dots)
}

# iris %>% group_by(Species) %>% .summarise_wrapper(count = n(), mean = mean(Petal.Width), sd = sd(Petal.Width))
# iris %>% .summarise_wrapper(quantile(Petal.Width))
.summarise_wrapper = function(df, ...) {
  dots = .interceptor(...)
  dplyr::summarise(df, !!!dots)
}

# intercept and wrap function calls in the ... parameters
.interceptor = function(...) {
  dots = dplyr::enexprs(...)
  if (length(dots)==1 && names(dots)=="") {
    # a tidy select or something else we can;t handle
    # we could in theory override all the tidyselect functions
    # message("Can't track tidyselects")
    return(dots)
  }
  for (col in names(dots)) {
    ex = dots[[col]]
    # replace expression with expression wrapped in a .describe() call
    dots[[col]] = substitute(.describe(ex))
  }
  return(dots)
}


# Captures code and history of dependencies of code into a history graph
# tmp = iris %>% mutate(newval = .describe(Petal.Width+Sepal.Width))
# tmp = tmp %>% mutate(newval2 = .describe(newval/Sepal.Width))
.describe = function(expr) {
  code = deparse(substitute(expr))
  vars = all.vars(substitute(expr))
  env = rlang::caller_env()
  res = eval(expr,env)
  var_history = lapply(vars, .get_dependency_history, env)

  # var_history is a named list of a list of dataframes
  history_nodes = lapply(var_history, function(x) x$nodes) %>%
    bind_rows() %>% distinct()
  history_edges = lapply(var_history, function(x) x$edges) %>%
    bind_rows() %>% distinct()
  history_heads = lapply(var_history, function(x) x$head) %>% unlist()

  # id of this is the code and the input.
  md5 = digest::digest(list(code,history_heads %>% sort()))
  # or the result.
  # md5 = digest::digest(res)
  # is it possible to introduce cycles?

  nodes = bind_rows(history_nodes,
                    tibble(md5=md5, code = code, type="calc")
  )
  edges = bind_rows(history_edges,
                    tibble(from = unname(history_heads), to = md5, name = vars)
  )

  return(structure(
    res,
    history = as.col_history(nodes,edges,md5)
  ))
}

print.col_history = function(x,...) {
  .print_level(hash=x$head, name="this", col_history = x, indent="", first =TRUE)
}

# recursive function for nested tree printing
.print_level = function(hash, name, col_history, indent, first) {
  last_code = col_history$nodes %>% filter(md5==hash) %>% pull(code) %>% paste0(collapse = paste0("\n",indent,"|    "))
  # cat(indent,"\u22a2 ",name,"=",last_code,"\n",sep = "")
  if (first) {
    cat(name,": ",last_code,"\n",sep = "")
  } else {
    cat(indent,"\u251C ",name,": ",last_code,"\n",sep = "")
  }

  depends_on = col_history$edges %>% filter(to==hash) %>% select(name,from)
  depends_on %>% filter(!is.na(from)) %>% rowwise() %>% group_walk(
    function(d,g,..) {
      new_indent = if (first) "  " else paste0(indent,"|  ")
      if (nrow(d) ==1) .print_level(d$from, d$name, col_history, new_indent, first=FALSE)
    }
  )
}

as.col_history = function(nodes,edges,head) {
  return(structure(
    list(nodes = nodes, edges = edges, head=head),
    class="col_history"
  ))
}

.new_history = function(x, code, type) {
  md5 = digest::digest(x)
  as.col_history(
    nodes = tibble(md5=md5, code=code, type=type),
    edges = tibble(from=character(), to=character(), name=character()),
    head = md5
  )
}

.has_history = function(col) {
  origin = attributes(col)[["history"]]
  return(is.null(origin))
}

get_history = function(col, code=deparse(substitute(col)), type="unknown") {
  origin = attributes(col)[["history"]]
  if (is.null(origin)) {
    return(.new_history(col, code, type))
  } else (
    return(origin)
  )
}

.get_dependency_history = function(var, env) {
  if (exists(var,where = .data, envir = env)) {
    # found it as a data variable
    get_history(env$.data[[var]], code="(data)", type="local")
  } else {
    # some sort of global variable maybe
    tmp = tryCatch({
      get_history( get(var,envir = env), code="(global)", type="global" )
    }, error = function(e) {
      .new_history(var, code="(unknown)", type="unknown")
    })
    return(tmp)
  }
}
