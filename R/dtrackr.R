## A set of functions to perform tidy manipulations on data and retain information about them for the purposes of documentation

## Low level functions ----

# function to process glue text in context of grouped input dataframe to produce a dataframe of messages.
.dataToNodesDf = function(.data, .glue, .isHeader, .type, .env) {
  .data = .data %>% .untrack()
  g = .data %>% dplyr::summarise(.count=dplyr::n(), .groups="keep") #dplyr::group_data() %>% dplyr::select(-.rows) %>% dplyr::group_by_all()
  return(.summaryToNodesDf(g, .glue, .isHeader, .type, .env))
}

# function to process glue text in context of a dplyr::summarised dataframe to produce a dataframe of messages.
.summaryToNodesDf = function(.summary, .glue, .isHeader, .type, .env) {
  .message = .strata = NULL
  grps = .summary %>% dplyr::groups()
  if (identical(.glue,NULL)) return(.summary %>% dplyr::select(!!!grps) %>% utils::head(0) %>% dplyr::mutate(.message = character(),.isHeader = logical(),.type = character()))
  g = .summary %>% .createStrataCol()
  g$.message = glue::glue_data(g,.glue,.envir=.env)
  g$.isHeader = .isHeader
  g$.type = .type
  return(g %>% dplyr::ungroup() %>% dplyr::select(!!!grps,.message,.strata,.isHeader,.type))
}

# .data might be a full data set or a dplyr::summarised data set but this takes the grouping and makes it into a single column with the names and values of the group
.createStrataCol = function(.data) {
  if (".strata" %in% colnames(.data)) return(.data)
  strataCols = .data %>% dplyr::group_vars()
  if (length(strataCols)==0) return(.data %>% dplyr::mutate(.strata = ""))
  .data = .data %>% tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(strataCols),.fns=function(x) paste0(dplyr::cur_column(),":",x), .names=".strata.{.col}")) %>%
    tidyr::unite(col = ".strata",dplyr::starts_with(".strata."),sep="; ")
  return(.data)
}

# assumes a properly constructed node df and puts it into the graph this function does not need to check .data versus .df. .df will have a .message and a .strata column
.writeMessagesToNode = function(.data, .df, .asOffshoot) {
  .message = .strata = .isHeader = .type = .id = .strata.prev = NULL
  # now we need to assemble the messages into a single formatted label per strata.
  # THIS IS WHERE THE LABEL FORMATTING OCCURS
  tmp = .df %>% dplyr::ungroup() %>% dplyr::select(.strata,.message,.isHeader,.type)
  tmp = tmp %>%
    dplyr::filter(!is.na(.message) & .message != "" & .message != "NA") #exclude empty or NA messages
  if(nrow(tmp)==0) return(.data)

  tmp = tmp %>%
    dplyr::mutate(.message = htmltools::htmlEscape(.message)) %>%
    dplyr::mutate(.message = ifelse(.isHeader,
                             paste0("<B>",.message,"</B><BR ALIGN='LEFT'/>"),
                             paste0(.message,"<BR ALIGN='LEFT'/>"))) %>%
    dplyr::group_by(.strata,.type) %>%
    dplyr::summarise(.label = paste0(.message, collapse = ""), .groups="drop") %>%
    dplyr::ungroup()

  # add the message to the .data@prov attribute which is a graph structure of nodes, edges and a head
  current = .data %>% p_get()

  currentRank = max(current$nodes$.rank,0)
  currentMaxId = max(current$nodes$.id,0)
  nodes=tmp %>% dplyr::mutate(.rank=currentRank+1, .id=dplyr::row_number()+currentMaxId)
  new = list()
  new$nodes = current$nodes %>% dplyr::bind_rows(nodes)
  #TODO: if the new node is terminal put in an invisible extra node for the branch point.
  newEdges = current$head %>%
    dplyr::left_join(
      nodes %>% dplyr::select(.to = .id, .rel = .type, .strata), by=character(),suffix=c(".prev","")
    ) %>%
    dplyr::filter(.strata == "" | .strata.prev == "" | .strata == .strata.prev | ifelse(.strata.prev == "",TRUE,.strata %>% stringr::str_starts(stringr::fixed(.strata.prev))) | ifelse(.strata == "",TRUE,.strata.prev %>% stringr::str_starts(stringr::fixed(.strata))))
  # somehow we would like to detect case where a .strata is missing from this newEdges set
  new$edges = current$edges %>% dplyr::bind_rows(newEdges)

  if (!.asOffshoot) {
    # TODO this is where we would need to try and identify .strata that do not have messages on this rank and
    # persist them into the new$head
    new$head = nodes %>% dplyr::select(.from = .id, .strata)

  } else {
    # this rank is a terminal part of the flowchart (i.e. an exclusion)
    new$head = current$head
  }
  .data = .data %>% p_set(new)

  return(.data)
}

# takes two data history graphs and merges them
.mergeGraphs = function(.graph1, .graph2) {
  .id = .from = .to = NULL
  # first remove overlapping nodes:
  .graph2$nodes = .graph2$nodes %>% dplyr::anti_join(.graph1$nodes, by=c(".id",".strata",".rank",".label",".type"))
  idsToChange = .graph2$nodes %>% dplyr::pull(.id)
  # exclude edges between the nodes we are removing (but not ones spanning gap)
  .graph2$edges = .graph2$edges %>% dplyr::filter(.from %in% idsToChange | .to %in% idsToChange)
  # now modify node ids
  graph1MaxId = max(.graph1$nodes$.id,0)
  idOffset = graph1MaxId+1-min(idsToChange)
  .graph2$nodes = .graph2$nodes %>% dplyr::mutate(.id = .id+idOffset)
  # change ids of edge ends, but only where they have changed
  .graph2$edges = .graph2$edges %>% dplyr::mutate(
    .from = ifelse(.from %in% idsToChange, .from+idOffset, .from),
    .to = ifelse(.to %in% idsToChange, .to+idOffset, .to)
  )
  # change ids of head nodes, but only where they have changed
  .graph2$head = .graph2$head %>% dplyr::mutate(
    .from = ifelse(.from %in% idsToChange, .from+idOffset, .from)
  )
  # ranks.
  # potentially complex. If two graphs are different streams of same process then we want to preserve rank
  # if two processes are totally separate trying to rank them together is potentially bad.
  # graph1Rank = max(graph1$nodes$.rank)
  # rankOffset = graph1Rank - max(graph2$nodes$.rank)
  # we can merge the data now.
  out = list(
    nodes = dplyr::bind_rows(.graph1$nodes,.graph2$nodes),
    edges = dplyr::bind_rows(.graph1$edges,.graph2$edges),
    head = dplyr::bind_rows(.graph1$head,.graph2$head)
  )
  return(out)
}

## Basic operations ----

.emptyGraph = function() {
  nodes=tibble::tibble(.id=integer(),.rank=integer(),.strata=character(),.label=character(),.type=character())
  edges=tibble::tibble(.to=integer(),.from=integer(),.rel=character(), .strata=character()) # empty edges
  head=tibble::tibble(.from=integer(), .strata=character())
  out = list(nodes=nodes,edges=edges,head=head)
}

.untrack = function(.data) {
  tmp = class(.data)
  tmp = tmp[tmp!="trackr_df"]
  class(.data) = tmp
  return(.data)
}

.retrack = function(.data) {
  tmp = unique(c("trackr_df",class(.data)))
  class(.data) = tmp
  return(.data)
}

.isTracked = function(.data) {
  return("trackr_df" %in% class(.data))
}

#' Strart tracking the dtrackr history graph
#'
#' @param .data - a dataframe which may be grouped
#'
#' @return the hisotry graph
#' @export
#'
#' @examples
#' iris %>% p_track()
p_track = function(.data) {
  if ("trackr_df" %in% class(.data)) return(.data)
  if (!"data.frame" %in% class(.data)) stop("dtrackr can only track data frames. Sorry.")
  return(.data %>% p_set((.data %>% p_get())))
}

#' Get the dtrackr history graph
#'
#' @param .data - a dataframe which may be grouped
#'
#' @return the history graph
#' @export
#'
#' @examples
#' iris %>% p_comment("A comment") %>% p_get()
p_get = function(.data) {
  out = attr(.data,"prov")
  if (identical(out,NULL)) out = .emptyGraph()
  return(out)
}

#' Set the dtrackr history graph
#'
#' @param .data - a dataframe which may be grouped
#' @param .graph - a history graph list (consisting of nodes, edges, and head) see examples
#'
#' @return the .data dataframe
#' @export
#'
#' @examples
#' mtcars %>% p_set(iris %>% p_comment("A comment") %>% p_get())
p_set = function(.data, .graph) {
  attr(.data,"prov") = .graph
  if (identical(.graph,NULL)) {
    .untrack(.data)
  } else {
    .retrack(.data)
  }
  return(.data)
}

#' Clear the dtrackr history graph
#'
#' @param .data - a dataframe which may be grouped
#'
#' @return the hisotry graph
#' @export
#'
#' @examples
#' mtcars %>% p_comment("A comment") %>% p_clear() %>% p_get()
p_clear = function(.data) {
  return(.data %>% p_set(NULL))
}

#' Copy the dtrackr history graph from one df to another
#'
#' @param .data - a dataframe which may be grouped
#' @param from - the dataframe to copy the history graph from
#'
#' @return the .data with the history graph of "from"
#' @export
#'
#' @examples
#' mtcars %>% p_copy(iris %>% p_comment("A comment"))
p_copy = function(.data, from) {
  return(.data %>% p_set(from %>% p_get()))
}

## User operations ----

# TODO:
# group_map - not included as does not return DF
# group_walk - not included as does not return DF
# count / tally - not included as summarisation step, not generally part of a dplyr pipeline

#' Simple count_if dplyr summary function
#'
#' @param ... - expression to be evaluated
#'
#' @return summary count
#' @export
#'
#' @examples iris %>% p_group_by(Species) %>% p_mutate(long_ones = p_count_if(Petal.Length > 4))
p_count_if = function(...) {
  return(sum(ifelse(...,1,0)))
}

#' Add a generic comment to the dtrackr history graph
#'
#' @param .data - a dataframe which may be grouped
#' @param .messages - a character vector of glue specifications. A glue specification can refer to any grouping variables of .data, or any variables defined in the calling environment, or the {.count} variable which is nrow(.data)
#' @param .headline - a glue specification which can refer to grouping variables of .data, or any variables defined in the calling environment, or the {.total} variable which is nrow(.data)
#' @param .type - one of "info","...,"exclusion": used to define formatting
#' @param .asOffshoot - do you want this comment to be an offshoot of the main flow (default = FALSE).
#'
#' @return the same .data dataframe with the history graph updated
#' @export
#'
#' @examples
#' iris %>% p_comment("hello {.total} rows")
p_comment = function(.data, .messages=NULL, .headline=NULL, .type="info", .asOffshoot = FALSE) {
  if (identical(.messages,NULL) & identical(.headline,NULL)) return(.data)
  .data = .data %>% .untrack()
  if (identical(.headline,NULL)) .headline = "{.strata}"
  default_env = rlang::caller_env()
  default_env$.total = nrow(.data)
  # .headline is a single glue spec
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .env=default_env)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = .type, .env=default_env)))
  .data = .data %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot)

  return(.data %>% .retrack())
}

# p_generate = function(.data, messageFn, type="info") {
#   out = .data %>% dplyr::group_modify(function(d,g,...) {
#     tibble::tibble(.message=messageFn(d))
#   })
#   return(p_comment(.data,out,type))
# }

# # e.g. df %>% p_clear() %>% p_glue(nrow,"has {.nrow} rows") %>% p_get()
# # TODO: make this into p_status_summary where messageFn is a ... named list and glue is a list.
# p_glue = function(.data, messageFn, .glue = paste0("{",fnName,"}"), type="info") {
#   default_env = rlang::caller_env()
#   fnName = paste0(".",as.character(substitute(messageFn)))
#   out = .data %>% dplyr::group_modify(function(d,g,...) {
#     tibble::tibble(!!fnName:=messageFn(d))
#   })
#   messages = glue::glue_data(.x=out, .glue)
#   return(p_comment(.data,messages,type))
# }



#' Add a summary to the dtrackr history graph
#'
#' Because of the ... summary specification parameters MUST BE NAMED.
#'
#' @param .data - a dataframe which may be grouped
#' @param ... - any normal dplyr::summarise specification, e.g. count=n() or av = mean(x) etc.
#' @param .messages - a character vector of glue specifications. A glue specification can refer to the summary outputs, any grouping variables of .data, the .strata, or any variables defined in the calling environment
#' @param .headline - a glue specification which can refer to grouping variables of .data, or any variables defined in the calling environment
#' @param .type - one of "info","exclusion": used to define formatting
#' @param .asOffshoot - do you want this comment to be an offshoot of the main flow (default = FALSE).
#'
#' @return the same .data dataframe with the history graph updated
#' @export
#'
#' @examples
#' iris %>% p_group_by(Species) %>%
#'    p_status(
#'       long = p_count_if(Petal.Length>5),
#'       short = p_count_if(Petal.Length<2),
#'       .messages="{Species}: {long} long ones & {short} short ones"
#'    ) %>% p_get()
p_status = function(.data, ..., .messages = NULL, .headline = NULL, .type="info", .asOffshoot = FALSE) {
  dots = rlang::enquos(...)
  .data = .data %>% .untrack()
  if(length(dots)==0) {
    dots = list(.count = rlang::expr(dplyr::n()))
    if(identical(.messages,NULL) & identical(.headline,NULL)) {
      .headline = "{.strata}"
      .messages = "{.count} items"
    }
  } else {
    if(identical(.messages,NULL) & identical(.headline,NULL)) {
      warning("p_status is missing both .messages and .headline specification. Did you forget to name them?")
      return(.data %>% .retrack())
    }
    if(identical(.headline,NULL)) .headline = "{.strata}"
  }

  default_env = rlang::caller_env()
  default_env$.total = nrow(.data)
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::summarise(!!!dots, .groups="keep") %>% dplyr::group_by(!!!grps)

  # .headline is a single glue spec
  tmpHead = .summaryToNodesDf(out,.headline,.isHeader=TRUE, .type = .type, .env=default_env)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(out,m,.isHeader=FALSE, .type = .type, .env=default_env)))
  .data = .writeMessagesToNode(.data, dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot)
  return(.data %>% .retrack())
}

#' Exclude all items matching a criteria
#'
#' Apply a set of filters and dplyr::summarise the actions of the filter to the dtrackr history graph
#' Because of the ... filter specification, all parameters MUST BE NAMED.
#' The filters work in an additive manner, i.e. the results excluding all things that match any of the criteria.
#' If na.rm = TRUE they also remove anything that cannot be evaluated by a criteria.
#'
#' @param .data - a dataframe which may be grouped
#' @param ... - a dplyr filter specification as a formula where the RHS is a glue specification, defining the message. This can refer to grouping variables
#' variables from the environment and {.excluded} and {.matched} or {.missing} (excluded = matched+missing), {.count} and {.total} - group and overall counts respectively, e.g. "excluding {.matched} items and {.missing} with missing values".
#' @param .headline - a glue specification which can refer to grouping variables of .data, or any variables defined in the calling environment
#' @param na.rm - (default FALSE) if the filter cannot be evaluated for a row count that row as missing and either exclude it (TRUE) or don't exclude it (FALSE)
#' @param .type - default "exclusion": used to define formatting
#' @param .asOffshoot - do you want this comment to be an offshoot of the main flow (default = TRUE).
#'
#' @return the filtered .data dataframe with the history graph updated
#' @export
#'
#' @examples
#' iris %>%
#'    p_exclude_all(
#'       Petal.Length > 5 ~ "{.excluded} long ones",
#'       Petal.Length < 2 ~ "{.excluded} short ones"
#'    ) %>%
#'    p_get()
p_exclude_all = function(.data, ..., .headline="{.strata}", na.rm=FALSE, .type="exclusion", .asOffshoot = TRUE) {
  .excl = .excl.na = .retain = NULL
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  filters = rlang::list2(...)
  if (length(filters)==0) {
    warning("No exclusions defined on p_exclude_all.")
    return(.data %>% .retrack())
  }
  default_env$.total = nrow(.data)
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type=.type, .env=default_env)
  messages = .dataToNodesDf(.data,.glue = "exclusions:",.isHeader=FALSE,.type=.type,.env = default_env)
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::mutate(.retain = TRUE)

  for(filter in filters) {
    glueSpec = rlang::f_rhs(filter)
    filt = rlang::f_lhs(filter)
    out = out %>% dplyr::group_modify(function(d,g,...) {
      d %>%
        dplyr::mutate(.excl = rlang::eval_tidy(filt,data = d, env=default_env)) %>%
        dplyr::mutate(.excl.na = ifelse(is.na(.excl),na.rm,.excl)) %>%
        dplyr::mutate(.retain = .retain & !.excl.na)
    })
    tmp = out %>%
      dplyr::summarise(
        .count = dplyr::n(),
        .excluded = p_count_if(.excl.na), #sum(ifelse(.excl.na,1,0)),
        .missing = p_count_if(is.na(.excl)&.excl.na), #sum(ifelse(is.na(.excl)&.excl.na,1,0)),
        .matched = p_count_if(!is.na(.excl)&.excl.na), #sum(ifelse(!is.na(.excl)&.excl.na,1,0))
        .groups="keep"
      ) %>%
      dplyr::ungroup() %>%
      tidyr::complete(tidyr::nesting(!!!grps),fill=list(.count=0,.missing=0)) %>%
      dplyr::group_by(!!!grps) %>% .createStrataCol()
    tmp$.message = rlang::eval_tidy(glue::glue_data(tmp,glueSpec,.envir = default_env), data=tmp, env = default_env)
    messages = messages %>% dplyr::bind_rows(tmp %>% dplyr::mutate(.isHeader=FALSE,.type=.type))
  }
  out = out %>% dplyr::filter(.retain) %>% dplyr::select(-.retain,-.excl, -.excl.na) %>% p_copy(.data) %>% .writeMessagesToNode(.df = dplyr::bind_rows(tmpHead,messages), .asOffshoot = .asOffshoot)
  return(out %>% .retrack())
}

#' Include any items matching a criteria
#'
#' Apply a set of inclusion criteria and dplyr::summarise the actions of the filter to the dtrackr history graph
#' Because of the ... filter specification, all parameters MUST BE NAMED.
#' The criteria work in an alternative manner, i.e. the results include anything that match any of the criteria.
#' If na.rm = TRUE they also keep anything that cannot be evaluated by a criteria - that may be true.
#'
#' @param .data - a dataframe which may be grouped
#' @param ... - a dplyr filter specification as a formula where the RHS is a glue specification, defining the message. This can refer to grouping variables,
#' variables from the environment and {.included} and {.matched} or {.missing} (included = matched+missing), {.count} and {.total} - group and overall counts respectively, e.g. "excluding {.matched} items and {.missing} with missing values".
#' @param .headline - a glue specification which can refer to grouping variables of .data, or any variables defined in the calling environment
#' @param na.rm - (default FALSE) if the filter cannot be evaluated for a row count that row as missing and either exclude it (TRUE) or don't exclude it (FALSE)
#' @param .type - default "exclusion": used to define formatting
#' @param .asOffshoot - do you want this comment to be an offshoot of the main flow (default = TRUE).
#'
#' @return the filtered .data dataframe with the history graph updated
#' @export
#'
#' @examples
#' iris %>%
#'    p_include_any(
#'       Petal.Length > 5 ~ "{.included} long ones",
#'       Petal.Length < 2 ~ "{.included} short ones"
#'    ) %>%
#'    p_get()
p_include_any = function(.data, ..., .headline="{.strata}", na.rm=TRUE, .type="inclusion", .asOffshoot = FALSE) {
  .incl = .incl.na = .retain = NULL
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  filters = rlang::list2(...)
  if (length(filters)==0) {
    warning("No inclusions defined on p_include_any.")
    return(.data %>% .retrack())
  }
  default_env$.total = nrow(.data)
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type=.type, .env=default_env)
  messages = .dataToNodesDf(.data,.glue = "inclusions:",.isHeader=FALSE,.type=.type,.env = default_env)
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::mutate(.retain = FALSE)
  for(filter in filters) {
    glueSpec = rlang::f_rhs(filter)
    filt = rlang::f_lhs(filter)
    out = out %>% dplyr::group_modify(function(d,g,...) {
      d %>%
        dplyr::mutate(.incl = rlang::eval_tidy(filt,data = d, env=default_env)) %>%
        dplyr::mutate(.incl.na = ifelse(is.na(.incl),!na.rm,.incl)) %>%
        dplyr::mutate(.retain = .retain | .incl.na)
    })
    tmp = out %>%
      dplyr::summarise(
        .count = dplyr::n(),
        .included = p_count_if(.incl.na), #sum(ifelse(.incl.na,1,0)),
        .missing = p_count_if(is.na(.incl)&.incl.na), #sum(ifelse(is.na(.incl)&.incl.na,1,0)),
        .matched = p_count_if(!is.na(.incl)&.incl.na), #sum(ifelse(!is.na(.incl)&.incl.na,1,0))
        .groups="keep"
      ) %>%
      dplyr::ungroup() %>%
      tidyr::complete(tidyr::nesting(!!!grps),fill=list(.count=0)) %>%
      dplyr::group_by(!!!grps) %>% .createStrataCol()
    tmp$.message = rlang::eval_tidy(glue::glue_data(tmp,glueSpec,.envir = default_env), data=tmp, env = default_env)
    messages = messages %>% dplyr::bind_rows(tmp %>% dplyr::mutate(.isHeader=FALSE,.type=.type))
  }
  out = out %>% dplyr::filter(.retain) %>% dplyr::select(-.retain,-.incl, -.incl.na) %>% p_copy(.data) %>% .writeMessagesToNode(.df = dplyr::bind_rows(tmpHead,messages), .asOffshoot = .asOffshoot)
  return(out %>% .retrack())
}

#' Remove a stratification from a data set
#'
#' Un-grouping a data set logically combines the different arms.
#' In the history this joins any stratified branches and acts as a specific type of p_summary, allowing you to
#' generate some summary statistics about the un-grouped data
#'
#' @param x - a dataframe which may be grouped (why not .data?)
#' @param ... a set of dplyr summary expressions. If left blank a default of ".count=n()" will be filled in
#' @param .messages - a set of glue specs. The glue code can use any summary variable defined in the ... parameter, or any global variable, or {.count}. the default is "total {.count} items"
#' @param .headline - a headline glue spec. The glue code can use any summary variable defined in the ... parameter, or {.count}.
#'
#' @return the .data but dplyr::ungrouped.
#' @export
#'
#' @examples
#' iris %>%
#'    p_group_by(Species) %>%
#'    p_comment("A test") %>%
#'    p_ungroup(
#'       avg = mean(Petal.Length),
#'       count = dplyr::n(),
#'       .messages="{count} items with {avg} petal length"
#'    ) %>%
#'    p_get()
p_ungroup = function(x, ..., .messages = "total {.count} items", .headline=NULL) {
  .data = x %>% .untrack()
  dots = dplyr::enexprs(...)
  if(length(dots)==0) dots = list(.count=rlang::expr(dplyr::n()))
  out = .data %>% dplyr::ungroup() %>% p_copy(.data) %>% p_status(!!!dots, .messages=.messages, .headline = .headline, .type="summarise")
  return(out %>% .retrack())
}

#' Summarise a data set
#'
#' Summarising a data set acts in the normal way. Any columns resulting form the summary can be added to the history graph
#' In the history this joins any stratified branches and acts as a specific type of p_summary, allowing you to
#' generate some summary statistics about the un-grouped data
#'
#' @param .data - a dataframe which may be grouped
#' @param ... a set of dplyr summary expressions.
#' @param .messages - a set of glue specs. The glue code can use any summary variable defined in the ... parameter, or any global variable, or {.strata}
#' @param .headline - a headline glue spec. The glue code can use any summary variable defined in the ... parameter, or any global variable, or {.strata}
#' @param .groups	- Experimental lifecycle Grouping structure of the result.
#'
#' @return the .data but ungrouped.
#' @export
#'
#' @examples
#' iris %>%
#'    p_group_by(Species) %>%
#'    p_summarise(avg = mean(Petal.Length), .messages="{avg} length") %>%
#'    p_get()
p_summarise = function(.data, ..., .groups=NULL, .messages = NULL, .headline="{.strata}") {
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::summarise(..., .groups=.groups)
  newGrps = out %>% dplyr::groups()
  out = out %>% dplyr::group_by(!!!grps)
  # .headline is a single glue spec
  tmpHead = .summaryToNodesDf(out,.headline,.isHeader=TRUE, .type = "summarise", .env=default_env)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(out,m,.isHeader=FALSE, .type = "summarise", .env=default_env)))
  out = out %>% dplyr::group_by(!!!newGrps) %>% p_copy(.data) %>% .writeMessagesToNode(.df=dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE)
  return(out %>% .retrack())
}

#' Standard dplyr modifying operations
#'
#' Equivalent Dplyr functions for mutating, selecting and renaming a data set act in the normal way.
#' mutates / selects / rename generally don't add anything in documentation so the default behaviour is to miss these out of the history.
#' This can be overridden with the .messages, or .headline values
#'
#' @param .data - a dataframe which may be grouped
#' @param ... a set of dplyr summary espressions.
#' @param .messages - a set of glue specs. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#' @param .headline - a headline glue spec. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#'
#' @return the .data but dplyr::ungrouped.
#' @export
#'
#' @examples
#' iris %>% p_group_by(Species) %>%
#'     p_mutate(Petal.Length = 2*Petal.Length, .messages="doubling length") %>%
#'     p_get()
p_mutate = function(.data, ..., .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::mutate(...)
  out = out %>% p_copy(.data) %>% p_comment(.messages=.messages, .headline = .headline, .type="mutate")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::add_count
p_add_count = function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::add_count(..., wt, sort, name)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="add_count")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::add_tally
p_add_tally = function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::add_tally(..., wt, sort, name)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="add_tally")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::transmute
p_transmute = function(.data, ..., .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::transmute(...)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="transmute")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::select
p_select = function(.data, ..., .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::select(...)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="select")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::relocate
p_relocate = function(.data, ..., .before = NULL, .after = NULL, .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::relocate(..., .before = .before, .after = .after)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="relocate")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::rename
p_rename = function(.data, ..., .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::rename(...)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="rename")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::rename_with
p_rename_with = function(.data, ..., .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::rename_with(...)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="rename_with")
  return(out %>% .retrack())
}

#' @inherit p_mutate
#' @inheritParams dplyr::arrange
p_arrange = function(.data, ...,  .by_group = FALSE, .messages = "", .headline = "") {
  .data = .data %>% .untrack()
  out = .data %>% dplyr::arrange(..., .by_group = .by_group)
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="arrange")
  return(out %>% .retrack())
}

#' Reshaping data using tidyr - pivot_wider
#'
#' A drop in replacement for tidyr::pivot_longer which optionally takes a message and headlien to store in the history graph
#'
#' @inheritParams  tidyr::pivot_wider
#' @param .messages - a set of glue specs. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#' @param .headline - a headline glue spec. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#'
#' @return the result of the pivot_wider but with a history graph.
#' @export
p_pivot_wider = function(data, id_cols = NULL, names_from = name, names_prefix = "",
                         names_sep = "_",names_glue = NULL,names_sort = FALSE,names_repair = "check_unique",
                         values_from = value,values_fill = NULL, values_fn = NULL, ..., .messages = "", .headline = "") {
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)
  .data = data %>% .untrack()
  out = .data %>% tidyr::pivot_wider(
    id_cols = {{id_cols}},
    # id_expand = FALSE,
    names_from = !!names_from,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_repair = names_repair,
    values_from = !!values_from,
    values_fill = values_fill,
    values_fn = values_fn,
    ...
  )
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="pivot_wider")
  return(out %>% .retrack())
}

#' Reshaping data using tidyr - pivot_longer
#'
#' A drop in replacement for tidyr::pivot_longer which optionally takes a message and headlien to store in the history graph
#'
#' @inheritParams  tidyr::pivot_longer
#' @param .messages - a set of glue specs. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#' @param .headline - a headline glue spec. The glue code can use any global variable, grouping variable, or {.strata}. Defaults to nothing.
#'
#' @return the result of the pivot_wider but with a history graph.
#' @export
p_pivot_longer = function(data,
                          cols,
                          names_to = "name",
                          names_prefix = NULL,
                          names_sep = NULL,
                          names_pattern = NULL,
                          names_ptypes = list(),
                          names_transform = list(),
                          names_repair = "check_unique",
                          values_to = "value",
                          values_drop_na = FALSE,
                          values_ptypes = list(),
                          values_transform = list(),
                          ..., .messages = "", .headline = "") {
  .data = data %>% .untrack()
  out = .data %>% tidyr::pivot_longer(
    cols = cols,
    names_to = names_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform,
    names_repair = names_repair,
    values_to = values_to,
    values_drop_na = values_drop_na,
    values_ptypes = values_ptypes,
    values_transform = values_transform,
    ...
  )
  out = out %>% p_copy(.data) %>% p_comment(.messages, .headline = .headline, .type="pivot_wider")
  return(out %>% .retrack())
}


#' Stratifying your analysis
#'
#' Grouping a data set acts in the normal way.
#'
#' @param .data - a dataframe which may be grouped
#' @param ... a set of dplyr column expressions.
#' @param .messages - a set of glue specs. The glue code can use any global variable, or {.cols} which is the columns that are being grouped by.
#' @param .headline - a headline glue spec. The glue code can use any global variable, or {.cols}.
#' @inheritParams dplyr::group_by
#'
#' @return the .data but grouped.
#' @export
#'
#' @examples
#' iris %>% p_group_by(Species, .messages="stratify by {.cols}") %>% p_comment("{.strata}") %>% p_get()
p_group_by = function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data), .messages = "stratify by {.cols}",  .headline=NULL) {
  if(!.add) .data = .data %>% ungroup()
  .data = .data %>% .untrack()
  col = dplyr::ensyms(...)
  .cols = col %>% sapply(rlang::as_label) %>% as.character() %>% paste(collapse=", ")
  tmp = p_comment(.data, .messages, .headline = .headline, .type="stratify")
  tmp2 = tmp %>% .untrack() %>% dplyr::group_by(!!!col, .add=.add, .drop=.drop) %>% p_copy(tmp)
  return(tmp2 %>% .retrack())
}


.beforeAfterGroupwiseCounts = function(.in,.out) {
  .count.in = .count.out = NULL
  grps = .in %>% dplyr::groups()
  countIn = .in %>% dplyr::group_by(!!!grps) %>% dplyr::summarise(.count.in=dplyr::n()) %>% dplyr::ungroup()
  countOut = .out %>% dplyr::group_by(!!!grps) %>% dplyr::summarise(.count.out = dplyr::n()) %>% dplyr::ungroup()

  tmp = countIn %>% dplyr::full_join(countOut, by=(grps %>% sapply(rlang::as_label) %>% as.character())) %>%
    dplyr::mutate(
      .count.out = ifelse(is.na(.count.out),0,.count.out),
      .count.in = ifelse(is.na(.count.in),0,.count.in)
    ) %>% dplyr::group_by(!!!grps)
  return(tmp)
}

#' Distinct values of data
#'
#' Distinct acts in the same way as DPLYR. Prior to the operation the size of the group is calculated {.count.in} and
#' after the operation the output size {.count.out}
#' The group {.strata} is also available (if grouped) for reporting
#'
#' @param .data - a dataframe which may be grouped
#' @param .f a function as expected by dplyr::group_modify e.g. function(d,g,...) { ...do something with d and return a dataframe... }
#' @param ... additional parameters for .f.
#' @param .keep - are the grouping variables kept in d, or split out to g (the default)
#' @param .messages - a set of glue specs. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#' @param .headline - a headline glue spec. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#'
#' @return the transformed .data.
#' @export
#'
#' @examples
#' p_bind_rows(iris, iris %>% p_filter(Petal.Length > 5)) %>%
#'    p_group_by(Species) %>% p_distinct() %>% p_get()
p_distinct = function(.data, .f, ..., .keep = FALSE, .messages="removing {.count.in-.count.out} duplicates", .headline="{.strata}") {
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  default_env$.total = nrow(.data)
  grps = .data %>% dplyr::groups()

  out = .data %>% dplyr::distinct()

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = "modify", .env=default_env)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = "modify", .env=default_env)))

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE)
  return(out %>% .retrack())
}


#' Filtering data
#'
#' Filter acts in the same way as DPLYR. Prior to the operation the size of the group is calculated {.count.in} and
#' after the operation the output size {.count.out}
#' The group {.strata} is also available (if grouped) for reporting
#'
#' @param .data - a dataframe which may be grouped
#' @param ... the filter criteria
#' @param .messages - a set of glue specs. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#' @param .headline - a headline glue spec. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#' @param .type - the format type of the action - typically an exclusion
#' @param .asOffshoot - if the type is exclusion, asOffshoot places the information box outside of the main flow, as an exclusion.
#' @inheritParams dplyr::filter
#'
#' @return the transformed .data.
#' @export
#'
#' @examples
#' iris %>% p_group_by(Species) %>% p_filter(Petal.Length > 5) %>% p_get()
p_filter = function(.data, ..., .preserve = FALSE, .messages="excluded {.count.in-.count.out} items", .headline="{.strata}", .type = "exclusion", .asOffshoot=(.type=="exclusion")) {
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  default_env$.total = nrow(.data)
  grps = .data %>% dplyr::groups()

  out = .data %>% dplyr::filter(..., .preserve = .preserve)

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .env=default_env)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .env=default_env)))

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot)
  return(out %>% .retrack())
}


#' Group-wise modification of data and complex operations
#'
#' Group modifying a data set acts in the normal way. The internal mechanics of the modify function are opaque to the history.
#' This means these can be used to wrap any unsupported operation.
#' Prior to the operation the size of the group is calculated {.count.in} and
#' after the operation the output size {.count.out}
#' The group {.strata} is also available (if grouped) for reporting
#'
#' @param .data - a dataframe which may be grouped
#' @param .f a function as expected by dplyr::group_modify e.g. function(d,g,...) { ...do something with d and return a dataframe... }
#' @param ... additional parameters for .f.
#' @param .keep - are the grouping variables kept in d, or split out to g (the default)
#' @param .messages - a set of glue specs. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#' @param .headline - a headline glue spec. The glue code can use any global variable, or {.strata},{.count.in},and {.count.out}
#' @param .type - default "modify": used to define formatting
#'
#' @return the transformed .data.
#' @export
#'
#' @examples
#' iris %>%
#'    p_group_by(Species) %>%
#'    p_group_modify(
#'       function(d,g,...) { return(tibble::tibble(x=runif(10))) },
#'       .messages="{.count.in} in, {.count.out} out"
#'    ) %>%
#'    p_get()
p_group_modify = function(.data, .f, ..., .keep = FALSE, .messages=NULL, .headline="{.strata}", .type = "modify") {
  .data = .data %>% .untrack()
  default_env = rlang::caller_env()
  default_env$.total = nrow(.data)

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .env=default_env)

  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::group_modify(.f, ..., .keep=.keep)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .env=default_env)))

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE)
  return(out %>% .retrack())
}

## Two DF operations ----

#' Union of two or more data sets
#'
#' This merges the history of 2 dataframes and binds the rows. It calculates the total number fo resulting rows as {.count.out}
#'
#' @param ... the data frames to bind
#' @param .messages - a set of glue specs. The glue code can use any global variable, or {.count.out}
#' @param .headline - a glue spec. The glue code can use any global variable, or {.count.out}
#' @inheritParams dplyr::bind_rows
#'
#' @return the union data frame
#' @export
#'
#' @examples
#' p_bind_rows( iris %>% p_comment("one"), iris %>% p_comment("two") ) %>% p_get()
p_bind_rows = function(..., .id = NULL, .messages="{.count.out} in union", .headline="Union") {
  dots = rlang::list2(...)
  if(!all(sapply(dots,.isTracked))) return(dplyr::bind_rows(..., .id=.id))
  #default_env = environment()
  mergedGraph=.emptyGraph()
  for(item in  dots) {
    mergedGraph = .mergeGraphs(mergedGraph, item %>% p_get())
  }
  dots = lapply(dots, .untrack)
  out = dplyr::bind_rows(..., .id=.id)
  .count.out = nrow(out)
  out = out %>% p_set(mergedGraph) %>% p_comment(.messages, .headline = .headline, .type="combine")
  return(out %>% .retrack())
  # tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .env=default_env)
  # tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = .type, .env=default_env)))
  # .data = .writeMessagesToNode(.data, dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot)
}

.doJoin = function(joinFunction, x, y, by, copy, suffix, ..., .messages, .headline) {
  x = x %>% .untrack()
  y = y %>% .untrack()
  #default_env = environment()
  .keys = paste(by,sep=", ")
  .count.lhs = nrow(x)
  .count.rhs = nrow(y)
  mergedGraph = .mergeGraphs(x %>% p_get(), y %>% p_get())
  out = joinFunction(x, y, by=by, copy=copy, suffix=suffix, ...)
  .count.out = nrow(out)
  out = out %>% p_set(mergedGraph) %>% p_comment(.messages, .headline = .headline, .type="combine")
  return(out %>% .retrack())
  # tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .env=default_env)
  # tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = .type, .env=default_env)))
  # .data = .writeMessagesToNode(.data, dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot)
}

#' Inner joins
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::inner_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_inner_join = function(x, y, by = NULL, copy=FALSE,  suffix=c(".x", ".y"), ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Inner join by {.keys}") {
  .doJoin(dplyr::inner_join, x=x, y=y, by=by, copy=copy, suffix=suffix, ..., .messages = .messages, .headline = .headline)
}

#' Left join
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::left_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_left_join = function(x, y, by = NULL, copy=FALSE, suffix=c(".x", ".y"), ... , keep = FALSE, .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Left join by {.keys}") {
  .doJoin(dplyr::left_join, x=x, y=y, by=by, copy=copy, suffix=suffix, ..., keep = keep, .messages = .messages, .headline = .headline)
}

#' Right join
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::right_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_right_join = function(x, y,  by = NULL, copy=FALSE, suffix=c(".x", ".y"), ..., keep = FALSE, .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Right join by {.keys}") {
  .doJoin(dplyr::right_join, x=x, y=y, by=by, copy=copy,suffix=suffix, ..., keep = keep, .messages = .messages, .headline = .headline)
}

#' Full join
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::full_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_full_join = function(x, y,  by = NULL, copy=FALSE, suffix=c(".x", ".y"), ..., keep = FALSE, .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Full join by {.keys}") {
  .doJoin(dplyr::full_join, x=x, y=y, by=by, copy=copy, suffix=suffix, ..., keep = keep, .messages = .messages, .headline = .headline)
}

#' Semi join
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::semi_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_semi_join = function(x, y,  by = NULL, copy=FALSE, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in intersection"), .headline="Semi join by {.keys}") {
  .doJoin(dplyr::semi_join, x=x, y=y, by=by, copy=copy, ..., .messages = .messages, .headline = .headline)
}

#' Anti join
#'
#' Mutating joins behave as dplyr joins
#'
#' @inheritParams dplyr::anti_join
#' @param .messages - a set of glue specs. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#' @param .headline - a glue spec. The glue code can use any global variable, {.keys} for the joining columns, {.count.lhs}, {.count.rhs}, {.count.out} for the input and output dataframes sizes respectively
#'
#' @export
p_anti_join = function(x, y,  by = NULL, copy=FALSE,  ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} not matched"), .headline="Semi join by {.keys}") {
  .doJoin(dplyr::anti_join, x=x, y=y, by=by, copy=copy, ..., .messages = .messages, .headline = .headline)
}

## Output operations ====

is_running_in_chunk = function() {
   isTRUE(try(rstudioapi::getActiveDocumentContext()$id != "#console"))
}

#' Flowchart output
#'
#' Create a flowchart of the history of the dataframe
#'
#' @param .data - the tracked dataframes
#' @param ... - other params passed onto p_get_as_dot, notable ones are fill, fontsize, colour, size, maxWidth and maxHeight
#' @param filename - a filename (without extension) which will be where the formatted flowcharts are saved
#' @inheritParams save_dot
#'
#' @return the output depends on the context in which the function is called. Some form of browseable output if in an interactive session or a PNG/PDG link if in knitr
#' @export
p_flowchart = function(.data, filename = NULL, size = std_size$half, maxWidth = size$width, maxHeight = size$height, rot=size$rot, formats=c("dot","png","pdf","svg"),...) {

  if("trackr_df" %in% class(.data)) .data = list(.data)
  mergedGraph=.emptyGraph()
  for(item in .data) {
    if ("trackr_df" %in% class(item)) {
      mergedGraph = .mergeGraphs(mergedGraph, item %>% p_get())
    } else {
      warning("Unsupported item in .data")
    }
  }

  outgraph = mergedGraph %>% .graph2dot(...)

  if (!identical(filename,NULL)) {
    filename = filename %>% stringr::str_remove("\\..*$")
    return(outgraph %>% save_dot(filename = filename, size=size,maxWidth=maxWidth, maxHeight=maxHeight,rot=rot,formats = formats))

  } else {
    if( isTRUE(getOption("knitr.in.progress")) ) {

      #fmt <- rmarkdown::default_output_format(knitr::current_input())$name
      if (knitr::is_html_output()) { #|| fmt %>% stringr::str_detect("html") || fmt=="article") {
        # message("html output for dtrackr")
        return(htmltools::HTML(dot2svg(outgraph)))
      } else {

        filename = tempfile()
        # message("saving to ",filename)
        return(outgraph %>% save_dot(filename= filename, size=size,maxWidth=maxWidth, maxHeight=maxHeight,rot=rot,formats = c("png","pdf")))
      }

    } else {

      if(is_running_in_chunk()) {
        return(htmltools::HTML(dot2svg(outgraph)))
      } else {
        return(htmltools::HTML(dot2svg(outgraph)) %>% htmltools::html_print())
      }
    }
  }
}

#' DOT output
#'
#' Outputs a dtrackr history graph as a DOT string for rendering with graphviz
#'
#' @param .data - the tracked dataframe
#' @param fill - the default node fill colour
#' @param fontsize - the default font size
#' @param colour - the default font colour
#' @param ... - not used
#'
#' @return a dot string
#' @export
p_get_as_dot = function(.data, fill="lightgrey", fontsize="8", colour="black", ...) {
  graph = .data %>% p_get()
  return(.graph2dot(graph))
}

.graph2dot = function(graph, fill="lightgrey", fontsize="8", colour="black", ...) {
  .rel = .id = .rank = nodeSpec = rankSpec = edgeSpec = NULL
  # THIS IS WHERE FORMATTING IS DEFINED
  nodesDf = graph$nodes %>% dplyr::mutate(
    .fillcolor= dplyr::case_when(.type=="summary"~"grey90",.type=="exclusion"~"grey80",TRUE~"white")
  )
  edgesDf = graph$edges %>% dplyr::mutate(
    #.headport="n", #ifelse(rel=="exclusion","w","n"),
    .weight=ifelse(.rel=="exclusion","1","100"),
    .tailport="s",
    .colour="black",
    .id = dplyr::row_number()
  )

  outNode = nodesDf %>% dplyr::group_by(dplyr::desc(.id)) %>% dplyr::mutate(
    nodeSpec = glue::glue("'{.id}' [label=<{.label}>,group='{.strata}',fillcolor='{.fillcolor}'];")
  ) %>%
    dplyr::group_by(.rank) %>%
    dplyr::summarise(rankSpec = paste0(nodeSpec,collapse = "\n"), .groups="drop") %>%
    dplyr::mutate(rankSpec = paste(
      "{ rank='same';",
      rankSpec,
      "}",sep="\n")
    ) %>%
    dplyr::arrange(dplyr::desc(.rank)) %>%
    dplyr::pull(rankSpec) %>%
    paste0(collapse="\n")

  outEdge = edgesDf %>%
    dplyr::arrange(dplyr::desc(.id)) %>%
    #dplyr::mutate(edgeSpec = glue::glue("'{.from}' -> '{.to}' [tailport='{.tailport}',headport='{.headport}',weight='{.weight}']")) %>%
    dplyr::mutate(edgeSpec = glue::glue("'{.from}' -> '{.to}' [tailport='{.tailport}',weight='{.weight}']")) %>%
    dplyr::pull(edgeSpec) %>%
    paste0(collapse="\n")

  outGraph = paste(
    "digraph {
     graph [layout = 'dot',
        splines='ortho',
        rankdir = 'TB',
        outputorder = 'edgesfirst',
        bgcolor = 'white',
        ranksep = '0.25',
        nodesep = '0.2',
        newrank='true']

    node [fontname = 'Helvetica',
        fontsize = '8',
        shape='box',
        fixedsize = 'false',
        margin = '0.1,0.1',
        width = '0',
        height = '0',
        style = 'filled',
        color = 'black',
        fontcolor = 'black',
        labeljust='l']

    edge [fontname = 'Helvetica',
        fontsize = '8',
        len = '0.5',
        color = 'black',
        arrowsize = '0.5']
    ",outNode,"\n",outEdge,sep="\n","}")

  return(outGraph)
}


## Aliases ----

#' @inherit p_track
#' @export
track <- p_track

#' @inherit p_exclude_all
#' @export
exclude_all <- p_exclude_all

#' @inherit p_include_any
#' @export
include_any <- p_include_any

#' @inherit p_flowchart
#' @export
flowchart <- p_flowchart

#' @inherit p_status
#' @export
status <- p_status

#' @inherit p_comment
#' @export
comment <- p_comment

## Dplyr bindings ----

#' @inherit p_ungroup
#' @export
#' @importFrom dplyr ungroup
ungroup.trackr_df <- p_ungroup

#' @inherit p_summarise
#' @export
#' @importFrom dplyr summarise
summarise.trackr_df <- p_summarise

#' @inherit p_mutate
#' @export
#' @importFrom dplyr mutate
mutate.trackr_df <- p_mutate

#' @inherit p_transmute
#' @export
#' @importFrom dplyr transmute
transmute.trackr_df <- p_transmute

#' @inherit p_select
#' @export
#' @importFrom dplyr select
select.trackr_df <- p_select

#' @inherit p_relocate
#' @export
#' @importFrom dplyr relocate
relocate.trackr_df <- p_relocate

#' @inherit p_rename
#' @export
#' @importFrom dplyr rename
rename.trackr_df <- p_rename

#' @inherit p_rename_with
#' @export
#' @importFrom dplyr rename_with
rename_with.trackr_df <- p_rename_with

#' @inherit p_arrange
#' @export
#' @importFrom dplyr arrange
arrange.trackr_df <- p_arrange

#' @inherit p_pivot_wider
#' @export
#' @importFrom tidyr pivot_wider
pivot_wider.trackr_df <- p_pivot_wider

#' @inherit p_pivot_longer
#' @export
#' @importFrom tidyr pivot_longer
pivot_longer.trackr_df <- p_pivot_longer

#' @inherit p_group_by
#' @export
#' @importFrom dplyr group_by
group_by.trackr_df <- p_group_by

#' @inherit p_distinct
#' @export
#' @importFrom dplyr distinct
distinct.trackr_df <- p_distinct

#' @inherit p_filter
#' @export
#' @importFrom dplyr filter
filter.trackr_df <- p_filter

#' @inherit p_group_modify
#' @export
#' @importFrom dplyr group_modify
group_modify.trackr_df <- p_group_modify

#' @inherit p_inner_join
#' @export
#' @importFrom dplyr inner_join
inner_join.trackr_df <- p_inner_join

#' @inherit p_left_join
#' @export
#' @importFrom dplyr left_join
left_join.trackr_df <- p_left_join

#' @inherit p_right_join
#' @export
#' @importFrom dplyr right_join
right_join.trackr_df <- p_right_join

#' @inherit p_full_join
#' @export
#' @importFrom dplyr full_join
full_join.trackr_df <- p_full_join

#' @inherit p_semi_join
#' @export
#' @importFrom dplyr semi_join
semi_join.trackr_df <- p_semi_join

#' @inherit p_anti_join
#' @export
#' @importFrom dplyr anti_join
anti_join.trackr_df <- p_anti_join

## Rexport dplyr generics

#' @export
dplyr::ungroup

#' @export
dplyr::summarise

#' @export
dplyr::mutate

#' @export
dplyr::transmute

#' @export
dplyr::select

#' @export
dplyr::relocate

#' @export
dplyr::rename

#' @export
dplyr::rename_with

#' @export
dplyr::arrange

#' @export
tidyr::pivot_wider

#' @export
tidyr::pivot_longer

#' @export
dplyr::group_by

#' @export
dplyr::distinct

#' @export
dplyr::filter

#' @export
dplyr::group_modify

#' @export
dplyr::inner_join

#' @export
dplyr::left_join

#' @export
dplyr::right_join

#' @export
dplyr::full_join

#' @export
dplyr::semi_join

#' @export
dplyr::anti_join

# complete override:

#' @inherit p_bind_rows
#' @export
bind_rows <- p_bind_rows

#' @inherit p_add_count
#' @export
add_count <- p_add_count

#' @inherit p_add_tally
#' @export
add_tally <- p_add_tally
