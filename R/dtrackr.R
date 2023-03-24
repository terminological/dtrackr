## A set of functions to perform tidy manipulations on data and retain information about them for the purposes of documentation

## Low level functions ----

# determine whether a continuous variable is in fact discrete and can be treated
# as such
.is.discrete = function(.data, .cutoff=10) {
  .data <- stats::na.omit(.data)
  return(length(unique(.data)) <= .cutoff | is.factor(.data))
}

# function to process glue text in context of grouped input dataframe to produce a dataframe of messages.
.dataToNodesDf = function(.data, .glue, .isHeader, .type, .envir) {
  .data = .data %>% .untrack()
  g = .data %>% dplyr::summarise(.count=dplyr::n(), .groups="keep") #dplyr::group_data() %>% dplyr::select(-.rows) %>% dplyr::group_by_all()
  return(.summaryToNodesDf(g, .glue, .isHeader, .type, .envir))
}

# summarise the dataframe and create a stratificiation
.taggedData = function(.data, ...) {
  .data = .data %>% .untrack()
  total = nrow(.data %>% dplyr::ungroup())
  .data %>% dplyr::summarise(..., .count=dplyr::n(), .groups="keep") %>% mutate(.total = total) %>% .createStrataCol()
}

# write  arbitrary content into the tags list. If the content is not specified then it is calculated from .data as a summarisation step by .taggedData().
.writeTag = function(.data, .tag, ..., .content = .taggedData(.data, ...)) {
  if (is.null(.tag)) return(.data)
  tmp = tibble::tibble(.tag = .tag, .content = list(.content))
  current = .data %>% p_get()
  current$tags = bind_rows(current$tags,tmp)

  .data %>% p_set(current)
}

.integrateDots = function(fn, ..., .envir) {
  tmp = rlang::enexprs(...)
  tmp = tmp[names(tmp) != ""]

  tmp2 = formals(fn)
  tmp2 = tmp2[names(tmp2) != ""]

  env2 = list2env( c(tmp,tmp2[!sapply(tmp2, is.symbol)]), .envir)
  return(env2)
}

# given a summary of the current state of the dataframe in the pipeline doGlue
# executes the glue function and returns the glue output as a vector. This
# checks for a few commonly occuring situations such as incorrect length resulting
# from the glue spec acting in a vectorised way.
.doGlue = function(g,.glue,.envir) {
  tmp = tryCatch(
    glue::glue_data(g,.glue,.envir=.envir),
    error = function(e) {
      # pframe = parent.frame()
      glueEnv = .envir # pframe$parentenv
      definedVars = setdiff(ls(glueEnv,all.names = TRUE), c("g",utils::lsf.str(glueEnv,all.names = TRUE)))
      definedVars = c(paste0(colnames(g),"*"),definedVars)
      definedVars = definedVars[!(definedVars %in% c(".glue",".envir",".Generic", ".GenericCallEnv", ".GenericDefEnv", ".Group",".Class","...",".Method",".headline", ".messages"))]
      stop("Error: ",e$message,", variables available for use in .messages are: ", paste0(definedVars, collapse = ", "))
    })
  if (length(tmp) != nrow(g)) {
    if (length(tmp)==0) stop("The message specification, (",.glue,") gave no results.")
    if (length(tmp)==1) return(rep(tmp,nrow(g)))
    glue_int = (.glue %>% stringr::str_match("^\\{(.*)\\}$"))[,2]
    if (nrow(g) == 1) {
      tmp = paste0(tmp,collapse=", ")
      message("The message specification, ",.glue," gave the wrong number of results which have been concatenated.")
      message("Probably you wanted `{paste0( <something> , collapse = ', ')}` or similar.")
    } else {

      stop(
        "The message specification in (",.glue,") must give a single result per group (this gave ",length(tmp)," items for ",nrow(g)," groups).\n",
        "Probably you wanted to `{paste0( <something> , collapse = ', ')}` or similar."
      )
    }
  }
  return(tmp)
}

# function to process glue text in context of a dplyr::summarised dataframe to produce a dataframe of messages.
.summaryToNodesDf = function(.summary, .glue, .isHeader, .type, .envir) {
  .message = .strata = NULL
  grps = .summary %>% dplyr::groups()
  if (identical(.glue,NULL)) return(.summary %>% dplyr::select(!!!grps) %>% utils::head(0) %>% dplyr::mutate(.message = character(),.isHeader = logical(),.type = character()))
  g = .summary %>% .createStrataCol()
  # catch errors and report environment contents at this stage.
  g$.message = .doGlue(g,.glue,.envir)
  g$.isHeader = .isHeader
  g$.type = .type
  return(g %>% dplyr::ungroup() %>% dplyr::select(!!!grps,.message,.strata,.isHeader,.type))
}

# .data might be a full data set or a dplyr::summarised data set but this takes the grouping and makes it into a single column with the names and values of the group
# .data = tibble(a=c(1,1,2,2),b=c(2,2,1,1),c=c("A","B","C","D")) %>% group_by(a,b)
.createStrataCol = function(.data) {
  .glue = .defaultStrataGlue()
  .sep = .defaultStrataSep()
  if (".strata" %in% colnames(.data)) return(.data)
  strataCols = .data %>% dplyr::group_vars()
  if (length(strataCols)==0) return(.data %>% dplyr::mutate(.strata = ""))
  .data = .data %>% tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(strataCols),.fns=function(x) glue::glue(.glue,.group=dplyr::cur_column(),.value=x), .names=".strata.{.col}")) %>%
    tidyr::unite(col = ".strata",dplyr::starts_with(".strata."),sep=.sep)
  return(.data)
}

# assumes a properly constructed node df and puts it into the graph this function does not need to check .data versus .df. .df will have a .message and a .strata column
# .stage lets us give a stage a name: TODO, expose this in the API.
.writeMessagesToNode = function(.data, .df, .asOffshoot, .excluded = NULL, .stage="") {

  if (.isPaused(.data)) return(.data)
  # this must be to prevent some kind of scoping issue
  .message = .strata = .isHeader = .type = .id = .strata.prev = NULL
  stage=.stage
  # now we need to assemble the messages into a single formatted label per strata.
  # THIS IS WHERE THE LABEL FORMATTING OCCURS
  tmp = .df %>% dplyr::ungroup() %>% dplyr::select(.strata,.message,.isHeader,.type)
  tmp = tmp %>%
    dplyr::filter(!is.na(.message) & .message != "" & .message != "NA") #exclude empty or NA messages
  if(nrow(tmp)==0) return(.data)

  tmp = tmp %>%
    dplyr::mutate(.message = htmltools::htmlEscape(.message) %>% stringr::str_replace_all("\n","<BR ALIGN='LEFT'/>")) %>%
    dplyr::mutate(.message = ifelse(.isHeader,
                             paste0("<B>",.message,"</B><BR ALIGN='LEFT'/>"),
                             paste0(.message,"<BR ALIGN='LEFT'/>"))) %>%
    dplyr::group_by(.strata,.type) %>%
    dplyr::summarise(.label = paste0(.message, collapse = ""), .groups="drop") %>%
    dplyr::mutate(.stage = stage) %>%
    dplyr::ungroup()

  # add the message to the .data@prov attribute which is a graph structure of nodes, edges and a head
  current = .data %>% p_get()

  currentRank = max(current$nodes$.rank,0)
  currentMaxId = max(current$nodes$.id,0)
  if(.asOffshoot) {
    nodes=tmp %>% dplyr::mutate(.rank=currentRank, .id=dplyr::row_number()+currentMaxId)
  } else {
    nodes=tmp %>% dplyr::mutate(.rank=currentRank+1, .id=dplyr::row_number()+currentMaxId)
  }
  new = list(
    excluded = current$excluded,
    capture = current$capture,
    tags = current$tags
  )
  new$nodes = current$nodes %>% dplyr::bind_rows(nodes)
  #TODO: investigate .rel here. I think it should be more to do with .asOffshoot than defined by .type
  newEdges = current$head %>%
    dplyr::left_join(
      nodes %>% dplyr::select(.to = .id, .rel = .type, .strata), by=character(),suffix=c(".prev","")
    ) %>%
    dplyr::filter(.strata == "" | .strata.prev == "" | .strata == .strata.prev | ifelse(.strata.prev == "",TRUE,.strata %>% stringr::str_starts(stringr::fixed(.strata.prev))) | ifelse(.strata == "",TRUE,.strata.prev %>% stringr::str_starts(stringr::fixed(.strata))))
  # somehow we would like to detect case where a .strata is missing from this newEdges set
  new$edges = current$edges %>% dplyr::bind_rows(newEdges)

  leading = nodes %>% dplyr::select(.from = .id, .strata)

  if(!is.null(.excluded)) {
    towrite = .excluded %>% p_clear() %>% .untrack() %>% dplyr::left_join(leading, by=".strata") #columns will be .from, .strata, .message, .filter, .data
    new$excluded = dplyr::bind_rows(current$excluded,towrite)
  }

  if (!.asOffshoot) {
    # TODO this is where we would need to try and identify .strata that do not have messages on this rank and
    # persist them into the new$head
    new$head = leading

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
  # these might happen if the pipeline started the same and then split, and is now being rejoined.
  .graph2$nodes = .graph2$nodes %>% dplyr::anti_join(.graph1$nodes, by=c(".id",".strata",".rank",".label",".type"))
  idsToChange = .graph2$nodes %>% dplyr::pull(.id)
  if (length(idsToChange) > 0) {

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
    # change id of any exclusions
    if(!is.null(.graph2$excluded)) {
      .graph2$excluded = .graph2$excluded %>% dplyr::mutate(
        .from = ifelse(.from %in% idsToChange, .from+idOffset, .from)
      )
    }

  }
  # ranks.
  # potentially complex. If two graphs are different streams of same process then we want to preserve rank
  # if two processes are totally separate trying to rank them together is potentially bad.
  # graph1Rank = max(graph1$nodes$.rank)
  # rankOffset = graph1Rank - max(graph2$nodes$.rank)
  # we can merge the data now.
  out = list(
    nodes = dplyr::bind_rows(.graph1$nodes,.graph2$nodes),
    edges = dplyr::bind_rows(.graph1$edges,.graph2$edges),
    head = dplyr::bind_rows(.graph1$head,.graph2$head),
    capture = isTRUE(.graph1$capture) | isTRUE(.graph2$capture),
    tags = dplyr::bind_rows(.graph1$tags,.graph2$tags)
  )
  # merge exclusions if any exist
  if(!is.null(.graph1$excluded) | !is.null(.graph2$excluded)) {
    out$excluded = dplyr::bind_rows(.graph1$excluded,.graph2$excluded)

  }
  return(out)
}

#' Print a history graph to the console
#'
#' @param x a dtrackr history graph (e.g. output from [p_get()])
#' @param ... not used
#'
#' @return nothing
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% comment("hello {.total} rows") %>% history() %>% print()
print.trackr_graph = function(x, ...) {
  .label = .strata = NULL
  graph = x
  ranks = length(unique(graph$nodes$.rank))
  tags = paste0(unique(graph$tags$.tag),collapse="; ")
  last = graph$head %>%
    dplyr::inner_join(graph$nodes, by=c(".from"=".id", ".strata")) %>%
    dplyr::mutate(tmp = stringr::str_remove_all(.label, "<BR[^>]+>$")) %>%
    dplyr::mutate(tmp = stringr::str_replace_all(tmp, "<BR[^>]+>", "\", \"")) %>%
    dplyr::mutate(tmp = stringr::str_remove_all(tmp, "<[^>]+>")) %>%
    dplyr::mutate(tmp = ifelse(.strata=="",sprintf("\"%s\"",tmp),sprintf("[%s]: \"%s\"",.strata,tmp))) %>%
    dplyr::pull(tmp)
  if (length(last)>0) {
    for (i in 1:length(last)) {
      last[[i]] = if(i==length(last)) sprintf("\u2514 %s",last[[i]]) else sprintf("\u251C %s",last[[i]])
    }
  } else {
    last[[1]] = "\u2514 <empty history>"
  }
  if (tags=="") tags="<none>"
  if (is.null(graph$excluded)) {
    excluded = "<not capturing exclusions>"
  } else {
    excluded = sum(sapply(graph$excluded$.excluded, nrow ), na.rm = TRUE)
  }

  tmp = c(
    "dtrackr history:",
    glue::glue("number of flowchart steps: {ranks} (approx)"),
    glue::glue("tags defined: {tags}"),
    glue::glue("items excluded so far: {excluded}"),
    "last entry / entries:",
    last
  )
  if (isTRUE(graph$paused)) {
   tmp = c(tmp,"TRACKING IS PAUSED")
  }
  cat(tmp,sep = "\n")
}

#' Plots a history graph as html
#'
#' @param x a dtrackr history graph (e.g. output from [history()])
#' @param fill the default node fill colour
#' @param fontsize the default font size
#' @param colour the default font colour
#' @param ... not used
#'
#' @return HTML displayed
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% comment("hello {.total} rows") %>% history() %>% plot()
plot.trackr_graph = function(x, fill="lightgrey", fontsize="8", colour="black", ...) {
  graph = x
  outgraph = .graph2dot(graph, fill, fontsize, colour, ...)
  htmltools::HTML(dot2svg(outgraph)) %>% htmltools::html_print()
}

## Basic operations ----

.emptyGraph = function(exclusions = FALSE,...) {
  nodes=tibble::tibble(.id=integer(),.rank=integer(),.strata=character(),.label=character(),.type=character())
  edges=tibble::tibble(.to=integer(),.from=integer(),.rel=character(), .strata=character()) # empty edges
  head=tibble::tibble(.from=integer(), .strata=character())
  tmp = list(
      nodes=nodes,
      edges=edges,
      head=head,
      capture=FALSE,
      paused=FALSE,
      tags=tibble::tibble(.tag=character(), .content=list())
      # excluded is set in p_capture_exclusions and will not be present all the time
  )
  class(tmp) = unique(c("trackr_graph",class(tmp)))
  return(tmp)
}

# there is a big difference between this function which just removes the class
# label and p_untrack which also deletes the history graph.
.untrack = function(.data) {
  if (is.null(.data)) return(.data)
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

.isPaused = function(.data, auto=FALSE) {
  tmp = p_get(.data)[["paused"]]
  if (is.null(tmp)) return(FALSE) # not paused
  if (is.logical(tmp)) return(tmp) # support legacy
  if (auto) {
    if (tmp == "auto") return(TRUE)
    else return(FALSE)
  }
  return(TRUE)
}

.trackingExclusions = function(.data) {
  old = .data %>% p_get()
  return(
    isTRUE(old$capture)
  )
}

.anyExclusionsTracked = function(.data) {
  old = .data %>% p_get()
  return(
    !is.null(old$excluded)
  )
}

.isExclusionsEmpty  = function(.data) {
  return(
    nrow((.data %>% p_get())$exclusions)==0
  )
}

.isHistoryEmpty = function(.data) {
  return(
    nrow((.data %>% p_get())$nodes)==0
  )
}

## Defaults ----

.defaultExclusions = function() {
  getOption("dtrackr.exclusions",default = FALSE)
}

.defaultMessage = function() {
  getOption("dtrackr.default_message",default = "{.count} items")
}

.defaultCountSubgroup = function() {
  getOption("dtrackr.default_count_subgroup",default = "{.name}: {.count} items")
}

.defaultHeadline = function() {
  getOption("dtrackr.default_headline",default = "{.strata}")
}

.defaultStrataGlue = function() {
  getOption("dtrackr.strata_glue", default = "{.group}:{.value}")
}

.defaultStrataSep = function() {
  getOption("dtrackr.strata_sep",default = "; ")
}

.defaultShowZeroExclusions = function() {
  getOption("dtrackr.show_zero_exclusions",default = TRUE)
}

.defaultMaxSupportedGroupings = function() {
  getOption("dtrackr.max_supported_groupings",default = 16)
}

## Tracking ----

#' Start tracking the dtrackr history graph
#'
#' @param .data a dataframe which may be grouped
#' @param .messages a character vector of glue specifications. A glue
#'   specification can refer to any grouping variables of .data, or any
#'   variables defined in the calling environment, the \{.total\} variable which
#'   is the count of all rows, the \{.count\} variable which is the count of
#'   rows in the current group and the \{.strata\} which describes the current
#'   group. Defaults to the value of `getOption("dtrackr.default_message")`.
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, or any variables defined in the calling environment, or the
#'   \{.total\} variable which is `nrow(.data)`, or \{.strata\} a summary of the
#'   current group. Defaults to the value of `getOption("dtrackr.default_headline")`.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the .data dataframe with additional history graph metadata, to allow
#'   tracking.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% track() %>% history()
p_track = function(.data, .messages=.defaultMessage(), .headline=.defaultHeadline(), .tag=NULL) {
  if (.isTracked(.data)) return(.data)
  if (!"data.frame" %in% class(.data)) stop("dtrackr can only track data frames. Sorry.")
  old = .data %>% p_get()
  .data = .data %>% p_set(old)
  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)
  # .headline is a single glue spec
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = "info", .envir=.envir)
  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = "info", .envir=.envir)))
  .data = .data %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE) %>% .writeTag(.tag = .tag)
  .data = .retrack(.data)
  if(.defaultExclusions()) {
    rlang::inform("dtrackr is capturing exclusions [getOption('dtrackr.exclusions') is TRUE]",.frequency = "always")
    .data = .data %>% p_capture_exclusions()
  }
  return(.data)
}

#' Remove tracking from the dataframe
#'
#' @param .data a tracked dataframe
#'
#' @return the .data dataframe with history graph metadata removed.
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% track() %>% untrack() %>% class()
p_untrack = function(.data) {
  .data = .data %>% p_clear()
  .data = .data %>% .untrack()
  return(.data)
}

#' Pause tracking the data frame.
#'
#' Pausing tracking of a data frame may be required if an operation is about to
#' be performed that creates a lot of groupings or that you otherwise don't
#' want to pollute the history graph (e.g. maybe selecting something using
#' an anti-join). Once paused the history is not updated until a `resume()` is
#' called, or when the data frame is ungrouped (if `auto` is enabled).
#'
#' @param .data a tracked dataframe
#' @param auto if `TRUE` the tracking will resume automatically when the
#' number of groups has fallen to a sensible level (default is `FALSE`)?
#'
#' @return the .data dataframe with history graph tracking paused
#' @export
#' @examples
#' iris %>% track() %>% pause() %>% history()
p_pause = function(.data, auto=FALSE) {
  old = .data %>% p_get()
  old$paused = if(auto) "auto" else "manual"
  .data = .data %>% p_set(old)
  return(.data)
}

#' Resume tracking the data frame.
#'
#' This may reset the grouping of the tracked data if the grouping structure
#' has changed since the data frame was paused. If you try and resume tracking a
#' data frame with too many groups (as defined by `options("dtrackr.max_supported_groupings"=XX)`)
#' then the resume will fail and the data frame will still be paused. This can
#' be overridden by specifying a value for the `.maxgroups` parameter.
#'
#' @param .data a tracked dataframe
#' @inheritDotParams p_group_by
#'
#' @return the .data data frame with history graph tracking resumed
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% track() %>% pause() %>% resume() %>% history()
p_resume = function(.data, ...) {
  .strata = NULL
  old = .data %>% p_get()

  # check that head is going to line up with old grouping.
  tmp = .createStrataCol(.data) %>% dplyr::pull(.strata) %>% unique()
  if (!all(tmp %in% old$head$.strata)) {
    grps = .data %>% dplyr::groups()
    .data = .data %>% p_ungroup() %>% p_group_by(!!!grps, ...)
    old = .data %>% p_get()
  }

  old$paused = NULL
  .data = .data %>% p_set(old)
  return(.data)
}

#' Start capturing exclusions on a tracked dataframe.
#'
#' @param .data a tracked dataframe
#' @param .capture Should we capture exclusions (things removed from the data
#'   set). This is useful for debugging data issues but comes at a significant
#'   cost. Defaults to the value of `getOption("dtrackr.exclusions")` or
#'   `FALSE`.
#'
#' @return the .data dataframe with the exclusions flag set (or cleared if
#'   `.capture=FALSE`).
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' tmp = iris %>% track() %>% capture_exclusions()
#' tmp %>% filter(Species!="versicolor") %>% history()
p_capture_exclusions = function(.data, .capture=TRUE) {
  if (!.isTracked(.data)) stop("dtrackr can only capture exclusions in tracked data frames. Did you forget to do a data %>% dtrackr::track()")
  old = .data %>% p_get() # N.B. this allow the exclusions flag to be set in the tracker graph. It can never be reset though
  old$capture = .capture
  # if there is no exclusions table in the tracking graph initialise it now.
  if (.capture & is.null(old$excluded)) {
    old$excluded = tibble::tibble(.strata=character(), .message = character(), .excluded=list(), .filter=character(), .from = integer())
  }
  .data = .data %>% p_set(old)
  return(.data)
}

#' Get the dtrackr history graph
#'
#' This provides the raw history graph and is not really intended for mainstream use.
#' The internal structure of the graph is explained below. print and plot S3 methods exist for
#' the dtrackr history graph.
#'
#' @param .data a dataframe which may be grouped
#'
#' @return the history graph. This is a list, of class `trackr_graph`, containing the following named items:
#' * excluded - the data items that have been excluded thus far as a nested dataframe
#' * tags - a dataframe of tag-value pairs containing the summary of the data at named points in the data flow (see [tagged()])
#' * nodes - a dataframe of the nodes of the flow chart
#' * edges - an edge list (as a dataframe) of the relationships between the nodes in the flow chart
#' * head - the current most recent nodes added into the graph as a dataframe.
#'
#' The format of this data may grow over time but these fields are unlikely to be changed.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' graph = iris %>% track() %>% comment("A comment") %>% history()
#' print(graph)
p_get = function(.data) {
  out = attr(.data,"prov")
  if (identical(out,NULL)) out = .emptyGraph()
  # make sure has the correct class
  class(out) = unique(c("trackr_graph",class(out)))
  return(out)
}

#' Get the dtrackr excluded data record
#'
#' @param .data a dataframe which may be grouped
#' @param simplify return a single summary dataframe of all exclusions.
#'
#' @return a new dataframe of the excluded data up to this point in the workflow. This dataframe is by default flattened, but if `.simplify=FALSE` has a nested structure containing records excluded at each part of the pipeline.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' tmp = iris %>% track() %>% capture_exclusions()
#' tmp %>% exclude_all(
#'    Petal.Length > 5.8 ~ "{.excluded} long ones",
#'    Petal.Length < 1.3 ~ "{.excluded} short ones",
#'    .stage = "petal length exclusion"
#' ) %>% excluded()
p_excluded = function(.data, simplify = TRUE) {
  .id = .rank = .stage = .from = .strata = .message = .excluded = .filter = NULL
  if (!.anyExclusionsTracked(.data)) stop("Exclusions were not tracked for this data frame. Did you forget to call '.data %>% capture_exclusions()'?")
  out = .data %>% p_get()
  tmp = out$excluded %>%
      dplyr::left_join(out$nodes %>% select(.id,.rank,.stage), by=c(".from"=".id")) %>%
      dplyr::mutate(.rank = dplyr::dense_rank(.rank)) %>%
      dplyr::mutate(.stage = ifelse(.stage=="",paste0("stage ",.rank), .stage)) %>%
      dplyr::select(-.from) %>%
      dplyr::select(.stage, .strata, .message, .excluded, .filter, .rank)
  if (simplify) {
    return(
      tmp %>%
        dplyr::mutate(.excluded = purrr::map(.excluded, ~ dplyr::mutate(.x, dplyr::across(.cols = dplyr::everything(), .fns = as.character)))) %>%
        tidyr::unnest(.excluded) %>%
        dplyr::select(-.rank)
    )
  } else {
    return(tmp)
  }
}

#' Retrieve tagged data in the history graph
#'
#' Any counts at the individual stages that was stored with a `.tag` option in a pipeline step can be recovered here. The idea here is to provide a quick way to access a single value
#' for the counts or other details tagged in a pipeline into a format that can be reported in text of a document. (e.g. for a results section). For more examples the consort statement vignette
#' has some examples of use.
#'
#' @param .data the tracked dataframe.
#' @param .tag (optional) the tag to retrieve.
#' @param .strata (optional) filter the tagged data by the strata. set to "" to filter just the top level ungrouped data.
#' @param .glue (optional) a glue specification which will be applied to the tagged content to generate a `.label` for the tagged content.
#' @param ... (optional) any other named parameters will be passed to `glue::glue` and can be used to generate a label.
#'
#' @return various things depending on what is requested.
#'
#' By default a tibble with a `.tag` column and all associated summary values in a nested `.content` column.
#'
#' If a `.strata` column is specified the results are filtered to just those that match a given `.strata` grouping (i.e. this will be the grouping label on the flowchart). Ungrouped content will have an empty "" as `.strata`
#'
#' If `.tag` is specified the result will be for a single tag and `.content` will be automatically un-nested to give a single un-nested dataframe of the content captured at the `.tag` tagged step.
#' This could be single or multiple rows depending on whether the original data was grouped at the point of tagging.
#'
#' If both the `.tag` and `.glue` is specified a `.label` column will be computed from `.glue` and the tagged content. If the result of this is a single row then just the string value of `.label` is returned.
#'
#' If just the `.glue` is specified, an un-nested dataframe with `.tag`,`.strata` and `.label` columns with a label for each tag in each strata.
#'
#' If this seems complex then the best thing is to experiment until you get the output you want, leaving any `.glue` options until you think you know what you are doing. It made sense at the time.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' tmp = iris %>% track() %>% comment(.tag = "step1")
#' tmp = tmp %>% filter(Species!="versicolor") %>% group_by(Species)
#' tmp %>% comment(.tag="step2") %>% tagged(.glue = "{.count}/{.total}")
p_tagged = function(.data, .tag=NULL, .strata=NULL, .glue=NULL, ...) {
  .content = .label = .content_rows = NULL
  out = .data %>% p_get()
  tag=.tag
  strata = .strata
  tmp = out$tags
  if (!is.null(.tag)) {
    # just getting one tag's values
    tmp = tmp %>% dplyr::filter(.tag == tag) %>% tidyr::unnest(.content)
    if (!is.null(.strata)) tmp = tmp %>% dplyr::filter(.strata == strata)
    if (!is.null(.glue)) {
      # calculate a .label column
      tmp = tmp %>% dplyr::mutate(.label = glue::glue(.glue,...))
      if (nrow(tmp) == 1) return(tmp %>% dplyr::pull(.label))
    }
    return(tmp)
  } else {
    # we are getting all tags values
    if (!is.null(.strata)) {
      # filter strata on all tags and exclude any that result in no content
      tmp = tmp %>% dplyr::mutate(
        .content = purrr::map(.content, ~ .x %>% filter(.strata == strata)),
        .content_rows = purrr::map(.content, ~ nrow(.x))
      )
      tmp = tmp %>% dplyr::filter(.content_rows>0) %>% dplyr::select(-.content_rows)
    }
    # apply glue spec to all remaining content.
    if (!is.null(.glue)) {
      # calculate the glue, and pull out the label and group strata and unnest it.
      tmp$.content = lapply(tmp$.content, function(.x) .x %>% dplyr::mutate(.label = glue::glue(.glue,...)) %>% dplyr::ungroup() %>% dplyr::select(.strata,.label) )
      tmp = tmp %>%
        # dplyr::mutate(
        # .content = purrr::map(.content, ~ .x %>%
        #                         dplyr::mutate(.label = glue::glue(.glue,...)) %>%
        #                         dplyr::ungroup() %>%
        #                         dplyr::select(.strata, .label)
        #                       )) %>%
        tidyr::unnest(.content)
      # if (nrow(tmp) == 1) return(tmp %>% pull(.label))
      return(tmp)
    }
    if (nrow(tmp)==1) {
      tmp = tmp %>% tidyr::unnest(.content)
    }
    return(tmp)
  }
}

#' Set the dtrackr history graph
#'
#' This is unlikely to be useful to an end user and is called automatically by many of the other
#' functions here. On the off chance you need to copy history metadata from one dataframe to another
#'
#' @param .data a dataframe which may be grouped
#' @param .graph a history graph list (consisting of nodes, edges, and head) see examples
#'
#' @return the .data dataframe with the history graph metadata set to the provided value
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' mtcars %>% p_set(iris %>% comment("A comment") %>% p_get()) %>% history()
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
#' This is unlikely to be needed directly and is mostly and internal function
#'
#' @param .data a dataframe which may be grouped
#'
#' @return the .data dataframe with the history graph removed
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' mtcars %>% track() %>% comment("A comment") %>% p_clear() %>% history()
p_clear = function(.data) {
  return(.data %>% p_set(NULL))
}

#' Copy the dtrackr history graph from one dataframe to another
#'
#' @param .data a dataframe which may be grouped
#' @param from the dataframe to copy the history graph from
#'
#' @return the .data dataframe with the history graph of "from"
#' @export
#'
#' @examples


#' mtcars %>% p_copy(iris %>% comment("A comment")) %>% history()
p_copy = function(.data, from) {
  return(.data %>% p_set(from %>% p_get()))
}

## User operations ----


#' Simple count_if dplyr summary function
#'
#' @param ... expression to be evaluated
#' @param na.rm ignore NA values?
#'
#' @return a count of the number of times the expression evaluated to true, in the current context
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' tmp = iris %>% dplyr::group_by(Species)
#' tmp %>% dplyr::summarise(long_ones = p_count_if(Petal.Length > 4))
p_count_if = function(..., na.rm = TRUE) {
  return(sum(ifelse(...,1,0), na.rm = na.rm))
}

#' Add a generic comment to the dtrackr history graph
#'
#' A comment can be any kind of note and is added once for every current
#' grouping as defined by the `.message` field. It can be made context specific
#' by including variables such as \{.count\} and \{.total\} in `.message` which
#' refer to the grouped and ungrouped counts at this current stage of the
#' pipeline respectively. It can also pull in any global variable.
#'
#' @param .data a dataframe which may be grouped
#' @param .messages a character vector of glue specifications. A glue
#'   specification can refer to any grouping variables of .data, or any
#'   variables defined in the calling environment, the \{.total\} of all rows,
#'   the \{.count\} variable which is the count in each group and \{.strata\} a
#'   description of the group
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, or any variables defined in the calling environment, or the
#'   \{.total\} variable (which is `nrow(.data)`) and \{.strata\} which is a
#'   description of the grouping
#' @param .type one of "info","...,"exclusion": used to define formatting
#' @param .asOffshoot do you want this comment to be an offshoot of the main
#'   flow (default = FALSE).
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the same .data dataframe with the history graph updated with the comment
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' iris %>% track() %>% comment("hello {.total} rows") %>% history()
p_comment = function(.data, .messages=.defaultMessage(), .headline=.defaultHeadline(), .type="info", .asOffshoot = (.type=="exclusion"), .tag=NULL) {
  .envir = rlang::caller_env()
  .comment(.data, .messages, .headline, .type, .asOffshoot, .tag, .envir)
}

# backend for comment
.comment = function(.data, .messages, .headline,  .type="info", .asOffshoot = (.type=="exclusion"), .tag=NULL, .envir = rlang::caller_env()) {
  if (identical(.messages,NULL) & identical(.headline,NULL)) return(.data)

  .data = .data %>% .untrack()

  .envir$.total = nrow(.data)
  # .headline is a single glue spec
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .envir=.envir)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = .type, .envir=.envir)))
  .data = .data %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot) %>% .writeTag(.tag = .tag)

  return(.data %>% .retrack())
}

#' Add a summary to the dtrackr history graph
#'
#' In the middle of a pipeline you may wish to document something about the data
#' that is more complex than the simple counts. `status` is essentially a
#' `dplyr` summarisation step which is connected to a `glue` specification
#' output, that is recorded in the data frame history. This means you can do an
#' arbitrary interim summarisation and put the result into the flowchart without
#' disrupting the pipeline flow.
#'
#' Because of the ... summary specification parameters MUST BE NAMED.
#'
#' @param .data a dataframe which may be grouped
#' @param ... any normal dplyr::summarise specification, e.g. `count=n()` or
#'   `av=mean(x)`, etcetera.
#' @param .messages a character vector of glue specifications. A glue
#'   specification can refer to the summary outputs, any grouping variables of
#'   .data, the \{.strata\}, or any variables defined in the calling environment
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, or any variables defined in the calling environment
#' @param .type one of "info","exclusion": used to define formatting
#' @param .asOffshoot do you want this comment to be an offshoot of the main
#'   flow (default = FALSE).
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the same .data dataframe with the history metadata updated with the
#'   status inserted as a new stage
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' tmp = iris %>% track() %>% group_by(Species)
#' tmp %>% status(
#'       long = p_count_if(Petal.Length>5),
#'       short = p_count_if(Petal.Length<2),
#'       .messages="{Species}: {long} long ones & {short} short ones"
#' ) %>% history()
p_status = function(.data, ..., .messages=.defaultMessage(), .headline=.defaultHeadline(), .type="info", .asOffshoot = FALSE, .tag=NULL) {
  if (.isPaused(.data)) return(.data) # save the effort of calculating if this is paused but this should
  # TODO: is this needed?
  dots = rlang::enquos(...)
  .data = .data %>% .untrack()
  if(identical(.messages,NULL) & identical(.headline,NULL)) {
    rlang::warn("p_status is missing both .messages and .headline specification. Did you forget to explicitly name them?",.frequency = "always")
    return(.data %>% .retrack())
  }

  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::summarise(!!!dots, .count=dplyr::n(), .groups="keep") %>% dplyr::group_by(!!!grps)

  # .headline is a single glue spec
  tmpHead = .summaryToNodesDf(out,.headline,.isHeader=TRUE, .type = .type, .envir=.envir)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(out,m,.isHeader=FALSE, .type = .type, .envir=.envir)))
  .data = .data %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot) %>% .writeTag(.tag = .tag, ...)
  return(.data %>% .retrack())
}

#' Add a subgroup count to the dtrackr history graph
#'
#' A frequent use case for more detailed description is to have a subgroup count
#' within a flowchart. This works best for factor subgroup columns but other
#' data will be converted to a factor automatically. The count of the items in
#' each subgroup is added as a new stage in the flowchart.
#'
#' @param .data a dataframe which may be grouped
#' @param .subgroup a column with a small number of levels (e.g. a factor)
#' @param ... passed to `base::factor(subgroup values, ...)` to allow reordering of levels etc.
#' @param .messages a character vector of glue specifications. A glue
#'   specification can refer to anything from the calling environment and \{.name\}
#'   for the subgroup name, \{.count\} for the subgroup count, \{.subtotal\} for the
#'   current grouping count and \{.total\} for the whole count
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, \{.subtotal\} for the current grouping count, or any variables
#'   defined in the calling environment
#' @param .type one of "info","exclusion": used to define formatting
#' @param .asOffshoot do you want this comment to be an offshoot of the main
#'   flow (default = FALSE).
#' @param .tag if you want to use the summary data from this step in the future then
#'   give it a name with .tag.
#' @param .maxsubgroups the maximum number of discrete values allowed in .subgroup is
#'   configurable with `options("dtrackr.max_supported_groupings"=XX)`. The
#'   default is 16. Large values produce unwieldy flow charts.
#'
#' @return the same .data dataframe with the history graph updated with a
#'   subgroup count as a new stage
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#' survival::cgd %>% track() %>% group_by(treat) %>%
#'   count_subgroup(center) %>% history()
p_count_subgroup = function(.data, .subgroup, ..., .messages=.defaultCountSubgroup(), .headline=.defaultHeadline(), .type="info", .asOffshoot = FALSE, .tag=NULL, .maxsubgroups=.defaultMaxSupportedGroupings()) {

  .count = .name = NULL
  .subgroup = rlang::ensym(.subgroup)
  if (.isPaused(.data)) return(.data) # save the effort of calculating if this is paused but this should
  if (length(.messages) > 1) stop("count_subgroup() only supports a single message format (i.e. .messages must be of length 1). This is repeated for each subgroup level.")
  type = .data %>% dplyr::pull(!!.subgroup)
  if ( ! .is.discrete(type, .maxsubgroups) ) stop("the subgroup column must be discrete with fewer than ",.maxsubgroups," values. You will need to cut the data in an appropriate way, or increase the .maxsubgroups parameter.")

  .data = .data %>% .untrack()
  if(identical(.messages,NULL) & identical(.headline,NULL)) {
    rlang::warn("p_count_subgroup is missing both .messages and .headline specification. Did you forget to explicitly name them?",.frequency = "always")
    return(.data %>% .retrack())
  }

  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)
  grps = .data %>% dplyr::groups()
  tmp = .data %>% mutate(.name = factor(!!.subgroup, ...)) %>%
    dplyr::group_by(!!!grps, .name, !!.subgroup) %>%
    dplyr::summarise(.count=dplyr::n(), .groups="keep") %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::mutate(.subtotal=sum(.count))

  # .headline is a single glue spec
  tmpHead = .summaryToNodesDf(
    # we need to make tmp unique here (on a per group basis)
    tmp %>% dplyr::select(-c(.name, .count, tidyselect::all_of(.subgroup))) %>% dplyr::distinct(),
    .headline,.isHeader=TRUE, .type = .type, .envir=.envir)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .envir=.envir)))
  .data = .data %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot) %>% .writeTag(.tag = .tag, ...)
  return(.data %>% .retrack())
}


#' Exclude all items matching one or more criteria
#'
#' Apply a set of filters and summarise the actions of the filter to the `dtrackr`
#' history graph. Because of the ... filter specification, all parameters MUST BE
#' NAMED. The filters work in an combinatorial manner, i.e. the results EXCLUDE ALL
#' rows that match any of the criteria. If `na.rm = TRUE` they also remove
#' anything that cannot be evaluated by any criteria.
#'
#' @param .data a dataframe which may be grouped
#' @param ... a dplyr filter specification as a set of formulae where the LHS are
#'   predicates to test the data set against, items that match any of the predicates will
#'   be excluded. The RHS is a glue specification, defining the message, to be
#'   entered in the history graph for each predicate. This can refer to grouping variables
#'   variables from the environment and \{.excluded\} and \{.matched\} or
#'   \{.missing\} (excluded = matched+missing), \{.count\} and \{.total\} -
#'   group and overall counts respectively, e.g. "excluding \{.matched\} items
#'   and \{.missing\} with missing values".
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, or any variables defined in the calling environment
#' @param na.rm (default FALSE) if the filter cannot be evaluated for a row
#'   count that row as missing and either exclude it (TRUE) or don't exclude it
#'   (FALSE)
#' @param .type default "exclusion": used to define formatting
#' @param .asOffshoot do you want this comment to be an offshoot of the main
#'   flow (default = TRUE).
#' @param .stage a name for this step in the pathway
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the filtered .data dataframe with the history graph updated with the
#'   summary of excluded items as a new offshoot stage
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' iris %>% track() %>% capture_exclusions() %>% exclude_all(
#'       Petal.Length > 5 ~ "{.excluded} long ones",
#'       Petal.Length < 2 ~ "{.excluded} short ones"
#' ) %>% history()
#'
#'
#' # simultaneous evaluation of criteria:
#' data.frame(a = 1:10) %>%
#'   track() %>%
#'   exclude_all(
#'     # These two criteria identify the same value and one item is excluded
#'     a > 9 ~ "{.excluded} value > 9",
#'     a == max(a) ~ "{.excluded} max value",
#'   ) %>%
#'   status() %>%
#'   history()
#'
#' # the behaviour is equivalent to the inverse of dplyr's filter function:
#' data.frame(a=1:10) %>%
#'   dplyr::filter(a <= 9, a != max(a)) %>%
#'   nrow()
#'
#' # step-wise evaluation of criteria results in a different output
#' data.frame(a = 1:10) %>%
#'   track() %>%
#'   # Performing the same exclusion sequentially results in 2 items
#'   # being excluded as the criteria no longer identify the same
#'   # item.
#'   exclude_all(a > 9 ~ "{.excluded} value > 9") %>%
#'   exclude_all(a == max(a) ~ "{.excluded} max value") %>%
#'   status() %>%
#'   history()
#'
#' # the behaviour is equivalent to the inverse of dplyr's filter function:
#' data.frame(a=1:10) %>%
#'   dplyr::filter(a <= 9) %>%
#'   dplyr::filter(a != max(a)) %>%
#'   nrow()
#'
p_exclude_all = function(.data, ..., .headline=.defaultHeadline(), na.rm=FALSE, .type="exclusion", .asOffshoot = TRUE, .stage=(if(is.null(.tag)) "" else .tag), .tag=NULL) {
  .excl = .excl.na = .retain = .strata = .message = .excluded = .filter = NULL
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  filters = rlang::list2(...)
  if (length(filters)==0) {
    rlang::warn("No exclusions defined on p_exclude_all.",.frequency = "always")
    return(.data %>% .retrack())
  }
  .envir$.total = nrow(.data)
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type=.type, .envir=.envir)
  #TODO: can we get rid of this?:
  messages = .dataToNodesDf(.data,.glue = "no exclusions",.isHeader=FALSE,.type=.type,.envir = .envir) %>% mutate(.excluded = -1)
  grps = .data %>% dplyr::groups()
  grpLabels = grps %>% lapply(rlang::as_label) %>% unlist() %>% as.character()
  out = .data %>% dplyr::mutate(.retain = TRUE)
  excluded = NULL

  for(filter in filters) {
    glueSpec = rlang::f_rhs(filter)
    filt = rlang::f_lhs(filter)
    filtStr = paste0(sapply(deparse(filt),trimws),collapse=" ")
    out = out %>% #dplyr::group_modify(function(d,g,...) {
      #d %>%
       # dplyr::mutate(.excl = rlang::eval_tidy(filt,data = d, env=.envir)) %>%
        dplyr::mutate(.excl = rlang::eval_tidy(filt, data = dplyr::cur_data_all(), env=.envir)) %>%
        dplyr::mutate(.excl.na = ifelse(is.na(.excl),na.rm,.excl)) %>%
        dplyr::mutate(.retain = .retain & !.excl.na)
    #})
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
      dplyr::group_by(!!!grps) %>%
      .createStrataCol()
    tmp$.message = rlang::eval_tidy(.doGlue(tmp,glueSpec,.envir), data=tmp, env = .envir)

    if(.trackingExclusions(.data)) {
      # browser()
      exclusions = out %>%
        dplyr::filter(.excl.na) %>%
        dplyr::select(-.retain,-.excl.na,-.excl) %>%
        dplyr::group_by(!!!grps) %>%
        .createStrataCol() %>%
        dplyr::inner_join(
          tmp %>% dplyr::select(.strata, .message), by=".strata"
        ) %>%
        dplyr::group_by() %>% # ?is this supposed to be here
        tidyr::nest(.excluded = !c(.strata,.message)) %>%
        dplyr::mutate(.filter = filtStr) %>%
        dplyr::select(.strata,.message,.excluded,.filter)
      excluded = dplyr::bind_rows(excluded,exclusions)

    }
    messages = messages %>% dplyr::bind_rows(tmp %>% dplyr::mutate(.isHeader=FALSE,.type=.type))
  }
  if(!.defaultShowZeroExclusions()) messages = messages %>% filter(.excluded != 0)

  messages = messages %>% group_by(!!!grps) %>% filter(!(.message=="no exclusions" & dplyr::n() > 1))
  out = out %>% dplyr::filter(.retain) %>% dplyr::select(-.retain,-.excl, -.excl.na) %>% p_copy(.data) %>%
    .writeMessagesToNode(.df = dplyr::bind_rows(tmpHead,messages), .asOffshoot = .asOffshoot, .excluded=excluded, .stage = .stage)  %>%
    .writeTag(.tag = .tag)
  return(out %>% .retrack())
}

#' Include any items matching a criteria
#'
#' Apply a set of inclusion criteria and record the actions of the
#' filter to the `dtrackr` history graph. Because of the ... filter specification,
#' all parameters MUST BE NAMED. This function is the opposite of
#' [dtrackr::exclude_all()] and the filtering criteria work to identify rows to
#' include i.e. the results include anything that match any of the criteria. If
#' `na.rm=TRUE` they also keep anything that cannot be evaluated by the criteria.
#'
#' @param .data a dataframe which may be grouped
#' @param ... a dplyr filter specification as a set of formulae where the LHS
#'   are predicates to test the data set against, items that match at least one
#'   of the predicates will be included. The RHS is a glue specification,
#'   defining the message, to be entered in the history graph for each predicate
#'   matched. This can refer to grouping variables, variables from the
#'   environment and \{.included\} and \{.matched\} or \{.missing\} (included =
#'   matched+missing), \{.count\} and \{.total\} - group and overall counts
#'   respectively, e.g. "excluding \{.matched\} items and \{.missing\} with
#'   missing values".
#' @param .headline a glue specification which can refer to grouping variables
#'   of .data, or any variables defined in the calling environment
#' @param na.rm (default TRUE) if the filter cannot be evaluated for a row
#'   count that row as missing and either exclude it (TRUE) or don't exclude it
#'   (FALSE)
#' @param .type default "inclusion": used to define formatting
#' @param .asOffshoot do you want this comment to be an offshoot of the main
#'   flow (default = FALSE).
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the filtered .data dataframe with the history graph updated with the
#'   summary of included items as a new stage
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' iris %>% track() %>% group_by(Species) %>% include_any(
#'       Petal.Length > 5 ~ "{.included} long ones",
#'       Petal.Length < 2 ~ "{.included} short ones"
#' ) %>% history()
#'
#' # simultaneous evaluation of criteria:
#' data.frame(a = 1:10) %>%
#'   track() %>%
#'   include_any(
#'     # These two criteria identify the same value and one item is excluded
#'     a > 1 ~ "{.included} value > 1",
#'     a != min(a) ~ "{.included} everything but the smallest value",
#'   ) %>%
#'   status() %>%
#'   history()
#'
#' # the behaviour is equivalent to dplyr's filter function:
#' data.frame(a=1:10) %>%
#'   dplyr::filter(a > 1, a != min(a)) %>%
#'   nrow()
#'
#' # step-wise evaluation of criteria results in a different output
#' data.frame(a = 1:10) %>%
#'   track() %>%
#'   # Performing the same exclusion sequentially results in 2 items
#'   # being excluded as the criteria no longer identify the same
#'   # item.
#'   include_any(a > 1 ~ "{.included} value > 1") %>%
#'   include_any(a != min(a) ~ "{.included} everything but the smallest value") %>%
#'   status() %>%
#'   history()
#'
#' # the behaviour is equivalent to dplyr's filter function:
#' data.frame(a=1:10) %>%
#'   dplyr::filter(a > 1) %>%
#'   dplyr::filter(a != min(a)) %>%
#'   nrow()
#'
p_include_any = function(.data, ..., .headline=.defaultHeadline(), na.rm=TRUE, .type="inclusion", .asOffshoot = FALSE, .tag=NULL) {
  .incl = .incl.na = .retain = NULL
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  filters = rlang::list2(...)
  if (length(filters)==0) {
    rlang::warn("No inclusions defined on p_include_any.",.frequency = "always")
    return(.data %>% .retrack())
  }
  .envir$.total = nrow(.data)
  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type=.type, .envir=.envir)
  messages = .dataToNodesDf(.data,.glue = "inclusions:",.isHeader=FALSE,.type=.type,.envir = .envir)
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::mutate(.retain = FALSE)
  for(filter in filters) {
    glueSpec = rlang::f_rhs(filter)
    filt = rlang::f_lhs(filter)
    # out = out %>% dplyr::group_modify(function(d,g,...) {
    #   d %>%
    #     dplyr::mutate(.incl = rlang::eval_tidy(filt, data = d, env=.envir)) %>%
    #     dplyr::mutate(.incl.na = ifelse(is.na(.incl),!na.rm,.incl)) %>%
    #     dplyr::mutate(.retain = .retain | .incl.na)
    # })
    # Fix for github issue #26. I didn't want to do this in a group_modify as it loses the grouped
    # columns (which I could have reassembled with the g parameter). Instead I am doing this
    # in a standard mutate using the dplyr::cur_data_all. This behaviour is demonstrated here:
    # iris %>% group_by(Species) %>% filter(rlang::eval_tidy( quo(Petal.Width==max(Petal.Width)), data = cur_data_all()))
    # where the result should be 5 long with entries for each Species.
    out = out %>%
        dplyr::mutate(.incl = rlang::eval_tidy(filt, data = dplyr::cur_data_all(), env=.envir)) %>%
        dplyr::mutate(.incl.na = ifelse(is.na(.incl),!na.rm,.incl)) %>%
        dplyr::mutate(.retain = .retain | .incl.na)

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
    # tmp$.message = rlang::eval_tidy(glue::glue_data(tmp,glueSpec,.envir = .envir), data=tmp, env = .envir)
    tmp$.message = rlang::eval_tidy(.doGlue(tmp,glueSpec,.envir), data=tmp, env = .envir)
    messages = messages %>% dplyr::bind_rows(tmp %>% dplyr::mutate(.isHeader=FALSE,.type=.type))
  }
  out = out %>% dplyr::filter(.retain) %>% dplyr::select(-.retain,-.incl, -.incl.na) %>% p_copy(.data) %>%
    .writeMessagesToNode(.df = dplyr::bind_rows(tmpHead,messages), .asOffshoot = .asOffshoot) %>% .writeTag(.tag = .tag)
  return(out %>% .retrack())
}

#' Remove a stratification from a data set
#'
#' Un-grouping a data set logically combines the different arms. In the history
#' this joins any stratified branches and acts as a specific type of [dtrackr::status()],
#' allowing you to generate some summary statistics about the un-grouped data.
#' See [dplyr::ungroup()].
#' @seealso dplyr::ungroup()
#'
#' @inheritParams dplyr::ungroup
#' @inheritDotParams dplyr::ungroup
#' @param .messages a set of glue specs. The glue code can use any any global
#'   variable, or \{.count\}. the default is "total \{.count\} items"
#' @param .headline a headline glue spec. The glue code can use \{.count\} and
#'   \{.strata\}.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the .data dataframe but ungrouped with the history graph
#'   updated showing the ungroup operation as a new stage.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% group_by(Species) %>% comment("A test")
#' tmp %>% ungroup(.messages="{.count} items in combined") %>% history()
p_ungroup = function(x, ..., .messages=.defaultMessage(), .headline=.defaultHeadline(), .tag=NULL) {
  .data = x %>% .untrack()
  # dots = dplyr::enexprs(...)
  # if(length(dots)==0) dots = list(.count=rlang::expr(dplyr::n()))
  # out = .data %>% dplyr::ungroup() %>% p_copy(.data) %>% p_status(!!!dots, .messages=.messages, .headline = .headline, .type="summarise", .tag=.tag)

  dots = list(.count=rlang::expr(dplyr::n()))
  out = .data %>%
    dplyr::ungroup(...) %>%
    p_copy(.data) %>%
    .retrack()

  if (.isPaused(x,auto = TRUE)) {
    if (!getOption("dtrackr.silent",FALSE)) {
      rlang::inform("Automatically resuming tracking.",.frequency = "always")
    }
    out = out %>% p_resume()
  }

  out = out %>% p_status(!!!dots, .messages=.messages, .headline = .headline, .type="summarise", .tag=.tag)

  return(out)
}

#' Summarise a data set
#'
#' Summarising a data set acts in the normal `dplyr` manner to collapse groups
#' to individual rows. Any columns resulting from the summary can be added to
#' the history graph In the history this also joins any stratified branches and
#' allows you to generate some summary statistics about the un-grouped data. See
#' [dplyr::summarise()].
#' @seealso dplyr::summarise()
#'
#' @inheritParams dplyr::summarise
#' @inheritDotParams dplyr::summarise
#' @param .messages a set of glue specs. The glue code can use any summary
#'   variable defined in the ... parameter, or any global variable, or
#'   \{.strata\}
#' @param .headline a headline glue spec. The glue code can use any summary
#'   variable defined in the ... parameter, or any global variable, or
#'   \{.strata\}
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the .data dataframe summarised with the history graph updated showing
#'   the summarise operation as a new stage
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% group_by(Species)
#' tmp %>% summarise(avg = mean(Petal.Length), .messages="{avg} length") %>% history()
p_summarise = function(.data, ..., .messages = "", .headline="", .tag=NULL) {
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::summarise(...)
  newGrps = out %>% dplyr::groups()
  out = out %>% dplyr::group_by(!!!grps)
  # .headline is a single glue spec
  tmpHead = .summaryToNodesDf(out,.headline,.isHeader=TRUE, .type = "summarise", .envir=.envir)

  # .messages is a load of glue specs
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(out,m,.isHeader=FALSE, .type = "summarise", .envir=.envir)))
  out = out %>% dplyr::group_by(!!!newGrps) %>% p_copy(.data) %>% .writeMessagesToNode(.df=dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE) %>% .writeTag(.tag, ...)
  return(out %>% .retrack())
}

### Mutate operations ----


.doMutate = function(.mutate_fn, .data, ..., .messages = "", .headline = "", .type, .tag=NULL) {
  .envir = .integrateDots(.mutate_fn, ..., .envir=rlang::caller_env())
  .data = .data %>% .untrack()
  out = .data %>% .mutate_fn(...)
  # TODO: consider whether this is a good idea.
  # If so it probably needs to be done globally.
  # .envir$.data.out = out
  .envir$.cols = paste0(colnames(out), collapse=", ")
  .envir$.new_cols = paste0(setdiff(colnames(out),colnames(.data)), collapse=", ")
  .envir$.dropped_cols = paste0(setdiff(colnames(.data),colnames(out)), collapse=", ")
  out = out %>% p_copy(.data) %>% .comment(.messages=.messages, .headline = .headline, .type=.type, .tag=.tag, .envir=.envir)
  return(out %>% .retrack())
}

#' dplyr modifying operations
#'
#' Equivalent `dplyr` functions for mutating, selecting and renaming a data set
#' act in the normal way. mutates / selects / rename generally don't add
#' anything in documentation so the default behaviour is to miss these out of
#' the history. This can be overridden with the .messages, or .headline values
#' in which case they behave just like a `comment()` See [dplyr::mutate()],
#' [dplyr::add_count()], [dplyr::add_tally()], [dplyr::transmute()],
#' [dplyr::select()], [dplyr::relocate()], [dplyr::rename()]
#' [dplyr::rename_with()], [dplyr::arrange()] for more details.
#'
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, grouping variable, \{.new_cols\} or \{.dropped_cols\} for changes to
#'   columns, \{.cols\} for the output column names, or \{.strata\}. Defaults to nothing.
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, grouping variable, \{.new_cols\}, \{.dropped_cols\}, \{.cols\} or \{.strata\}.
#'   Defaults to nothing.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the .data dataframe after being modified by the `dplyr` equivalent
#'   function, but with the history graph updated with a new stage if the
#'   `.messages` or `.headline` parameter is not empty.
#'
#' @seealso dplyr::mutate()
#' @inheritParams dplyr::mutate
#' @inheritDotParams dplyr::mutate
#' @export
#' @example inst/examples/mutate-examples.R
p_mutate = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::mutate, .data, ..., .messages = .messages, .headline = .headline, .type="mutate", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::add_count()
#' @inheritParams dplyr::add_count
#' @inheritDotParams dplyr::add_count
#' @export
#' @example inst/examples/add-count-tally-examples.R
p_add_count = function(x, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::add_count, x, ..., .messages=.messages, .headline = .headline, .type="add_count", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::add_tally()
#' @inheritParams dplyr::add_tally
#' @inheritDotParams dplyr::add_tally
#' @export
#' @example inst/examples/add-count-tally-examples.R
p_add_tally = function(x, ..., .messages = "", .headline = "", .tag=NULL) {
  if (!.isTracked(x)) return(dplyr::add_tally(x,...))
  .doMutate(dplyr::add_tally, x, ..., .messages=.messages, .headline = .headline, .type="add_tally", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::transmute()
#' @inheritParams dplyr::transmute
#' @inheritDotParams dplyr::transmute
#' @export
#' @example inst/examples/transmute-examples.R
p_transmute = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::transmute, .data, ..., .messages=.messages, .headline = .headline, .type="transmute", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::select()
#' @inheritParams dplyr::select
#' @inheritDotParams dplyr::select
#' @export
#' @example inst/examples/select-examples.R
p_select = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::select, .data, ..., .messages=.messages, .headline = .headline, .type="transmute", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::relocate()
#' @inheritParams dplyr::relocate
#' @inheritDotParams dplyr::relocate
#' @export
#' @example inst/examples/relocate-examples.R
p_relocate = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::relocate, .data, ..., .messages=.messages, .headline = .headline, .type="relocate", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::rename()
#' @inheritParams dplyr::rename
#' @inheritDotParams dplyr::rename
#' @export
#' @example inst/examples/rename-examples.R
p_rename = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::rename, .data, ..., .messages=.messages, .headline = .headline, .type="rename", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::rename_with()
#' @inheritParams dplyr::rename_with
#' @inheritDotParams dplyr::rename_with
#' @export
#' @example inst/examples/rename-examples.R
p_rename_with = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::rename_with, .data, ..., .messages=.messages, .headline = .headline, .type="rename_with", .tag=.tag)
}

#' @inherit p_mutate
#' @seealso dplyr::arrange()
#' @inheritParams dplyr::arrange
#' @inheritParams dplyr::arrange
#' @export
#' @example inst/examples/arrange-examples.R
p_arrange = function(.data, ..., .messages = "", .headline = "", .tag=NULL) {
  .doMutate(dplyr::arrange, .data, ..., .messages=.messages, .headline = .headline, .type="arrange", .tag=.tag)
}


#' Reshaping data using `tidyr::pivot_wider`
#'
#' A drop in replacement for [tidyr::pivot_wider()] which optionally takes a
#' message and headline to store in the history graph.
#' @seealso tidyr::pivot_wider()
#'
#' @inheritParams tidyr::pivot_wider
#' @inheritDotParams tidyr::pivot_wider
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, grouping variable, or \{.strata\}. Defaults to nothing.
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, grouping variable, or \{.strata\}. Defaults to nothing.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the data dataframe result of the `tidyr::pivot_wider` function but with
#'   a history graph updated with a `.message` if requested.
#' @export
p_pivot_wider = function(
    data, ..., .messages = "", .headline = "", .tag=NULL
) {
  names_from <- rlang::enquo(names_from)
  values_from <- rlang::enquo(values_from)
  .data = data %>% .untrack()
  out = .data %>% tidyr::pivot_wider(...)
  # TODO: shold this be a .beforeAfterGroupwiseCount operation as it goes from narrow to long
  out = out %>% p_copy(.data) %>% .comment(.messages, .headline = .headline, .type="pivot_wider", .tag=.tag)
  return(out %>% .retrack())
}

#' Reshaping data using `tidyr::pivot_longer`
#'
#' A drop in replacement for [tidyr::pivot_longer()] which optionally takes a
#' message and headline to store in the history graph.
#' @seealso tidyr::pivot_longer()
#'
#' @inheritParams  tidyr::pivot_longer
#' @inheritDotParams  tidyr::pivot_longer
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, grouping variable, or \{.strata\}. Defaults to nothing.
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, grouping variable, or \{.strata\}. Defaults to nothing.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the result of the `tidyr::pivot_longer` but with a history graph
#'   updated.
#' @export
p_pivot_longer = function(data, ..., .messages = "", .headline = "", .tag=NULL) {
  .data = data %>% .untrack()
  out = .data %>% tidyr::pivot_longer(...)
  out = out %>% p_copy(.data) %>% .comment(.messages, .headline = .headline, .type="pivot_longer", .tag=.tag)
  return(out %>% .retrack())
}


#' Stratifying your analysis
#'
#' Grouping a data set acts in the normal way. When tracking a dataframe
#' sometimes a `group_by()` operation will create a lot of groups. This happens
#' for example if you are doing a `group_by()`, `summarise()` step that is
#' aggregating data on a fine scale, e.g. by day in a timeseries. This is
#' generally a terrible idea when tracking a dataframe as the resulting
#' flowchart will have many many branches and be illegible. `dtrackr` will detect this issue and
#' pause tracking the dataframe with a warning. It is up to the user to the
#' `resume()` tracking when the large number of groups have been resolved e.g.
#' using a `dplyr::ungroup()`. This limit is configurable with
#' `options("dtrackr.max_supported_groupings"=XX)`. The default is 16. See
#' [dplyr::group_by()].
#' @seealso dplyr::group_by()
#'
#' @inheritParams dplyr::group_by
#' @inheritDotParams dplyr::group_by
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, or \{.cols\} which is the columns that are being grouped by.
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, or \{.cols\}.
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#' @param .maxgroups the maximum number of subgroups allowed before the tracking
#'   is paused.
#'
#' @return the .data but grouped.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% track() %>% group_by(Species, .messages="stratify by {.cols}")
#' tmp %>% comment("{.strata}") %>% history()
p_group_by = function(.data, ..., .messages = "stratify by {.cols}",  .headline=NULL, .tag=NULL, .maxgroups = .defaultMaxSupportedGroupings()) {
  # explicitly dtrackr::ungroup if .add is false to generate an un-grouped node
  # in the graph. otherwise we get an n x m crossover point which the
  # flowchart can't handle.

  # check for a .add parameter in the group by input.
  dots = rlang::enexprs(...)
  .add = isTRUE(dots$.add)

  if(!.add & dplyr::is.grouped_df(.data)) .data = .data %>% ungroup()
  # TODO: putting in a special hidden node type
  if(is.null(.messages) & is.null(.headline)) stop("group_by .messages cannot be NULL, or else there is nothing to attach the other nodes to.")

  .data = .data %>% .untrack()

  tmp = .data %>% dplyr::group_by(...)
  # figure out final grouping - only for the strata label though
  col =  tmp %>% dplyr::groups()
  .cols = col %>% sapply(rlang::as_label) %>% as.character() %>% paste(collapse=", ")

  # check the size of the final grouping
  final_groups = tmp %>% dplyr::n_groups()

  if (final_groups <= .maxgroups ) {
    # A small grouping.
    tmp = tmp %>% p_copy(.data)
    # check to see if we should auto resume
    if (.isPaused(.data,auto = TRUE)) {
      if (!getOption("dtrackr.silent",FALSE)) {
        rlang::inform("Automatically resuming tracking.",.frequency = "always")
      }
      tmp = tmp %>% p_resume()
    }
    tmp = tmp %>% .comment(.messages, .headline = .headline, .type="stratify", .tag=.tag)

  } else {

    # This group by has resulted in lots of groups
    if (!.isPaused(.data)) {
      # if the input is not already paused we pause it with a flag to allow auto
      # resume.
      if (!getOption("dtrackr.silent",FALSE)) {
        rlang::inform(paste0("This group_by() has created more than the maximum number of supported groupings (",.defaultMaxSupportedGroupings(),") which will likely impact performance. We have paused tracking the dataframe."),.frequency = "always")
        rlang::inform("To change this limit set the option 'dtrackr.max_supported_groupings'. To continue tracking use ungroup() then dtrackr::resume() once groupings have become a bit more manageable",.frequency = "once",.frequency_id = "maxgrp")
      }
      tmp = .data %>% dplyr::group_by(...) %>% p_copy(.data) %>% p_pause(auto = TRUE)
    } else {
      tmp = .data %>% dplyr::group_by(...) %>% p_copy(.data)
    }

  }

  return(tmp %>% .retrack())

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
#' Distinct acts in the same way as in `dplyr::distinct`. Prior to the operation
#' the size of the group is calculated \{.count.in\} and after the operation the
#' output size \{.count.out\} The group \{.strata\} is also available (if
#' grouped) for reporting. See [dplyr::distinct()].
#' @seealso dplyr::distinct()
#'
#' @inheritParams dplyr::distinct
#' @inheritDotParams dplyr::distinct
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the .data dataframe with distinct values and history graph updated.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = bind_rows(iris %>% track(), iris %>% track() %>% filter(Petal.Length > 5))
#' tmp %>% group_by(Species) %>% distinct() %>% history()
p_distinct = function(.data, ..., .messages="removing {.count.in-.count.out} duplicates", .headline=.defaultHeadline(), .tag=NULL) {
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)
  grps = .data %>% dplyr::groups()

  out = .data %>% dplyr::distinct(...)

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = "modify", .envir=.envir)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = "modify", .envir=.envir)))

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE) %>% .writeTag(.tag, .content = tmp)
  return(out %>% .retrack())
}


#' Filtering data
#'
#' Filter acts in the same way as in `dplyr` where predicates which evaluate to
#' TRUE act to select items to include, and items for which the predicate cannot
#' be evaluated are excluded. For tracking prior to the filter operation the
#' size of each group is calculated \{.count.in\} and after the operation the
#' output size of each group \{.count.out\}. The grouping \{.strata\} is also
#' available (if grouped) for reporting. See [dplyr::filter()].
#'
#' @seealso dplyr::filter()
#' @inheritParams dplyr::filter
#' @inheritDotParams dplyr::filter
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .type the format type of the action typically an exclusion
#' @param .asOffshoot if the type is exclusion, `.asOffshoot` places the
#'   information box outside of the main flow, as an exclusion.
#' @param .stage a name for this step in the pathway
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with `.tag`.
#'
#' @return the filtered .data dataframe with history graph updated
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% track() %>% group_by(Species)
#' tmp %>% filter(Petal.Length > 5) %>% history()
p_filter = function(.data,
                    ...,
                    .messages="excluded {.excluded} items",
                    .headline=.defaultHeadline(),
                    .type = "exclusion",
                    .asOffshoot=(.type=="exclusion"),
                    .stage=(if(is.null(.tag)) "" else .tag),
                    .tag=NULL) {
  .count.in = .count.out = .strata = .message = NULL
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)
  grps = .data %>% dplyr::groups()

  #tryCatch({
  filterExprs = rlang::enexprs(...)
  # }, error= function(e) {
  #   # TODO: Support accross syntax
  #   stop("dtrackr does not yet support filtering by things that are not a set of simple expressions (e.g. across() syntax). You should untrack the dataframe before trying this.")
  # })

  out = .data %>% dplyr::filter(...)

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .envir=.envir)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmp = tmp %>% dplyr::mutate(.excluded = .count.in-.count.out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .envir=.envir)))

  excluded = NULL
  if(.trackingExclusions(.data)) {
    excluded = .data %>%
      dplyr::anti_join(out, by=colnames(.data)) %>%
      dplyr::group_by(!!!grps) %>%
      .createStrataCol() %>%
      dplyr::inner_join(tmpBody %>% select(.strata,.message), by=".strata") %>%
      dplyr::ungroup() %>%
      tidyr::nest(.excluded = !c(.strata,.message)) %>%
      dplyr::mutate(.filter = paste0(sapply(sapply(filterExprs,deparse),trimws), collapse=", "))
  }

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot, .excluded=excluded, .stage = .stage) %>% .writeTag(.tag, .content = tmp)
  return(out %>% .retrack())

}

### Slice support ----


.doSlice = function(.slice_fn, .data, ..., .messages=c("{.count.in} before","{.count.out} after"), .headline=.defaultHeadline()) {
  .type="inclusion"
  .asOffshoot = FALSE
  # tagging slice operations?, .stage="", .tag=NULL
  .count.in = .count.out = .strata = .message = NULL
  .data = .data %>% .untrack()
  .envir = .integrateDots(.slice_fn, ..., .envir=rlang::caller_env())
  # .envir$.total = nrow(.data)
  grps = .data %>% dplyr::groups()

  out = .data %>% .slice_fn(...)
  .envir$.data.out = out
  tmpHead = .dataToNodesDf(.data, .headline,.isHeader=TRUE, .type = .type, .envir=.envir)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmp = tmp %>% dplyr::mutate(.excluded = .count.in-.count.out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .envir=.envir)))

  # Not defined what tracking exclusions looks like for slice operations
  # excluded = NULL
  # if(.trackingExclusions(.data)) {
  #   excluded = .data %>%
  #     dplyr::anti_join(out, by=colnames(.data)) %>%
  #     dplyr::group_by(!!!grps) %>%
  #     .createStrataCol() %>%
  #     dplyr::inner_join(tmpBody %>% select(.strata,.message), by=".strata") %>%
  #     dplyr::ungroup() %>%
  #     tidyr::nest(.excluded = !c(.strata,.message)) %>%
  #     dplyr::mutate(.filter = paste0(sapply(sapply(filterExprs,deparse),trimws), collapse=", "))
  # }
  #
  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=.asOffshoot) #, .excluded=excluded) #, .stage = .stage) # %>% .writeTag(.tag, .content = tmp)
  return(out %>% .retrack())

}

#' Slice operations
#'
#' Slice operations behave as in dplyr, except the history graph can be updated with
#' tracked dataframe with the before and after sizes of the dataframe.
#' See [dplyr::slice()], [dplyr::slice_head()], [dplyr::slice_tail()],
#' [dplyr::slice_min()], [dplyr::slice_max()], [dplyr::slice_sample()],
#' for more details on the underlying functions.
#'
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.count.in\}, \{.count.out\} for the input and output dataframes
#'   sizes respectively and \{.excluded\} for the difference
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.count.in\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively.
#' @return the sliced dataframe with the history graph updated.
#'
#' @seealso dplyr::slice()
#' @inheritParams dplyr::slice
#' @inheritDotParams dplyr::slice
#' @export
#' @example inst/examples/slice-examples.R
p_slice = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' @inherit p_slice
#' @seealso dplyr::slice_head()
#' @inheritDotParams dplyr::slice_head
#' @export
#' @example inst/examples/slice-head-tail-examples.R
p_slice_head = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice_head, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' @inherit p_slice
#' @seealso dplyr::slice_tail()
#' @inheritDotParams dplyr::slice_tail
#' @export
#' @example inst/examples/slice-head-tail-examples.R
p_slice_tail = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice_tail, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' @inherit p_slice
#' @seealso dplyr::slice_min()
#' @inheritDotParams dplyr::slice_min
#' @export
#' @example inst/examples/slice-max-min-examples.R
p_slice_min = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice_min, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' @inherit p_slice
#' @seealso dplyr::slice_max()
#' @inheritDotParams dplyr::slice_max
#' @export
#' @example inst/examples/slice-max-min-examples.R
p_slice_max = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice_max, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' @inherit p_slice
#' @seealso dplyr::slice_sample()
#' @inheritDotParams dplyr::slice_sample
#' @export
#' @example inst/examples/slice-sample-examples.R
p_slice_sample = function(.data, ..., .messages = c("{.count.in} before","{.count.out} after"), .headline="slice data") {
  .doSlice(dplyr::slice_sample, .data = .data, ..., .messages = .messages, .headline = .headline)
}

#' Group-wise modification of data and complex operations
#'
#' Group modifying a data set acts in the normal way. The internal mechanics of
#' the modify function are opaque to the history. This means these can be used
#' to wrap any unsupported operation without losing the history (e.g. `df %>%
#' track() %>% group_modify(function(d,...) { d %>% unsupported_operation() })`
#' ) Prior to the operation the size of the group is calculated \{.count.in\}
#' and after the operation the output size \{.count.out\} The group \{.strata\}
#' is also available (if grouped) for reporting See [dplyr::group_modify()].
#'
#' @seealso dplyr::group_modify()
#'
#' @inheritParams dplyr::group_modify
#' @inheritDotParams dplyr::group_modify
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .headline a headline glue spec. The glue code can use any global
#'   variable, or \{.strata\},\{.count.in\},and \{.count.out\}
#' @param .type default "modify": used to define formatting
#' @param .tag if you want the summary data from this step in the future then
#'   give it a name with .tag.
#'
#' @return the transformed .data dataframe with the history graph updated.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% track() %>% group_by(Species)
#' tmp %>% group_modify(
#'       function(d,g,...) { return(tibble::tibble(x=runif(10))) },
#'       .messages="{.count.in} in, {.count.out} out"
#' ) %>% history()
p_group_modify = function(.data, ..., .messages=NULL, .headline=.defaultHeadline(), .type = "modify", .tag=NULL) {
  .data = .data %>% .untrack()
  .envir = rlang::caller_env()
  .envir$.total = nrow(.data)

  tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .envir=.envir)

  grps = .data %>% dplyr::groups()
  out = .data %>% dplyr::group_modify(...)
  tmp = .beforeAfterGroupwiseCounts(.data,out)
  tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .summaryToNodesDf(tmp,m,.isHeader=FALSE, .type = .type, .envir=.envir)))

  out = out %>% p_copy(.data) %>% .writeMessagesToNode(dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot=FALSE) %>% .writeTag(.tag, .content = tmp)
  return(out %>% .retrack())
}

## Two DF operations ----

### Set ops ----


.doSetOperation = function(.set_fn, ..., .messages, .headline) {
  dots = rlang::list2(...)
  if(!any(sapply(dots,.isTracked))) return(.set_fn(...))
  .envir = .integrateDots(.set_fn, ..., .envir=rlang::caller_env())
  mergedGraph=.emptyGraph()
  for(item in  dots) {
    if (.isTracked(item)) mergedGraph = .mergeGraphs(mergedGraph, item %>% p_get())
  }
  dots = lapply(dots, .untrack)
  out = rlang::exec(.set_fn, !!!dots)
  .envir$.count.out = nrow(out)
  out = out %>% p_set(mergedGraph) %>% .comment(.messages, .headline = .headline, .type="combine",.envir = .envir)
  return(out %>% .retrack())
}

#' Set operations
#'
#' These perform set operations on tracked dataframes. It merges the history
#' of 2 (or more) dataframes and combines the rows (or columns). It calculates the total number of
#' resulting rows as \{.count.out\} in other terms it performs exactly the same
#' operation as the equivalent `dplyr` operation. See [dplyr::bind_rows()],
#' [dplyr::bind_cols()], [dplyr::intersect()], [dplyr::union()],
#' [dplyr::setdiff()],[dplyr::intersect()], or [dplyr::union_all()] for the
#' underlying function details.
#'
#' @param ... a collection of tracked data frames to combine
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, or \{.count.out\}
#' @param .headline a glue spec. The glue code can use any global variable, or
#'   \{.count.out\}
#' @return the dplyr output with the history graph updated.
#'
#' @seealso dplyr::bind_rows()
#'
#' @export
#' @example inst/examples/set-operation-examples.R
p_bind_rows = function(..., .messages="{.count.out} in union", .headline="Union") {
  .doSetOperation(dplyr::bind_rows, ..., .messages=.messages, .headline = .headline)
}

#' @inherit p_bind_rows
#' @seealso dplyr::bind_cols()
#' @export
p_bind_cols = function(...,
       .messages="{.count.out} in combined set", .headline="Bind columns") {
  .doSetOperation(dplyr::bind_cols, ..., .messages=.messages, .headline = .headline)
}

#' @inherit p_bind_rows
#' @seealso generics::intersect()
#' @inheritParams generics::setops
#' @export
p_intersect = function(x, y, ..., .messages="{.count.out} in intersection", .headline="Intersection") {
  .doSetOperation(dplyr::intersect, x, y, ..., .messages=.messages, .headline = .headline)
}

#' @inherit p_bind_rows
#' @seealso generics::union()
#' @inheritParams  generics::setops
#' @export
p_union = function(x, y, ..., .messages="{.count.out} unique items in union", .headline="Distinct union") {
  .doSetOperation(dplyr::union, x, y, ..., .messages=.messages, .headline = .headline)
}

#' @inherit p_bind_rows
#' @seealso dplyr::union_all()
#' @inheritParams dplyr::setops
#' @export
p_union_all = function(x, y, ..., .messages="{.count.out} items in union", .headline="Union") {
  .doSetOperation(dplyr::union_all, x, y, ..., .messages=.messages, .headline = .headline)
}

#' @inherit p_bind_rows
#' @seealso dplyr::setdiff()
#' @inheritParams generics::setops
#' @export
p_setdiff = function(x, y, ..., .messages="{.count.out} items in difference", .headline="Difference") {
  .doSetOperation(dplyr::setdiff, x, y, ..., .messages=.messages, .headline = .headline)
}

### Joins ----

.doJoin = function(joinFunction, x, y, by=NULL, ..., .messages, .headline) {

  mergedGraph = .mergeGraphs(x %>% p_get(), y %>% p_get())
  x = x %>% .untrack()
  y = y %>% .untrack()

  # TODO: why not here?
  # .envir = .integrateDots(joinFunction, by=by, ..., .envir=rlang::caller_env())

  if (is.null(by)) {
    # Fix of #25 - the natural join when columns are not specified
    .keys = paste0(intersect(colnames(x),colnames(y)), collapse = ", ")
  } else if (is.null(names(by))) {
    .keys = paste0(by, collapse = ", ")
  } else {
    .keys = paste0(
      ifelse(names(by) != "", paste(names(by),by,sep="="),by),
      collapse=", "
    )
  }
  .count.lhs = nrow(x)
  .count.rhs = nrow(y)
  out = joinFunction(x, y, by=by, ...)
  .count.out = nrow(out)
  out = out %>% p_set(mergedGraph) %>% .comment(.messages, .headline = .headline, .type="combine")
  return(out %>% .retrack())
  # tmpHead = .dataToNodesDf(.data,.headline,.isHeader=TRUE, .type = .type, .envir=.envir)
  # tmpBody = dplyr::bind_rows(lapply(.messages, function(m) .dataToNodesDf(.data,m,.isHeader=FALSE, .type = .type, .envir=.envir)))
  # .data = .writeMessagesToNode(.data, dplyr::bind_rows(tmpHead,tmpBody), .asOffshoot)
}

# TODO:
# should items excluded during joins be captured? Arguably yes but should this be LHS only or both sides?
# behaviour during inner_join will be items on LHS & RHS being excluded.
# behaviour during left_join will be only items on RHS being excluded.
# behaviour during right_join will be only items on LHS being excluded.
# anti_join is like an exclude_all - i.e. a deliberate exclusion and makes sense only to capture excluded LHS.
# semi_join is like an include_any (which are currently not captured)
# to get excluded on lhs it is a lhs %>% anti_join(rhs, by=by) unless the operation is an anti_join itself when it is lhs %>% semi_join(rhs, by=by)
# to get excluded on rhs it is rhs %>% anti_join(lhs, by = inversion_of_by).
# where inversion_of_by is a named vector with names and values swapped round.


#' Inner joins
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::inner_join()] for more details
#' on the underlying functions.
#'
#' @seealso dplyr::inner_join()
#'
#' @inheritParams dplyr::inner_join
#' @inheritDotParams dplyr::inner_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/inner-join-examples.R
p_inner_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Inner join by {.keys}") {
  .doJoin(dplyr::inner_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Left join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::left_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::left_join()
#'
#' @inheritParams dplyr::left_join
#' @inheritDotParams dplyr::left_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/left-join-examples.R
p_left_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Left join by {.keys}") {
  .doJoin(dplyr::left_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Right join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::right_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::right_join()
#'
#' @inheritParams dplyr::right_join
#' @inheritDotParams dplyr::right_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/full-join-examples.R
p_right_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Right join by {.keys}") {
  .doJoin(dplyr::right_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Full join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::full_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::full_join()
#'
#' @inheritParams dplyr::full_join
#' @inheritDotParams dplyr::full_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/full-join-examples.R
p_full_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in linked set"), .headline="Full join by {.keys}") {
  .doJoin(dplyr::full_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Semi join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::semi_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::semi_join()
#'
#' @inheritParams dplyr::semi_join
#' @inheritDotParams dplyr::semi_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/semi-join-examples.R
p_semi_join = function(x, y,  ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} in intersection"), .headline="Semi join by {.keys}") {
  .doJoin(dplyr::semi_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Anti join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::anti_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::anti_join()
#'
#' @inheritParams dplyr::anti_join
#' @inheritDotParams dplyr::anti_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/anti-join-examples.R
p_anti_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} not matched"), .headline="Semi join by {.keys}") {
  .doJoin(dplyr::anti_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

#' Nest join
#'
#' Mutating joins behave as `dplyr` joins, except the history graph of the two
#' sides of the joins is merged resulting in a tracked dataframe with the
#' history of both input dataframes. See [dplyr::nest_join()] for more details
#' on the underlying functions.
#' @seealso dplyr::nest_join()
#'
#' @inheritParams dplyr::nest_join
#' @inheritDotParams dplyr::nest_join
#' @param .messages a set of glue specs. The glue code can use any global
#'   variable, \{.keys\} for the joining columns, \{.count.lhs\},
#'   \{.count.rhs\}, \{.count.out\} for the input and output dataframes sizes
#'   respectively
#' @param .headline a glue spec. The glue code can use any global variable,
#'   \{.keys\} for the joining columns, \{.count.lhs\}, \{.count.rhs\},
#'   \{.count.out\} for the input and output dataframes sizes respectively
#' @return the join of the two dataframes with the history graph updated.
#'
#' @export
#' @example inst/examples/nest-join-examples.R
p_nest_join = function(x, y, ..., .messages = c("{.count.lhs} on LHS","{.count.rhs} on RHS","{.count.out} matched"), .headline="Nest join by {.keys}") {
  .doJoin(dplyr::nest_join, x=x, y=y, ..., .messages = .messages, .headline = .headline)
}

## Output operations ====

# TRUE if the whole document is being knitted.
# FALSE if running in chunk in RStudio, or not interactive, or
is_knitting = function() {
  isTRUE(getOption("knitr.in.progress"))
}

# TRUE is being knitted OR running in chunk in RStudio
# FALSE if not interactive or interactive but in console in RStudio
is_running_in_chunk = function() {
  # check for installation as dependency is optional
  if (rlang::is_installed("rstudioapi")) {
    is_knitting() ||
    isTRUE(try({
      rstudioapi::getActiveDocumentContext()$id != "#console" &
      rstudioapi::getActiveDocumentContext()$path %>% stringr::str_ends("Rmd")
    },silent = TRUE))
  } else {
    return(is_knitting())
  }
}

#' Flowchart output
#'
#' Generate a flowchart of the history of the dataframe(s), with all the tracked
#' data pipeline as stages in the flowchart. Multiple dataframes can be plotted
#' together in which case an attempt is made to determine which parts are
#' common.
#'
#' @param .data the tracked dataframe(s) either as a single dataframe or as a
#'   list of dataframes.
#' @param ... other parameters passed onto either `p_get_as_dot()`, notable ones are
#'   `fill` (background colour e.g. `lightgrey`), `fontsize` (in points),
#'   `colour` (font colour)
#' @param filename a file name which will be where the formatted flowcharts are
#'   saved. If no extension is specified the output formats are determined by
#'   the `formats` parameter.
#' @inheritParams save_dot
#' @param defaultToHTML if the correct output format is not easy to determine
#'   from the context, default providing `HTML` (TRUE) or to embedding the `PNG` (FALSE)
#'
#' @return the nature of the flowchart output depends on the context in which
#'   the function is called. It will be some form of browse-able html output if
#'   called from an interactive session or a `PNG`/`PDF` link if in `knitr` and
#'   knitting latex or word type outputs, if file name is specified the output
#'   will also be saved at the given location.
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% track() %>% comment(.tag = "step1") %>% filter(Species!="versicolor")
#' tmp %>% group_by(Species) %>% comment(.tag="step2") %>% flowchart()
p_flowchart = function(.data, filename = NULL, size = std_size$half, maxWidth = size$width, maxHeight = size$height, formats=c("dot","png","pdf","svg"), defaultToHTML = TRUE, landscape = size$rot!=0, ...) {

  # make sure .data is a list of dataframes
  if(is.data.frame(.data)) .data = list(.data)
  mergedGraph=.emptyGraph()

  if(!any(sapply(.data, .isTracked))) stop("None of the inputs is a tracked data frame. Did you forget to call dtrackr::track()")
  if(!all(sapply(.data, .isTracked))) rlang::warn("Some of the inputs are not tracked data frames. These will be ignored.", .frequency = "always")

  for(item in .data) {
    if ("trackr_df" %in% class(item)) {
      mergedGraph = .mergeGraphs(mergedGraph, item %>% p_get())
    }
  }

  # if we are knitting and the output is not HTML we will need the image
  # saved somewhere as a png and pdf. Also if output if github markdown
  # Also if we are viewing the image in the console and the user requested the png
  if(
    (is_knitting() && !(knitr::is_html_output()))
    ||
    (!is_knitting() && !is_running_in_chunk() && !defaultToHTML)

  ) {
    if (is.null(filename)) {
      # no file was given but for latex we need to convert to PDF anyway
      filename = tempfile(pattern = "flowchart_")
      formats = c("png","pdf")
    } else {
      formats = unique(c("png","pdf",formats))
    }
  }

  outgraph = mergedGraph %>% .graph2dot(...)

  if (!identical(filename,NULL)) {
    tmp = outgraph %>% save_dot(filename = filename, size=size,maxWidth=maxWidth, maxHeight=maxHeight, formats = formats, landscape = landscape, ...)
    svg = tmp$svg
  } else {
    svg = dot2svg(outgraph) %>% .scale_svg(maxWidth=maxWidth, maxHeight=maxHeight, landscape = landscape)
  }

  # Decide on the output format

  if (is_knitting()) {

    # warning(knitr::pandoc_to())

    # fmt <- rmarkdown::default_output_format(knitr::current_input())$name
    if (knitr::is_html_output(excludes = c("markdown"))) {
      return(htmltools::HTML(svg))

    } else if (knitr::is_latex_output()) {
      return(knitr::include_graphics(tmp$paths$png, auto_pdf = TRUE))

    } else if (knitr::pandoc_to(fmt = c("docx","odt"))) {
      return(knitr::include_graphics(tmp$paths$png))

    } else if (knitr::pandoc_to(fmt = c("markdown"))) {
      return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(data = charToRaw(svg), mime = "image/svg+xml"))))

    # } else if (knitr::pandoc_to(fmt = c("gfm"))) {
    #   # https://github.com/YourUserAccount/YourProject/blob/master/DirectoryPath/ReadMe.md
    #   # https://stackoverflow.com/questions/14494747/how-to-add-images-to-readme-md-on-github/48723190#48723190
    #   # There is no easy option here as README.Rmd file will create a github
    #   # README that is in one place, but when picked up by pkgdown it is moved
    #   # to another. For an image to appear in Github readme it must be hosted
    #   # and this is an issue for everything else.
    #   # https://github.com/r-lib/pkgdown/issues/280
    #   # suggests a location under man/figures should work for both but this is
    #   # too much trouble to set up for one readme file
    #   return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(file = tmp$paths$png, mime = "image/png"))))

    } else {
      # the user specified type.
      if (!defaultToHTML) {
        return(knitr::include_graphics(tmp$paths$png,auto_pdf = TRUE))
      } else {
        return(htmltools::HTML(svg))
      }

    }



  } else {

    # We are not kitting
    if(is_running_in_chunk()) {

      # We are in an RStudio chunk
      return(htmltools::HTML(svg))
    } else {

      # We are at the console probably
      if (defaultToHTML) {
        # print the html
        htmltools::HTML(svg) %>% htmltools::html_print()
      } else {
        # display the png
        getOption("viewer")(tmp$paths$png)
      }
    }
  }

}

#' DOT output
#'
#' (advance usage) outputs a `dtrackr` history graph as a DOT string for rendering with `Graphviz`
#'
#' @param .data the tracked dataframe
#' @param fill the default node fill colour
#' @param fontsize the default font size
#' @param colour the default font colour
#' @param ... not used
#'
#' @return a representation of the history graph in `Graphviz` dot format.
#' @export
#' @examples
#' library(dplyr)
#' library(dtrackr)
#'
#' tmp = iris %>% track() %>% comment(.tag = "step1") %>% filter(Species!="versicolor")
#' dot = tmp %>% group_by(Species) %>% comment(.tag="step2") %>% p_get_as_dot()
#' cat(dot)
p_get_as_dot = function(.data, fill="lightgrey", fontsize="8", colour="black", ...) {
  graph = .data %>% p_get()
  return(.graph2dot(graph))
}

.graph2dot = function(graph, fill="lightgrey", fontsize="8", colour="black", ...) {
  .rel = .id = .rank = .from = .hasOffshoot = nodeSpec = rankSpec = edgeSpec = NULL
  # THIS IS WHERE FORMATTING IS DEFINED
  nodesDf = graph$nodes %>% dplyr::mutate(
    .fillcolor= dplyr::case_when(.type=="summary"~"grey90",.type=="exclusion"~"grey80",TRUE~"white")
  ) %>% mutate(
    .hasOffshoot = .id %in% (graph$edges %>% dplyr::filter(.rel=="exclusion") %>% dplyr::pull(.from))
  )
  edgesDf = graph$edges %>% dplyr::mutate(
    .headport=ifelse(.rel=="exclusion","w","n"), #"n",
    .weight=ifelse(.rel=="exclusion","1","100"),
    .tailport= ifelse(.rel=="exclusion","e","s"), #"s",
    .colour="black",
    .id = dplyr::row_number()
  )

  outNode = nodesDf %>% dplyr::group_by(dplyr::desc(.id)) %>% dplyr::mutate(
    # TODO: this maybe not going to work completely
    # nodeSpec = glue::glue("'{.id}' [label=<{.label}>,group='{.strata}',fillcolor='{.fillcolor}'];")
    nodeSpec = # ifelse(.hasOffshoot,
      # glue::glue("'{.id}' [label=<{.label}>,group='{.strata}',fillcolor='{.fillcolor}'];\n'{.id}_e' [width='0', shape='point', style='invis'];"),
      glue::glue("'{.id}' [label=<{.label}>,group='{.strata}',fillcolor='{.fillcolor}'];")
    #)
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
    # TODO: there is a problem with Ortho layout and ports.
    # https://stackoverflow.com/questions/27504703/in-graphviz-how-do-i-align-an-edge-to-the-top-center-of-a-node
    dplyr::mutate(edgeSpec = glue::glue("'{.from}' -> '{.to}' [tailport='{.tailport}',weight='{.weight}']")) %>%
    dplyr::mutate(edgeSpec = # ifelse(.rel=="exclusion",
        # add in another invisible node on the same rank
        # glue::glue("'{.from}' -> '{.from}_e' [weight='{.weight}', dir='none']\n'{.from}_e' -> '{.to}' [tailport='{.tailport}',weight='{.weight}']",),
        glue::glue("'{.from}' -> '{.to}' [tailport='{.tailport}',weight='{.weight}']") #)
    ) %>%
    # dplyr::mutate(edgeSpec = glue::glue("'{.from}' -> '{.to}' [headport='{.headport}', tailport='{.tailport}',weight='{.weight}']")) %>% # Loses heads of arrows
    # dplyr::mutate(edgeSpec = glue::glue("'{.from}':'{.tailport}' -> '{.to}':'{.headport}' [weight='{.weight}']")) %>% # Loses heads of arrows
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

#' @inherit p_get
#' @export
history <- p_get

#' @inherit p_pause
#' @export
pause <- p_pause

#' @inherit p_resume
#' @inheritDotParams p_group_by
#' @export
resume <- p_resume

#' @inherit p_untrack
#' @export
untrack <- p_untrack

#' @inherit p_capture_exclusions
#' @export
capture_exclusions <- p_capture_exclusions

#' @inherit p_excluded
#' @export
excluded <- p_excluded

#' @inherit p_tagged
#' @export
tagged <- p_tagged

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

#' @inherit p_count_subgroup
#' @export
count_subgroup <- p_count_subgroup

#' @inherit p_comment
#' @export
comment <- p_comment

## Dplyr bindings ----

#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr arrange
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr nest_join
#' @importFrom dplyr slice
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @importFrom dplyr slice_min
#' @importFrom dplyr slice_max
#' @importFrom dplyr slice_sample
#' @importFrom dplyr intersect
#' @importFrom dplyr union
#' @importFrom dplyr union_all
#' @importFrom dplyr setdiff
#' @importFrom dplyr add_count
NULL


#' @inherit p_ungroup
#' @export
ungroup.trackr_df <- p_ungroup

#' @inherit p_summarise
#' @export
summarise.trackr_df <- p_summarise

#' @inherit p_mutate
#' @export
mutate.trackr_df <- p_mutate

#' @inherit p_transmute
#' @export
transmute.trackr_df <- p_transmute

#' @inherit p_select
#' @export
select.trackr_df <- p_select

#' @inherit p_relocate
#' @export
relocate.trackr_df <- p_relocate

#' @inherit p_rename
#' @export
rename.trackr_df <- p_rename

#' @inherit p_rename_with
#' @export
rename_with.trackr_df <- p_rename_with

#' @inherit p_arrange
#' @export
arrange.trackr_df <- p_arrange

#' @inherit p_pivot_wider
#' @export
pivot_wider.trackr_df <- p_pivot_wider

#' @inherit p_pivot_longer
#' @export
pivot_longer.trackr_df <- p_pivot_longer

#' @inherit p_group_by
#' @export
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
group_modify.trackr_df <- p_group_modify

#' @inherit p_inner_join
#' @export
inner_join.trackr_df <- p_inner_join

#' @inherit p_left_join
#' @export
left_join.trackr_df <- p_left_join

#' @inherit p_right_join
#' @export
right_join.trackr_df <- p_right_join

#' @inherit p_full_join
#' @export

full_join.trackr_df <- p_full_join

#' @inherit p_semi_join
#' @export
semi_join.trackr_df <- p_semi_join

#' @inherit p_anti_join
#' @export
anti_join.trackr_df <- p_anti_join

#' @inherit p_nest_join
#' @export
nest_join.trackr_df <- p_nest_join

#' @inherit p_slice
#' @export
slice.trackr_df <- p_slice

#' @inherit p_slice_head
#' @export
slice_head.trackr_df <- p_slice_head

#' @inherit p_slice_tail
#' @export
slice_tail.trackr_df <- p_slice_tail

#' @inherit p_slice_min
#' @export
slice_min.trackr_df <- p_slice_min

#' @inherit p_slice_max
#' @export
slice_max.trackr_df <- p_slice_max

#' @inherit p_slice_sample
#' @export
slice_sample.trackr_df <- p_slice_sample

#' @inherit p_intersect
#' @export
intersect.trackr_df <- p_intersect

#' @inherit p_union
#' @export
union.trackr_df <- p_union

#' @inherit p_union_all
#' @export
union_all.trackr_df <- p_union_all

#' @inherit p_setdiff
#' @export
setdiff.trackr_df <- p_setdiff

#' @inherit p_add_count
#' @export
add_count.trackr_df <- p_add_count

# re-exports:

# TODO: Not really sure why I need this to make the R CMD check work but it does seem to be important.
# this is something to do with stats::filter and dplyr::filter
#' @importFrom dplyr filter
#' @export
dplyr::filter

# complete override. These are not S3 methods:

#' @inherit p_bind_rows
#' @export
bind_rows <- p_bind_rows

#' @inherit p_bind_cols
#' @export
bind_cols <- p_bind_cols

#' @inherit p_add_tally
#' @export
add_tally <- p_add_tally
