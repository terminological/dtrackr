# NEWS

# dtrackr 0.1.0.9000 - pre-release github version

* Initial package features complete
* Dplyr like for like functionality supporting - 
add_count, add_tally, anti_join, arrange, bind_rows, comment, 
distinct, exclude_all, filter, full_join, group_by, group_modify,
inner_join, left_join, mutate, pivot_longer, pivot_wider, relocate,
rename, rename_with, right_join, select, semi_join, summarise, 
transmute, ungroup
* add a comment into the history (stratified by grouping),
* add a status into the history (stratified by grouping),
* apply exclusion and inclusion filters including stratified commentary
* merge histories of joining data frames
* export history as dot graphviz graph
* render history graph to SVG, PNG, PDF, DOT or PS formats

# dtrackr 0.2.0

* track excluded items with debugging info
* allow across syntax in most situations (except group_by)
* support pausing and unpausing of the dataframe tracking
* preventing massive group_by groups if large numbers of very small groups are selected (e.g. part of a group_by() %>% mutate())
* switch warnings to rlang::warn etc.

# dtrackr 0.2.1

* minor bug fixes for grouping
* support for tagging pipeline with a piece of data for later retrieval to support e.g. for counts within abstract
* support for tidyselect syntax in group by

# dtrackr 0.2.2

* new feature of subgroup counts for determining size of subgroups in grouped (or ungrouped) flowchart.
* fix for multiple names join columns bug.
* bug fixes and extended github workflows.

# dtrackr 0.2.3

* documentation updates & JOSS paper
* fix for fix for multiple names join columns bug.
* ungroup()  change to fit with dplyr::ungroup API.
* fixed url redirects, DESCRIPTION file issues and switched to LICENSE file template, for CRAN submission.

# dtrackr 0.2.4

* second round fixes for CRAN. Improved examples in documentation. 
* Print and plot S3 methods for history graph. 
* Vignette fixes. 
* Non breaking changes to API so bumping to new minor version for CRAN submission.
* (Version to stay unstable until CRAN submission complete)

