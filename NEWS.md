# NEWS

# dtrackr 0.4.0

* Updating release following `dplyr` version 1.1.0 an changes in new versions of
`tidyr`
* More use of @inheritDotParams to decouple from upstream APIs
* fix for #33
* Fix for RSVG scaling inconsistency / rotation issues.
* TODO: (next version) exclusions in joins
* TODO: (next version) better support for long format tables
* TODO: (future version) column level tracking (prototyped in 0.2.5)

# dtrackr 0.3.0

* Updating release in preparation for `dplyr` version 1.1.0
* Fix problems detected with URLs and DOI.
* Updated URL: https://terminological.github.io/dtrackr to 
https://terminological.github.io/dtrackr/ in readme README.md 
(fixed missing trailing slash)
* Manually fixed auto-generated codecov badge.
* Removed project level Zenodo DOI as triggering a invalid DOI from CRAN
* Updated maintainer email to longer term stable email address. URLs in 
* DESCRIPTION updated to point to github source.
* There are some spelling issues that arise from documentation inherited from 
`dplyr`and `tidyr` equivalent functions, that would need to be fixed upstream.
* Documentation inherited from dplyr may be inconsistent until dplyr release.

# dtrackr 0.2.5

* updates following JOSS review: changes in documentation have resulted in
additional `Suggests` dependencies on CRAN packages.
* bug fix for group_by not checking pause status before warning that pause required.
* documentation improvements - full review of all documentation. Additional
context to function reference to draw attention to supported dplyr operations 
* added `nest_join.tracked_df` S3 method, added `slice*.tracked_df` functions, and 
missing set operations functions. 
* re-factored mutate style operations, regression tested
* automated testing for new methods, and full testing for slice, set ops, and mutate style
functions with new example code.
* expanded test coverage ( `covr::package_coverage(type="all"` ):
`dtrackr` Coverage: 83.60%,
`R/dot.R`: 78.12%,
`R/dtrackr.R`: 84.08%) mostly through examples
* improved error handling and reporting in mutate functions.
* bug #25 & #26 fixes (see github) 
* Logo and documentation fixes and improvements.
* outstanding issues: 
* re-use of documentation results in examples tests running multiple times.

# dtrackr 0.2.4

* second round fixes for CRAN. Improved examples in documentation. 
* Print and plot S3 methods for history graph. 
* Vignette fixes. 
* Non breaking changes to API so bumping to new minor version for CRAN submission.

# dtrackr 0.2.3

* documentation updates & JOSS paper
* fix for fix for multiple names join columns bug.
* `ungroup()`  change to fit with `dplyr::ungroup` API.
* fixed url redirects, DESCRIPTION file issues and switched to LICENSE file template, for CRAN submission.

# dtrackr 0.2.2

* new feature of subgroup counts for determining size of subgroups in grouped (or un-grouped) flowchart.
* fix for multiple names join columns bug.
* bug fixes and extended github workflows.

# dtrackr 0.2.1

* minor bug fixes for grouping
* support for tagging pipeline with a piece of data for later retrieval to support e.g. for counts within abstract
* support for tidyselect syntax in group by

# dtrackr 0.2.0

* track excluded items with debugging info
* allow across syntax in most situations (except group_by)
* support pausing and unpausing of the dataframe tracking
* preventing massive group_by groups if large numbers of very small groups are selected (e.g. part of a group_by() %>% mutate())
* switch warnings to rlang::warn etc.

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
