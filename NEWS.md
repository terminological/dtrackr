# NEWS

# trackr 0.1.0.9000 - pre-release github version

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

# Future developments

* develop flexible formatting
* Track md5 of dataframes and compare history graphs for divergences
* other dplyr functions
* test working with dbplyr data frames
