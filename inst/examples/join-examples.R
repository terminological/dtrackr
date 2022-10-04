library(dplyr)
library(tidyr)
library(dtrackr)
# Joins across data sets

# example data uses the dplyr starways data
people = starwars %>% select(-films, -vehicles, -starships)
films = starwars %>% select(name,films) %>% unnest(cols = c(films))

lhs = people %>% track() %>% comment("People df {.total}")
rhs = films %>% track() %>% comment("Films df {.total}") %>%
  comment("a test comment")

# Inner join
join = lhs %>% inner_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history() %>% print()
nrow(join)
# Display the tracked graph (not run in examples)
# join %>% flowchart()

# Left join
join = lhs %>% left_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)

# Full join
join = lhs %>% full_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)

# Semi join
join = lhs %>% semi_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)

# Anti join
join = lhs %>% anti_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)

# Nest join
join = lhs %>% nest_join(rhs, by="name") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)







