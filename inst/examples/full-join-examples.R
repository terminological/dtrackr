library(dplyr)
library(dtrackr)
# Joins across data sets

# example data uses the dplyr starways data
people = starwars %>% select(-films, -vehicles, -starships)
films = starwars %>% select(name,films) %>% tidyr::unnest(cols = c(films))

lhs = people %>% track() %>% comment("People df {.total}")
rhs = films %>% track() %>% comment("Films df {.total}") %>%
  comment("a test comment")

# Full join
join = lhs %>% full_join(rhs, by="name", multiple = "all") %>% comment("joined {.total}")
# See what the history of the graph is:
join %>% history()
nrow(join)
# Display the tracked graph (not run in examples)
# join %>% flowchart()

