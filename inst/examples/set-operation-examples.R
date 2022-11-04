library(dplyr)
library(dtrackr)

# Set operations
people = starwars %>% select(-films, -vehicles, -starships)
chrs = people %>% track("start")

lhs = chrs %>% include_any(
  species == "Human" ~ "{.included} humans",
  species == "Droid" ~ "{.included} droids"
)

# these are different subsets of the same data
rhs = chrs %>% include_any(
  species == "Human" ~ "{.included} humans",
  species == "Gungan" ~ "{.included} gungans"
) %>% comment("{.count} gungans & humans")


# Unions
set = bind_rows(lhs,rhs) %>% comment("{.count} 2*human,droids and gungans")
# display the history of the result:
set %>% history()
nrow(set)
# not run - display the flowchart:
# set %>% flowchart()

set = union(lhs,rhs) %>% comment("{.count} human,droids and gungans")
# display the history of the result:
set %>% history()
nrow(set)
# not run - display the flowchart:
# set %>% flowchart()

set = union_all(lhs,rhs) %>% comment("{.count} 2*human,droids and gungans")
# display the history of the result:
set %>% history()
nrow(set)
# not run - display the flowchart:
# set %>% flowchart()

# Intersections and differences

set = setdiff(lhs,rhs) %>% comment("{.count} droids and gungans")
# display the history of the result:
set %>% history()
nrow(set)
# not run - display the flowchart:
# set %>% flowchart()

set = intersect(lhs,rhs) %>% comment("{.count} humans")
# display the history of the result:
set %>% history()
nrow(set)
# not run - display the flowchart:
# set %>% flowchart()
