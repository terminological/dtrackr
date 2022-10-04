library(dplyr)
library(tidyr)
library(dtrackr)

# an arbitrary 50 items from the iris dataframe is selected. The
# history is tracked
iris %>% track() %>% slice(51:100) %>% history()

# the first 50% of the data frame, is taken and the history tracked
iris %>% track() %>% group_by(Species) %>%
  slice_head(prop=0.5,.messages="{.count.out} / {.count.in}",
             .headline="First {sprintf('%1.0f',prop*100)}%") %>%
  history()

# The last 100 items:
iris %>% track() %>% group_by(Species) %>%
  slice_tail(n=100,.messages="{.count.out} / {.count.in}",
             .headline="Last {n}") %>%
  history()

# Subset the data by the maximum of a given value
iris %>% track() %>% group_by(Species) %>%
  slice_max(prop=0.5, order_by = Sepal.Width,
            .messages="{.count.out} / {.count.in} = {prop} (with ties)",
            .headline="Widest 50% Sepals") %>%
  history()


# The narrowest 25% of the iris data set by group can be calculated in the
# slice_min() function. Recording this is a matter of tracking and
# using glue specs.
iris %>%
  track() %>%
  group_by(Species) %>%
  slice_min(prop=0.25, order_by = Sepal.Width,
            .messages="{.count.out} / {.count.in} (with ties)",
            .headline="narrowest {sprintf('%1.0f',prop*100)}% {Species}") %>%
  history()



# In this example the iris dataframe is resampled 100 times with replacement
# within each group and the
iris %>%
  track() %>%
  group_by(Species) %>%
  slice_sample(n=100, replace=TRUE,
               .messages="{.count.out} / {.count.in} = {n}",
               .headline="100 {Species}") %>%
  history()
