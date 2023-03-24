library(dplyr)
library(dtrackr)

# In this example the iris dataframe is resampled 100 times with replacement
# within each group and the
iris %>%
  track() %>%
  group_by(Species) %>%
  slice_sample(n=100, replace=TRUE,
               .messages="{.count.out} / {.count.in} = {n}",
               .headline="100 {Species}") %>%
  history()
