library(dplyr)
library(dtrackr)

# mutate and other functions are unitary operations that generally change
# the structure but not size of a dataframe. In dtrackr these are by ignored
# by default but we can change that so that their behaviour is obvious.

# In this example we compare the column names of the input and the
# output to identify the new columns created by the transmute operation as
# the `.new_cols` variable
# Here we do the same for a transmute()
iris %>%
  track() %>%
  group_by(Species, .add=TRUE) %>%
  transmute(
    sepal.w = Sepal.Width-1,
    sepal.l = Sepal.Length+1,
    .messages="{.new_cols}",
    .headline="New columns from transmute:") %>%
  history()

