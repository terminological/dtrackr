library(dplyr)
library(dtrackr)

# mutate and other functions are unitary operations that generally change
# the structure but not size of a dataframe. In dtrackr these are by ignored
# by default but we can change that so that their behaviour is obvious.

# mutate
# In this example we compare the column names of the input and the
# output to identify the new columns created by the mutate operation as
# the `.new_cols` variable
iris %>%
  track() %>%
  mutate(extra_col = NA_real_,
         .messages="{.new_cols}",
         .headline="Extra columns from mutate:") %>%
  history()

# transmute
# Here we do the same for a transmute()
iris %>%
  track() %>%
  group_by(Species) %>%
  transmute(
    sepal.w = Sepal.Width-1,
    sepal.l = Sepal.Length+1,
    .messages="{.new_cols}",
    .headline="New columns from transmute:") %>%
  history()

# select
# The output of the select verb (here using tidyselect syntax) can be captured
# and here all column names are being reported with the .cols variable.
iris %>%
  track() %>%
  group_by(Species) %>%
  select(
    tidyselect::starts_with("Sepal"),
    .messages="{.cols}",
    .headline="Output columns from select:") %>%
  history()

# relocate, this shows how the columns can be reordered
iris %>%
  track() %>%
  group_by(Species) %>%
  relocate(
    tidyselect::starts_with("Sepal"),
    .after=Species,
    .messages="{.cols}",
    .headline="Order of columns from relocate:") %>%
  history()

# rename can show us which columns are new and which have been
# removed (with .dropped_cols)
iris %>%
  track() %>%
  group_by(Species) %>%
  rename(
    Stamen.Width = Sepal.Width,
    Stamen.Length = Sepal.Length,
    .messages=c("added {.new_cols}","dropped {.dropped_cols}"),
    .headline="Renamed columns:") %>%
  history()

# rename_with uses a function
iris %>%
  track() %>%
  group_by(Species) %>%
  rename_with(
    .fn = toupper,
    .messages="{.new_cols}",
    .headline="Renamed columns:") %>%
  history()

# arrange
# In this case we sort the data descending and show the first value
# is the same as the maximum value.
iris %>%
  track() %>%
  arrange(
    desc(Petal.Width),
    .messages="{.count} items, columns: {.cols}",
    .headline="Reordered dataframe:") %>%
  history()

# add_count
# adding in a count or tally column as a new column
iris %>%
  track() %>%
  add_count(Species, name="new_count_total",
            .messages="{.new_cols}",
            # .messages="{.cols}",
            .headline="New columns from add_count:") %>%
  history()

# add_tally
iris %>%
  track() %>%
  group_by(Species) %>%
  dtrackr::add_tally(wt=Petal.Length, name="new_tally_total",
            .messages="{.new_cols}",
            .headline="New columns from add_tally:") %>%
  history()



