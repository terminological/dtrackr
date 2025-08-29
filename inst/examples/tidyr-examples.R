library(dplyr)
library(dtrackr)

starwars %>%
  track() %>%
  tidyr::unnest(starships, keep_empty = TRUE) %>%
  tidyr::nest(world_data = c(-homeworld)) %>%
  history()

# There is a problem with `tidyr::unnest` that means if you want to override the
# `.messages` option at the moment it will most likely fail. Forcing the use of
# the specific `dtrackr::p_unnest` version solves this problem, until hopefully it is
# resolved in `tidyr`:
starwars %>%
  track() %>%
  p_unnest(
    films,
    .messages = c("{.count.in} characters", "{.count.out} appearances")
  ) %>%
  dplyr::group_by(gender) %>%
  tidyr::nest(
    people = c(-gender, -species, -homeworld),
    .messages = c("{.count.in} appearances", "{.count.out} planets")
  ) %>%
  status() %>%
  history()

# This example includes pivoting and nesting. The CMS patient care data
# has multiple tests per institution in a long format, and observed /
# denominator types. Firstly we pivot the data to allow us to easily calculate
# a total percentage for each institution. This is duplicated for every test
# so we nest the tests to get to one row per institution. Those institutions
# with invalid scores are excluded.
cms_history = tidyr::cms_patient_care %>%
  track() %>%
  tidyr::pivot_wider(names_from = type, values_from = score) %>%
  dplyr::mutate(
    percentage = sum(observed) / sum(denominator) * 100,
    .by = c(ccn, facility_name)
  ) %>%
  tidyr::nest(
    results = c(measure_abbr, observed, denominator),
    .messages = c("{.count.in} test results", "{.count.out} facilities")
  ) %>%
  exclude_all(
    percentage > 100 ~ "{.excluded} facilities with anomalous percentages",
    na.rm = TRUE
  )

print(cms_history %>% dtrackr::history())

# not run in examples:
if (interactive()) {
  cms_history %>% flowchart()
}
