library(tidyverse)
library(gt)

lims <- tribble(
  ~limit, ~year25, ~year26,
  "Employee", 23500, 24500,
  "Catch-up 50+", 7500, 8000,
  "Catch-up 60-63", 11250, 11250
)

gt(lims) %>%
  tab_header("401K Contribution Limits") %>%
  fmt_currency(year25:year26, decimals = 0) %>%
  cols_label(
    limit = "Limit",
    year25 = "2025",
    year26 = "2026"
  ) %>%
  tab_footnote(
    "Only available if allowed by specific 401K Plan. Thomasville plan doesn't appear to allow super catch-up.",
    cells_body(columns = limit, rows = limit == "Catch-up 60-63")
  ) %>%
  tab_footnote(
    "For high earners, catch-ups must be Roth beginning in 2026",
    cells_body(columns = year26, rows = limit != "Employee")
  ) %>%
  data_color(
    colors = "yellow",
    columns = year26,
    rows = limit != "Employee"
  )
