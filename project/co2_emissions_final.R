# CO2 emissions by state 

library(janitor)
library(dplyr)

co2_raw <- read_xlsx('project_data/raw/seds_co2.xlsx',
                     sheet = 4, skip = 2)


co2_clean <- co2_raw |>
  select(State, matches("^201[4-9]$|^202[0-3]$")) |>
  filter(State != "DC" & State != "US") |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year") |>
  mutate(year = as.integer(year)) |>
  rename(state = State, mil_metric_tons = value)


