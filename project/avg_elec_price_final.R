# EIA Electricity Prices

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usdata)


df_elec <- read_csv("project_data/raw/Average_retail_price_of_electricity.csv", skip = 4,
                    show_col_types = FALSE) |>
  drop_na()

# Remove prefix from state column
df_elec$description <- df_elec$description |>
  gsub("^All sectors :\\s*", "", x = _) |>
  sort(x = _)

# Filter and abbr.   
df_elec <- df_elec |>
  rename(state = description) |>
  filter(state != "District Of Columbia") |>
  select(-units, -"source key") |>
  mutate(state = state2abbr(state))

# Pivot to state x year format
df_elec_clean <- df_elec |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year") |>
  mutate(year = as.integer(year)) |>
  filter(year <= 2023) |>
  rename(avg_retail_price_kwh = value)

