#RPS data

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)


rps_raw <- read_xlsx('project_data/raw/rps_ces_nominal_aug_2024.xlsx',
                     sheet = 1,
                     skip = 24)

View(rps_raw)

# Filter data using regular expressions to find relevant policies
rps_small <- rps_raw |>
  select(-`Special Notes`) |>
  fill(State, .direction = 'down') |>
  filter(grepl('total rps', `RPS Tier or Carve Out`, ignore.case = TRUE) | 
           grepl('solar', `RPS Tier or Carve Out`, ignore.case = TRUE) & 
           grepl('carve|requirement', `RPS Tier or Carve Out`, ignore.case = TRUE) &
           !grepl('non', `RPS Tier or Carve Out`, ignore.case = TRUE))



# Pivot & slice 
rps_long <- rps_small |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year",
               values_to = "percent") |>
  filter(year >= 2014, year <= 2023) |>
  mutate(year = as.integer(year)) |>
  clean_names() |>
  rename(policy = rps_tier_or_carve_out)

View(rps_long)

# Create binaries and select relevant policies and pivot data to (state X year)
# Created binary variables to flag whether there were active policies in states
  # in addition to any targets to help differentiate those states with RPS targets
  # and without

policy_long <- rps_long |>
  mutate(rps_target = ifelse(grepl('total rps', policy, ignore.case = TRUE) &
                               !is.na(percent), percent, NA_real_),
         rps_active = ifelse(rps_target > 0 & !is.na(rps_target), 1, 0),
         solar_active = ifelse(grepl('solar', policy, ignore.case = TRUE) &
                                 percent > 0, 1, 0))


policy_clean <- policy_long |>
  group_by(state, year) |>
  summarise(rps_target = max(rps_target, na.rm = TRUE),
            rps_active = max(rps_active),
            solar_active = max(solar_active))



