#join test 

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)


file_list <- list.files('project_data/raw/generator', full.names = TRUE)

solar_state_year <- data.frame()

for (file in file_list) {
  year <- as.numeric(gsub("\\D", "", basename(file)))
  
  df <- read_xlsx(file, sheet = 1) |>
    clean_names()
  
  df_solar <- df |>
    filter(technology == 'Solar Photovoltaic') |>
    group_by(state) |>
    reframe(total_mw = sum(nameplate_capacity_mw)) |>
    mutate(year = year)
  
  solar_state_year <- bind_rows(solar_state_year, df_solar)
}


rps_raw <- read_xlsx('project_data/raw/rps_ces_nominal_aug_2024.xlsx',
                     sheet = 1,
                     skip = 24)

rps_small <- rps_raw |>
  select(-`Special Notes`) |>
  fill(State, .direction = 'down') |>
  filter(grepl('total rps', `RPS Tier or Carve Out`, ignore.case = TRUE) | 
           grepl('solar', `RPS Tier or Carve Out`, ignore.case = TRUE) & 
           grepl('carve|requirement', `RPS Tier or Carve Out`, ignore.case = TRUE) &
           !grepl('non', `RPS Tier or Carve Out`, ignore.case = TRUE))

rps_long <- rps_small |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year",
               values_to = "percent") |>
  filter(year >= 2014, year <= 2023) |>
  mutate(year = as.integer(year)) |>
  clean_names() |>
  rename(policy = rps_tier_or_carve_out)

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


analysis_df <- solar_state_year |>
  left_join(policy_clean,
            by = c('state', 'year'))

df <- read_xlsx('data/raw/party_control/legis_control_2014.xlsx', skip = 1)

year <- 2014

party_control <- df |>
  clean_names() |>
  slice(1:50) |>
  mutate(year = year) |>
  mutate(state = state2abbr(state)) |>
  select("state", "year", "state_control")

megatron <- analysis_df |>
  left_join(party_control,
            by = c('state', 'year'))

View(megatron)

