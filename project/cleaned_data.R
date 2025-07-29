#join test 

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usdata)


# Solar generator import -----------------------------------------------

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

View(df_solar)
View(solar_state_year)

# RPS Data Cleaning/Import ------------------------------------------------


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


# Pivot data to panel data (state x year)
rps_long <- rps_small |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year",
               values_to = "percent") |>
  filter(year >= 2014, year <= 2023) |>
  mutate(year = as.integer(year)) |>
  clean_names() |>
  rename(policy = rps_tier_or_carve_out)

# Create binary variables as policy indicators
policy_long <- rps_long |>
  mutate(rps_target = ifelse(grepl('total rps', policy, ignore.case = TRUE) &
                               !is.na(percent), percent, NA_real_),
         rps_active = ifelse(rps_target > 0 & !is.na(rps_target), 1, 0),
         solar_active = ifelse(grepl('solar', policy, ignore.case = TRUE) &
                                 percent > 0, 1, 0))

# Aggregate data to relevant variables
policy_clean <- policy_long |>
  group_by(state, year) |>
  summarize(rps_target = max(rps_target, na.rm = TRUE),
            rps_active = max(rps_active),
            solar_active = max(solar_active))

View(policy_clean)


# Retail Electricity Price ------------------------------------------------

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
  rename(avg_retail_price_kwh = value) |>
  filter(year <= 2023)


# Political Control -------------------------------------------------------

file_list_party <- list.files('project_data/raw/party_control', full.names = TRUE)

df_party <- data.frame()

for (file in file_list_party) {
  year <- as.numeric(gsub("\\D", "", basename(file)))
  
  df <- read_xlsx(file, skip = 1) |>
    clean_names() |>
    mutate(year = year) |>
    select(state, year, state_control) |>
    slice(1:50)
  
  df_party <- bind_rows(df_party, df)
  
}


# Clean state names and outliers

## Removed "*" from state names as footnotes not directly relevant to this analysis
## "N/A" listed are all from Nebraska as they have 'unicameral' state congress. However,
## Nebraska's has been consistently "Rep" since 1999. 

library(usdata)


df_party$state <- gsub("\\*", "", df_party$state)
df_party$state_control <- df_party$state_control |> 
  gsub("\\*", "", x = _) |>
  gsub("N/A", "Rep", x = _)

df_party_clean <- df_party |> 
  mutate(state = state2abbr(state))



# Join data sets -----------------------------------------------------------

analysis_df <- solar_state_year |>
  full_join(policy_clean, by = c('state', 'year')) |>
  full_join(df_party_clean, by = c('state', 'year')) |>
  full_join(df_elec_clean, by = c('state', 'year'))

View(analysis_df)

analysis_df[204, ]
