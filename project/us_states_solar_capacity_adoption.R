# Andrew Shaw
# 6011 R Coding & Analysis Final
# final code | compiled scripts

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usdata)
library(tibble)
library(maps)


# Create full (state X year) panel ----------------------------------------
# Used as base to left_join other data sets to ensure that all combinations
# are retained as some states had no reported solar capacity or missing policy data.

grid <- expand.grid(
  state = state.abb,
  year = 2014:2023
) |> as_tibble()


# Solar generator capacity ------------------------------------------------


file_list <- list.files('project_data/raw/generator', full.names = TRUE)

solar_clean <- data.frame()

for (file in file_list) {
  year <- as.numeric(gsub("\\D", "", basename(file)))
  
  df <- read_xlsx(file, sheet = 1) |>
    clean_names()
  
  df_solar <- df |>
    filter(technology == 'Solar Photovoltaic') |>
    group_by(state) |>
    reframe(total_mw = sum(nameplate_capacity_mw)) |>
    mutate(year = year) |>
    filter(state != 'DC')
  
  solar_clean <- bind_rows(solar_clean, df_solar)
}



# RPS data ----------------------------------------------------------------

rps_raw <- read_xlsx('project_data/raw/rps_ces_nominal_aug_2024.xlsx',
                     sheet = 1,
                     skip = 24)

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


# Political party control -------------------------------------------------

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

# Clean state names

# Removed "*" from state names as footnotes not directly relevant to this analysis
# "N/A" listed are all from Nebraska as they have 'unicameral' state congress. However,
# Nebraska's has been consistently "Rep" since 1999.

df_party$state <- gsub("\\*", "", df_party$state)
df_party$state_control <- df_party$state_control |> 
  gsub("\\*", "", x = _) |>
  gsub("N/A", "Rep", x = _)

df_party_clean <- df_party |> 
  mutate(state = state2abbr(state))


# Average retail electricity prices ---------------------------------------

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


# CO2 emissions by State --------------------------------------------------

co2_raw <- read_xlsx('project_data/raw/seds_co2.xlsx',
                     sheet = 4, skip = 2)


co2_clean <- co2_raw |>
  select(State, matches("^201[4-9]$|^202[0-3]$")) |>
  filter(State != "DC" & State != "US") |>
  pivot_longer(cols = matches("[0-9]{4}"),
               names_to = "year") |>
  mutate(year = as.integer(year)) |>
  rename(state = State, mil_metric_tons = value)





# Merge -------------------------------------------------------------------

solar_panel_merge <- grid |>
  left_join(solar_clean, by = c('state', 'year')) |>
  left_join(policy_clean, by = c('state', 'year')) |>
  left_join(df_party_clean, by = c('state', 'year')) |>
  left_join(df_elec_clean, by = c('state', 'year')) |>
  left_join(co2_clean, by = c('state', 'year'))


View(solar_panel_merge)

# Account for non-binding RPS states --------------------------------------
# There are 13 states that do not have binding RPS standards
# Some carve-outs to exists for solar and indeed solar capacity does exists
# in such states. For the purposes of this analysis, NA's will be adjusted
# to 0 for a more insightful view of the impacts of renewable standards in
# building out solar capacity

solar_panel_final <- solar_panel_merge |>
  mutate(
    rps_active = replace_na(rps_active, 0), 
    rps_target = replace_na(rps_target, 0),
    solar_active = replace_na(solar_active,0),
    total_mw = replace_na(total_mw, 0))


# Party control as factor variable ----------------------------------------

solar_panel_final$state_control <- factor(solar_panel_final$state_control,
                                          levels = c("Dem", "Divided", "Rep")) |>
  relevel(solar_panel_final$state_control, ref = "Rep")



# Model -------------------------------------------------------------------

model1 <- lm(total_mw ~ rps_target + avg_retail_price_kwh + mil_metric_tons +
               state_control, data = solar_panel_final)

summary(model1)


# RPS only subset ---------------------------------------------------------
# Given the vast amount of states that do not have binding RPS requirements
# viewing the data with only those that do can remove some of the noise to
# give a better picture of factors contributing towards capacity growth

solar_rps_only <- solar_panel_final |>
  filter(rps_active ==1)

model_rps_only <- lm(total_mw ~ avg_retail_price_kwh + mil_metric_tons +
                       state_control + rps_target, data = solar_rps_only)

summary(model_rps_only)


# Model yearly fixed effects ----------------------------------------------
solar_panel_final$year <- as.factor(solar_panel_final$year)

model_fixed <- lm(total_mw ~ rps_target + avg_retail_price_kwh + mil_metric_tons +
                    state_control + year, data = solar_panel_final)

summary(model_fixed)


# Logarithmic capacity model ----------------------------------------------
solar_log_df <- solar_panel_final |>
  filter(total_mw > 0)

log_model <- lm(log(total_mw) ~ rps_target + avg_retail_price_kwh + state_control +
                  year + mil_metric_tons, data = solar_log_df)

summary(log_model)


# State control interaction -----------------------------------------------
party_control_model <- lm(log(total_mw) ~ rps_target * state_control + year +
                            avg_retail_price_kwh + mil_metric_tons,
                          data = solar_log_df)

summary(party_control_model)

