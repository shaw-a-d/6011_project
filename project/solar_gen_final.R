# Solar generator capacity (state x year)

library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)


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

