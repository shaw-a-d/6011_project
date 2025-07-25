#import data test


library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)

setwd("~/Documents/GitHub/6011_coding/6011_project")

file_list <- list.files("project_data/generator", full.names = TRUE)

solar_state_year <- data.frame()

for (file in file_list) {
  year <- as.numeric(gsub("\\D", "", basename(file)))

  df <- read_xlsx(file, sheet = 1) |>
    clean_names()

  df_solar <- df |>
    filter(technology == "Solar Photovoltaic") |>
    group_by(state) |>
    reframe(total_mw = sum(nameplate_capacity_mw)) |>
    mutate(year = year)

  solar_state_year <- bind_rows(solar_state_year, df_solar)
}

View(solar_state_year)

ggplot(solar_state_year, aes(x = year, y = total_mw, color = state)) +
  geom_line()
