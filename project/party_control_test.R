library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

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

View(df_party)

party_control <- df |>
  clean_names() |>
  slice(1:50) |>
  mutate(year = year) |>
  mutate(state = state2abbr(state)) |>
  select("state", "year", "state_control")

View(party_control)

install.packages('usdata')
library(usdata)
