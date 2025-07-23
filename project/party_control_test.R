library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('~/Documents/Academic/University/GATech/Summer 2025/6011 Coding & Analysis')

df <- read_xlsx('data/raw/party_control/legis_control_2014.xlsx', skip = 1)

year <- 2014

party_control <- df |>
  clean_names() |>
  slice(1:50) |>
  mutate(year = year) |>
  mutate(state = state2abbr(state)) |>
  select("state", "year", "state_control")

View(party_control)

install.packages('usdata')
library(usdata)
