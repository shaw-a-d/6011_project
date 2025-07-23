setwd('~/Documents/Academic/University/GATech/Summer 2025/6011 Coding & Analysis')

install.packages('maps')
install.packages('janitor')
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(maps)

generator_2022 <- read_xlsx('~/Documents/Academic/University/GATech/Summer 2025/6011 Coding & Analysis/data/raw/eia8602022/3_1_Generator_Y2022.xlsx',
                            sheet=1)
generator_2022 <- clean_names(generator_2022)

colnames(generator_2022)


solardf <- generator_2022 |>
  filter((technology == 'Solar Photovoltaic') |
           (technology == 'Solar Thermal without Energy Storage') |
           (technology == 'Solar Thermal with Energy Storage'))




solardf <- solardf |>
  filter(operating_year >= 2008 & operating_year <= 2023)



solar_state_year <- solardf |>
  group_by(state, operating_year) |>
  reframe(nameplate_capacity_mw)

View(solar_state_year)

solar2022 <- solar_state_year |>
  filter(operating_year == 2022)

View(solar2022)


state_abbrev <- data.frame(
  state = state.abb,
  state_name = tolower(state.name)
)


View(solar_2022_named)


solar_2022_named <- solar2022 |>
  left_join(state_abbrev, by = 'state')

solar_2022_summary <- solar_2022_named |>
  group_by(state_name) |>
  summarise(nameplate_capacity_mw = sum(nameplate_capacity_mw, na.rm=TRUE))

View(solar_2022_summary)


states_map <- map_data('state')

View(states_map)
View(solar_2022_named)

map_data_joined <- left_join(states_map, solar_2022_summary, by = c('region' = 'state_name'))


View(map_data_joined)


ggplot(map_data_joined, aes(x=long, y = lat, group = group, fill = nameplate_capacity_mw)) +
  geom_polygon(color = 'white') +
  coord_fixed(1.3) +
  scale_fill_gradient(low = 'lightyellow', high='darkred', na.value='gray90') +
  labs(title = 'Solar Capacity by State (2022)',
       fill = 'Total MW') +
  theme_void()
