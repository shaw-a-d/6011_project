# Visualizations

library(maps)
library(dplyr)
library(ggplot2)
library(geofacet)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(viridis)

# Modify data ---------------------------------------------------------------------

# Calculate absolute change in capacity 
solar_growth <- solar_panel_final |>
  group_by(state, year) |>
  filter(year %in% c(2014, 2023)) |>
  summarise(total_mw = sum(total_mw, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year,
              values_from =  total_mw,
              names_prefix = "mw_") |>
  mutate(mw_growth = mw_2023 - mw_2014)



mean_rps <- solar_panel_final |>
  group_by(state) |>
  summarise(mean_rps = mean(rps_target, na.rm = TRUE), .groups = "drop")

solar_growth <- solar_growth |>
  left_join(mean_rps, by = "state")


# Map ---------------------------------------------------------------------

options(tigris_use_cache = TRUE)

states_sf <- states(cb = TRUE, resolution = "20m") |>
  filter(!STUSPS %in% c("AK", "HI", "PR"))

solar_growth_sf <- solar_growth |>
  rename(STUSPS = state)


states_merged <- left_join(states_sf, solar_growth_sf, by = "STUSPS")

states_merge_proj <- st_transform(states_merged, crs = 5070)


ggplot(states_merge_proj) +
  geom_sf(aes(fill = mw_growth), color = "white") +
  geom_sf_text(aes(label = paste0(STUSPS, "\n", round(mean_rps*100, 1), "%")), size = 2.5,
               color = "black") +
  scale_fill_gradient(
    name = "Total Solar Capacity\nGrowth (in mW)",
    low = "#f0f0f0",
    high = "#1f78b4"
  ) +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
  labs(title = "Solar Growth vs. Avg RPS Target (2014-2023)")


  