library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

file_list_party <- list.files('project_data/raw/party_control', full.names = TRUE)

df_party <- data.frame()

# Import ------------------------------------------------------------------

for (file in file_list_party) {
  year <- as.numeric(gsub("\\D", "", basename(file)))
  
  df <- read_xlsx(file, skip = 1) |>
    clean_names() |>
    mutate(year = year) |>
    select(state, year, state_control) |>
    slice(1:50)
  
  df_party <- bind_rows(df_party, df)
  
}


# Clean state names and outliers ----------------------------------------------

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



