### create city shapefile

library(tidyverse)
library(tidycensus)
library(sf)

places <- get_decennial(
  geography = "place",
  variables = "P1_001N",  # Total population variable in 2020 Census
  year = 2020,
  sumfile = "pl",
  geometry = T
)

places_ <- places |> 
  filter(value > 50000)

sdp <- read_csv("data/raw/SDP areas.csv")

sdp_list <- unlist(sdp$place)

selected_places <- c("Akron", "Baltimore", "Birmingham", "Boston", "Buffalo", "Charlotte", 
                     "Chicago", "Cleveland", "Columbus", "Dallas", "Framingham", "Hartford", "Los Angeles", 
                     "Long Island", "Long Island", "Minneapolis", "Milwaukee", "Nashville", 
                     "New Haven", "New Orleans", "New York City", "Pittsburgh", "Reno", "Rochester", "Syracuse",
                     "Richmond", "San Diego", "San Jose", "Seattle") # remove Houston

# Create a regex pattern from the list
pattern <- paste(selected_places, collapse = "|")

# Filter the dataframe using partial matching
filtered_places <- places_ %>%
  filter(str_detect(NAME, pattern)) |> 
  filter(GEOID != "0660620") |> 
  filter(GEOID != "0922700") |> 
  filter(GEOID != "1258350") |> 
  filter(GEOID != "1304204") |> 
  filter(GEOID != "1319000") |> 
  filter(GEOID != "1814734") 

more_places <- places_ |> 
  filter(GEOID == "0644000") |> 
  rbind(filtered_places) |> 
  select(NAME, GEOID, geometry) |> 
  mutate(NAME = ifelse(GEOID == "4752006", "Nashville, TN", NAME),
         NAME = str_remove(NAME, " city"),
         NAME = str_remove(NAME, " CDP")) |> 
  separate(NAME, into = c("city", "state"), sep = ", ") |> 
  filter(GEOID != "2669035") |> 
  filter(GEOID != "2754880")

places_line <- st_cast(more_places, "MULTILINESTRING")
places_line <- st_cast(places_line, "LINESTRING")

write_sf(more_places, "data/processed/new_city_boundaries.geojson")
# then you need to add this to the app
