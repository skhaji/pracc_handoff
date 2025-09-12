### process spatial data

library(sf)
library(tidyverse)


### spatial data

raw_sds_sf <- st_read("data/raw/EDGE_SCHOOLDISTRICT_TL22_SY2122/EDGE_SCHOOLDISTRICT_TL22_SY2122.shp")

sds_sf_22 <- raw_sds_sf |> 
  filter(NAME != "School District Not Defined") |> 
  # filter(is.na(SCSDLEA)) |> #### remove the secondary shapes
  select(STATEFP, GEOID, NAME, INTPTLAT, INTPTLON, geometry)

sd_list <- st_drop_geometry(sds_sf_22) |> 
  select(STATEFP:NAME) |> 
  rename(state = STATEFP,
         district_id = GEOID,
         district = NAME)


write_csv(sd_list, "data/processed/all_school_districts_geo.csv")


### shapes to reduce size
sds_shape <- raw_sds_sf |> 
  select(GEOID, STATEFP, ELSDLEA:UNSDLEA, NAME, geometry) |>
  rename(district_id = GEOID,
         state = STATEFP,
         district_name = NAME) |>
  mutate(level = case_when(!is.na(ELSDLEA) ~ "elementary",
                           !is.na(SCSDLEA) ~ "secondary",
                           !is.na(UNSDLEA) ~ "unified")) |>
  select(state, district_id, district_name, level, geometry) |> 
  filter( state != "60", state != "15", state != "02", state != "72", state != "66" , state != "78", state != "69") |> 
  st_transform(4326)

write_sf(sds_shape, "data/processed/geo/sds_2022.geojson")
