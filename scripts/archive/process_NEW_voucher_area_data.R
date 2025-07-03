#### create Boston dataset

library(tidyverse)
library(sf)
library(cleangeo)
library(openxlsx)
library(readxl)
library(rmapshaper)

options(scipen = 999)

### simplified sd data
raw_sd_22 <- st_read("data/processed/geo/sds_2022_15pct.geojson")

### round 1
# raw_boston <- st_read("data/raw/voucher_areas/Boston_FINALECCOMAP_091824/FINALECCOMAP_091824.shp") 
# raw_long_island <- st_read("data/raw/voucher_areas/NY_Opp_Areas_2024/LI_Opportunity_Areas_2024.shp.geojson")
# raw_houston <- st_read("data/raw/voucher_areas/Houston/Houston_opportunity_areas.shp")
# 
# #### round 2
#### buffalo needs to be REDONE ####
###################################
raw_buffalo <- st_read("data/raw/voucher_areas/Buffalo Tracts for Prrac/tracts.shp") |> 
  filter(Opprt_D == "Opportunity")
# raw_baltimore <- st_read("data/raw/voucher_areas/BRHP 2025 Opportunity Area/BRHP_opportunity.shp")

#### round 3
# raw_dallas <- st_read("data/raw/voucher_areas/Dallas (Inclusive Communities Project)/WTA_2020_v2.shp") |>
#   filter(Walker_Tar == "Yes")

raw_dallas <- st_read("data/raw/voucher_areas/Dallas (Inclusive Communities Project)/dallas_inclusive_communities_fromcts.geojson")
raw_new_haven <- st_read("data/raw/voucher_areas/New Haven Opportunity Areas/new_haven_opp_areas_from192cts.geojson")
raw_kent <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Kent Eligible Tracts.shp")
raw_oakland <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Oakland Elgible Tracts.shp")
raw_macomb <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Macomb Eligible Tracts.shp")
raw_santa_clara <- st_read("data/raw/voucher_areas/Santa Clara County/SCCHA_Opportunity_Map_2024.shp")


area_calc <- function(new_voucher_area) {
  new_areas <- st_as_sf(clgeo_Clean(new_voucher_area)) %>%
    mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
    st_transform(4326)
}

### create new haven and the right columns
new_haven_voucher_area <- area_calc(raw_new_haven) |> 
  mutate(voucher_area = "new haven") |> 
  select(GEOID, voucher_area, geometry)

dallas_voucher_area <- area_calc(raw_dallas) |> 
  mutate(voucher_area = "dallas") |> 
  select(GEOID, voucher_area, geometry)

### then select the variables you want to keep

# boston_voucher_area <- st_as_sf(clgeo_Clean(raw_boston)) %>%
#   mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
#   st_transform(4326) |> 
#   select(TOWN, voucher_area, geometry)
# 
# li_voucher_area <- raw_long_island %>%
#   mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|> 
#   st_transform(4326) |> 
#   select(GEOID, voucher_area, geometry)
# 
# houston_voucher_area <- raw_houston %>%
#   mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|> 
#   st_transform(4326) |> 
#   select(Opparea, voucher_area, geometry)
# 
# dallas_voucher_area <- st_as_sf(clgeo_Clean(raw_dallas)) %>%
#   mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7) |>
#   st_transform(4326) |>
#   select(GEOID, voucher_area, geometry)
# 
buffalo_voucher_area <- raw_buffalo %>%
  mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|>
  st_transform(4326) |>
  select(GEOID, voucher_area, geometry)
# 
# baltimore_voucher_area <- raw_baltimore %>%
#   mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|> 
#   st_transform(4326) |> 
#   select(GEOID, voucher_area, geometry)
# 
santa_clara_voucher_area <- st_as_sf(clgeo_Clean(raw_santa_clara)) %>%
  mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7) |>
  st_transform(4326) |>
  select(fips, voucher_area, geometry) |>
  rename(GEOID = fips)

kent_voucher_area <- raw_kent %>%
  mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|>
  st_transform(4326) |>
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10)

oakland_voucher_area <- raw_oakland %>%
  mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|>
  st_transform(4326) |>
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10)

macomb_voucher_area <- raw_macomb %>%
  mutate(voucher_area = (units::drop_units(st_area(.))) * 3.86102e-7)|>
  st_transform(4326) |>
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10)


# rm(raw_boston, raw_long_island, raw_houston, raw_kent, raw_oakland, raw_macomb, raw_santa_clara, raw_dallas, raw_buffalo, raw_baltimore)

##########################
###  BOSTON ##############
##########################

### create list of school districts that overlap at all to reduce the number of distircts for intersection later
boston_overlapping_list <- raw_sd_22 |> 
  st_join(boston_voucher_area, largest = FALSE, join = st_overlaps) |> 
  filter(!is.na(TOWN)) |> 
  st_drop_geometry() |> 
  select(district_id) |> 
  distinct()

### transform voucher area to boston projection
boston_voucher_area_26986 <- boston_voucher_area |> 
  st_transform(26986)

### select overlapping districts, calculate area
boston_sds_22 <- raw_sd_22 |> 
  right_join(boston_overlapping_list, by = "district_id") |> 
  st_transform(26986) %>%
  mutate(area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  select(district_id, area)

#### now do intersection to select the areas with actual overlap
boston_voucher_intersection <- st_intersection(boston_sds_22, boston_voucher_area_26986) %>%
  mutate(int_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  filter(int_area >= .1) |> # select only districts with a large enough intersection to warrent including
  select(district_id, geometry) |> 
  group_by(district_id) |> 
  summarise(count = n())

boston_sds <- boston_voucher_intersection |> 
  left_join(st_drop_geometry(raw_sd_22)) |> 
  select(state, district_id, district_name, level, geometry) |> 
  st_transform(4326) |> 
  ms_simplify(keep = 0.15)


##########################
###  Long Island #########
##########################

### create list of school districts that overlap at all to reduce the number of distircts for intersection later
li_overlapping_list <- raw_sd_22 |> 
  st_join(li_voucher_area, largest = FALSE, join = st_overlaps) |> 
  filter(!is.na(GEOID)) |> 
  st_drop_geometry() |> 
  select(district_id) |> 
  distinct()

### transform voucher area to boston projection
li_voucher_area_2263 <- li_voucher_area |> 
  st_transform(2263)

### select overlapping districts, calculate area
li_sds_22 <- raw_sd_22 |> 
  right_join(li_overlapping_list, by = "district_id") |> 
  st_transform(2263) %>%
  mutate(area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  select(district_id, area)

#### now do intersection to select the areas with actual overlap
li_voucher_intersection <- st_intersection(li_sds_22, li_voucher_area_2263) %>%
  mutate(int_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  filter(int_area >= 1) |> # select only districts with a large enough intersection to warrent including
  select(district_id, geometry) |> 
  group_by(district_id) |> 
  summarise(count = n())

li_sds <- li_voucher_intersection |> 
  left_join(st_drop_geometry(raw_sd_22)) |> 
  select(state, district_id, district_name, level, geometry) |> 
  st_transform(4326)

ny_sd_county <- read_csv("/Users/sarahodges/Documents/spatial/NewSchool/methods1-materials-fall2024/methods1-slides/data/processed/ny_sd_county_pov_data.csv") |> 
  select(id, COUNTY) |> 
  rename(district_id = id,
         County = COUNTY) |> 
  mutate(district_id = as.character(district_id))

nassau_sds <- li_sds |> 
  left_join(ny_sd_county, by = "district_id") |> 
  filter(County == "Nassau County") |> 
  select(-County)

suffolk_sds <- li_sds |> 
  left_join(ny_sd_county, by = "district_id") |> 
  filter(County == "Suffolk County") |> 
  select(-County)

##########################
###  Houston #########
##########################

### create list of school districts that overlap at all to reduce the number of distircts for intersection later
houston_overlapping_list <- raw_sd_22 |> 
  st_join(houston_voucher_area, largest = FALSE, join = st_overlaps) |> 
  filter(!is.na(Opparea)) |> 
  st_drop_geometry() |> 
  select(district_id) |> 
  distinct()

houston_voucher_area_2278 <- houston_voucher_area |> 
  st_transform(2278)

### select overlapping districts, calculate area
houston_sds_22 <- raw_sd_22 |> 
  right_join(houston_overlapping_list, by = "district_id") |> 
  st_transform(2278) %>%
  mutate(area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  select(district_id, area)

#### now do intersection to select the areas with actual overlap
houston_voucher_intersection <- st_intersection(houston_sds_22, houston_voucher_area_2278) %>%
  mutate(int_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  filter(int_area >= 1) |> # select only districts with a large enough intersection to warrent including
  select(district_id, geometry) |> 
  group_by(district_id) |> 
  summarise(count = n())

houston_sds <- houston_voucher_intersection |> 
  left_join(st_drop_geometry(raw_sd_22)) |> 
  select(state, district_id, district_name, level, geometry) |> 
  st_transform(4326) |> 
  ms_simplify(keep = 0.15)

## create list of districts to include in the data
all_sds <- st_drop_geometry(houston_sds) |> 
  select(district_id) |> 
  mutate(voucher_area = "houston") |> 
  rbind(st_drop_geometry(boston_sds) |> 
          select(district_id)|> 
          mutate(voucher_area = "boston")) |> 
  rbind(st_drop_geometry(nassau_sds) |> 
          select(district_id)|> 
          mutate(voucher_area = "nassau")) |> 
  rbind(st_drop_geometry(suffolk_sds) |> 
          select(district_id)|> 
          mutate(voucher_area = "suffolk"))

## write out for full dataset
write_csv(all_sds, "data/final/all_sds_in_tool.csv")

# write_sf(nassau_sds, "data/processed/geo/voucher_area_sds/nassau-county-ny.geojson")
# write_sf(suffolk_sds, "data/processed/geo/voucher_area_sds/suffolk-county-ny.geojson")
# write_sf(boston_sds, "data/processed/geo/voucher_area_sds/boston-area.geojson")
# write_sf(houston_sds, "data/processed/geo/voucher_area_sds/houston-area.geojson")
# 
# ### write to website
write_sf(nassau_sds, "sdat/sdat-app/static/data/nassau-county-ny.geojson")
write_sf(suffolk_sds, "sdat/sdat-app/static/data/suffolk-county-ny.geojson")
write_sf(boston_sds, "sdat/sdat-app/static/data/boston-area.geojson")
write_sf(houston_sds, "sdat/sdat-app/static/data/houston-area.geojson")


#### Add more

##########################
###  Generalize ##############
##########################

# List of voucher areas
# voucher_areas <- list(
#   macomb = macomb_voucher_area,
#   oakland = oakland_voucher_area,
#   kent = kent_voucher_area,
#   santa_clara = santa_clara_voucher_area,
#   baltimore = baltimore_voucher_area,
#   buffalo = buffalo_voucher_area,
#   dallas = dallas_voucher_area
# )
# 
# voucher_area = oakland_voucher_area

# voucher_areas <- list(
#   baltimore = baltimore_voucher_area,
#   buffalo = buffalo_voucher_area
# )

# voucher_areas <- list(
#   macomb = macomb_voucher_area,
#   oakland = oakland_voucher_area,
#   kent = kent_voucher_area,
#   santa_clara = santa_clara_voucher_area,
#   dallas = dallas_voucher_area,
#   new_haven = new_haven_voucher_area
# )

voucher_areas <- list(
  buffalo = buffalo_voucher_area
)

process_voucher_area <- function(voucher_area) {
  # Create a list of school districts that overlap
  overlapping_list <- raw_sd_22 %>% 
    st_join(voucher_area, largest = FALSE) %>% 
    filter(!is.na(GEOID)) %>% 
    st_drop_geometry() %>% 
    select(district_id) %>% 
    distinct()
  
  # Transform the voucher area to the desired projection
  voucher_area_102008 <- voucher_area %>% 
    st_transform("ESRI:102008")
  
  # Select overlapping districts and calculate area
  sds_22 <- raw_sd_22 %>% 
    right_join(overlapping_list, by = "district_id") %>% 
    st_transform("ESRI:102008") %>% 
    mutate(area = (units::drop_units(st_area(.))) * 3.86102e-7) %>% 
    select(district_id, area)
  
  # Perform intersection to select areas with actual overlap
  voucher_intersection <- st_intersection(sds_22, voucher_area_102008) %>%
    st_collection_extract("POLYGON") %>%  # Retain only polygon geometries
    mutate(int_area = (units::drop_units(st_area(.))) * 3.86102e-7) %>% 
    filter(int_area >= .1) %>% 
    select(district_id, geometry) %>% 
    group_by(district_id) %>% 
    summarise(count = n())
  
  # Finalize the results for this area
  voucher_intersection %>% 
    left_join(st_drop_geometry(raw_sd_22)) %>% 
    select(state, district_id, district_name, level, geometry) %>% 
    st_transform(4326) 
}

# Apply the function to each voucher area using lapply
final_results <- lapply(voucher_areas, process_voucher_area) 
final_dataframe <- bind_rows(final_results, .id = "source") 

final_dataframe_df <- st_drop_geometry(final_dataframe) |> 
  rename(voucher_area = source) |> 
  select(district_id, voucher_area)

#### Add new area to list of all sds included
all_sds <- read_csv("data/final/all_sds_in_tool.csv") 
all_new_sds <- st_drop_geometry(all_sds) |> 
  filter(voucher_area != "buffalo")  |> 
  select(district_id, voucher_area) |> 
  distinct() |> 
  rbind(final_dataframe_df) |> 
  distinct()

### write it out
# write_csv(all_new_sds, "data/final/all_sds_in_tool.csv")

### create a geojson for the new areas and write it out
new_haven_sds <- final_dataframe |>
  filter(source == "new_haven") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(new_haven_sds, "sdat/sdat-app/static/data/new-haven-area.geojson")

dallas_sds <- final_dataframe |>
  filter(source == "dallas") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(dallas_sds, "sdat/sdat-app/static/data/dallas-area.geojson")

santa_clara_sds <- final_dataframe |>
  filter(source == "santa_clara") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(santa_clara_sds, "sdat/sdat-app/static/data/santa-clara-area.geojson")

macomb_sds <- final_dataframe |>
  filter(source == "macomb") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(macomb_sds, "sdat/sdat-app/static/data/macomb-area.geojson")

oakland_sds <- final_dataframe |>
  filter(source == "oakland") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(oakland_sds, "sdat/sdat-app/static/data/oakland-area.geojson")

kent_sds <- final_dataframe |>
  filter(source == "kent") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)
write_sf(kent_sds, "sdat/sdat-app/static/data/kent-area.geojson")
         
buffalo_sds <- final_dataframe |>
  filter(source == "buffalo") |>
  select(-source) |>
  select(state, district_id, district_name, level, geometry)

# baltimore_sds <- final_dataframe |> 
#   filter(source == "baltimore") |> 
#   select(-source) |> 
#   select(state, district_id, district_name, level, geometry)
# 
# write_sf(buffalo_sds, "sdat/sdat-app/static/data/buffalo-area.geojson")
# write_sf(baltimore_sds, "sdat/sdat-app/static/data/baltimore-area.geojson")


