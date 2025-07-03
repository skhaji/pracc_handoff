## process the rest of the shapes for the map
### each area has a different definition. In this script I am selecting the census tracts that meet the criteria for Housing Choice programs
# then I intersect the tracts with school districts to find the districts that Housing Choice areas can go to

library(tidyverse)
library(sf)
library(cleangeo)
library(openxlsx)
library(readxl)
library(rmapshaper)
library(leaflet)

options(scipen = 999)

### simplified sd data
raw_sd_22 <- st_read("data/processed/geo/sds_2022_15pct.geojson") |> 
  st_transform('ESRI:102008')

# SNOMass 
# Berkshire County - 1
# Cape Cod - 4
# Springfleld-Holyoke - 5
# Metro Boston - 6
# Framingham - 8
# Lowell-Lawrence - 2
# South Shore - 9
# Worcester - 7

raw_snomass <- read_sf("data/raw/voucher_areas/eligible_raw/snomass_eligible.shp")

snomass <- raw_snomass |> 
  mutate(voucher_area = case_when(
    Regin == "REGION 1" ~ "Berkshire County",
    Regin == "REGION 2" ~ "Lowell-Lawrence",
    Regin == "REGION 4" ~ "Cape Cod",
    Regin == "REGION 5" ~ "Springfleld-Holyoke",
    Regin == "REGION 6" ~ "Metro Boston",
    Regin == "REGION 7" ~ "Worcester",
    Regin == "REGION 8" ~ "Framingham",
    Regin == "REGION 9" ~ "South Shore",
    TRUE ~ NA
  )) |> 
  select(GEOID, voucher_area, geometry) |> 
  filter(voucher_area != "Metro Boston") |> 
  st_transform('ESRI:102008')

# HUD Community Choice

raw_hud_cc <- st_read("data/raw/voucher_areas/eligible_raw/hud_community_choice_to_select.shp")

hud_cc <- raw_hud_cc |> 
  select(Neighborho, AOI, geometry) |> 
  rename(GEOID = Neighborho,
         voucher_area = AOI) |> 
  st_transform('ESRI:102008')

#### Charlotte
charlotte <- st_read("data/raw/voucher_areas/eligible_raw/charlotte_eligible.shp") |> 
  select(NPA_ID, City, geometry) |> 
  rename(GEOID = NPA_ID,
         voucher_area = City) |> 
  mutate(voucher_area = "charlotte") |> 
  st_transform('ESRI:102008')

### Dallas -  I already did this one in a previous script
# dallas <- st_read("data/raw/voucher_areas/eligible_raw/dallas_inclusive_communities_fromcts.geojson")

### milwaukee
milwaukee <- st_read("data/raw/voucher_areas/eligible_raw/milwaukee_eligible.shp") |> 
  mutate(voucher_area = "milwaukee") |> 
  select(GEOID, voucher_area, geometry) |> 
  st_transform('ESRI:102008')

### new york - this is the data received in 2025 from Victoria Beckleys that I will use to select 
# tracts based on NRI quintiles, different counties have different criteria
raw_ny <- st_read("data/raw/voucher_areas/NY_Opp_Areas_2025/NRI_Score_Tracts.shp")

# downstate and ithaca area are quintiles 4 and 5
ny_downstate <- raw_ny |> 
  mutate(voucher_area = ifelse(grepl("36027", GEOID), "orange-dutchess", NA),
         voucher_area = ifelse(grepl("36059", GEOID), "nassau", voucher_area),
         voucher_area = ifelse(grepl("36119", GEOID), "westchester", voucher_area),
         voucher_area = ifelse(grepl("36109", GEOID), "ithaca", voucher_area),
         voucher_area = ifelse(grepl("36103", GEOID), "suffolk", voucher_area)) |> 
  filter(NRI_Quinti >= 4) |>
  filter(!is.na(voucher_area)) |> 
  select(GEOID, voucher_area, geometry) |> 
  st_transform('ESRI:102008')

# upstate are quintiles 3, 4, and 5
ny_upstate <- raw_ny |> 
  mutate(voucher_area = ifelse(grepl("36029", GEOID), "buffalo", NA),
         voucher_area = ifelse(grepl("36055", GEOID), "rochester", voucher_area),
         voucher_area = ifelse(grepl("36067", GEOID), "syracuse", voucher_area)) |> 
  filter(NRI_Quinti >= 3) |>
  filter(!is.na(voucher_area)) |> 
  select(GEOID, voucher_area, geometry) |> 
  st_transform('ESRI:102008')

ny <- ny_downstate |> 
  rbind(ny_upstate)

leaflet() |> 
  addTiles() |> 
  addPolygons(data = ny |> st_transform(4326))

rm(ny_upstate, ny_downstate)

### sandiego
raw_san_diego <- st_read("data/raw/voucher_areas/eligible_raw/san_diego.shp") 

san_diego <- raw_san_diego |> 
  mutate(voucher_area = "san-diego") |> 
  select(fips, voucher_area, geometry) |> 
  rename(GEOID = fips) |> 
  st_transform('ESRI:102008')


### seattle
seattle <- st_read("data/raw/voucher_areas/eligible_raw/seattle_eligible.shp") |> 
  mutate(voucher_area = "Seattle") |> 
  select(GEOID10, voucher_area, geometry) |> 
  rename(GEOID = GEOID10) |> 
  st_transform('ESRI:102008')

### the rest
raw_dallas <- st_read("data/raw/voucher_areas/Dallas (Inclusive Communities Project)/dallas_inclusive_communities_fromcts.geojson")
raw_new_haven <- st_read("data/raw/voucher_areas/New Haven Opportunity Areas/new_haven_opp_areas_from192cts.geojson")
raw_kent <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Kent Eligible Tracts.shp")
raw_oakland <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Oakland Elgible Tracts.shp")
raw_macomb <- st_read("data/raw/voucher_areas/Michigan (Kent, Oakland, Macomb)/Macomb Eligible Tracts.shp")
raw_santa_clara <- st_read("data/raw/voucher_areas/Santa Clara County/SCCHA_Opportunity_Map_2024.shp")

### create new haven and the right columns
new_haven<- raw_new_haven |> 
  mutate(voucher_area = "new-haven") |> 
  select(GEOID, voucher_area, geometry) |> 
  st_transform('ESRI:102008')

dallas <- raw_dallas |> 
  mutate(voucher_area = "dallas") |> 
  select(GEOID, voucher_area, geometry) |> 
  st_transform('ESRI:102008')

# write_sf(dallas, "data/temp/dallas-today2.geojson")

santa_clara <- st_as_sf(clgeo_Clean(raw_santa_clara)) %>%
  mutate(voucher_area = "santa-clara") |> 
  select(fips, voucher_area, geometry) |>
  rename(GEOID = fips) |> 
  st_transform('ESRI:102008')

kent <- raw_kent %>%
  mutate(voucher_area = "kent") |> 
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10) |> 
  st_transform('ESRI:102008')

oakland <- raw_oakland %>%
  mutate(voucher_area = "oakland") |> 
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10) |> 
  st_transform('ESRI:102008')

macomb <- raw_macomb %>%
  mutate(voucher_area = "macomb") |> 
  select(GEOID10, voucher_area, geometry) |>
  rename(GEOID = GEOID10) |> 
  st_transform('ESRI:102008')

####################################
#### Do intersection for all ######
####################################

## rbind all in 
voucher_area_list<- c("snomass", "hud_cc", "charlotte", "milwaukee", "ny", "seattle", "macomb", "oakland",
                      "kent", "santa_clara", "dallas", "new_haven", "san_diego")

# Use lapply to get each data frame by name and then rbind them together
new_voucher_areas <- do.call(rbind, lapply(voucher_area_list, get))

# write_sf(new_voucher_areas, "data/temp/new_vouchers_25_2.geojson")

### create list of school districts that overlap at all to reduce the number of districts for intersection later
overlapping_list <- raw_sd_22 |> 
  # st_join(new_voucher_areas, largest = FALSE, join = st_overlaps) |> 
  st_join(new_voucher_areas) |> 
  filter(!is.na(voucher_area)) |> 
  st_drop_geometry() |> 
  select(district_id, voucher_area) |> 
  distinct() |> 
  filter(!(voucher_area == "westchester" & district_id == "3620580")) # remove a distirct that shouldn't be there


### select overlapping districts, calculate area
sds_22 <- raw_sd_22 |> 
  right_join(overlapping_list, by = "district_id") %>%
  mutate(area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  select(district_id, area)

#### now do intersection to select the areas with actual overlap
voucher_intersection <- st_intersection(sds_22, new_voucher_areas) %>%
  mutate(int_area = (units::drop_units(st_area(.))) * 3.86102e-7) |> 
  filter(int_area >= .1) |> # select only districts with a large enough intersection to warrant including
  select(district_id, geometry) |> 
  group_by(district_id) |> 
  summarise(count = n())

# write_sf(voucher_intersection, "data/temp/voucher_intersection_252.geojson")

voucher_sds_poly <- voucher_intersection |> 
  left_join(st_drop_geometry(raw_sd_22)) |> 
  select(state, district_id, district_name, level, geometry) |> 
  st_transform(4326) %>%
  filter(!st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  st_collection_extract("POLYGON") %>% ## all this to remove two stupid points that shouldn't be there
  mutate(area = units::drop_units(st_area(.))) |> 
  filter(area > 1000) |> 
  select(-area) |> 
  group_by(state, district_id, district_name) |> 
  summarise(level = first(level))


# Define columbus area school districts (short names)
columbus_sds <- c(
  "Bexley", "Walnut", "Dublin", "Gahanna-Jefferson",
  "Grandview",
  "Hilliard", "Albany", "Olentangy", "Arlington",
  "Westerville", "Worthington"
)

# Filter for Ohio (state == 39)
oh_sd <- raw_sd_22 %>%
  filter(state == 39)

# Function to check for fuzzy matches
fuzzy_match_district <- function(district_name, search_terms) {
  # Tokenize the district name
  district_name_lower <- tolower(district_name)
  for (term in search_terms) {
    term_lower <- tolower(term)
    if (grepl(term_lower, district_name_lower, fixed = TRUE)) {
      return(TRUE) # Found a match
    }
  }
  return(FALSE) # No match found
}

# Apply fuzzy matching to filter oh_sd
columbus_oh_sd <- oh_sd %>%
  filter(sapply(district_name, fuzzy_match_district, search_terms = columbus_sds)) |> 
  filter(district_id != 3904742) |> 
  filter(district_id != 3904988) |> 
  filter(district_id != 3904690) |> 
  mutate(voucher_area = "columbus") |>
  select(state, voucher_area, district_name, level, district_id, geometry) |> 
  st_transform(4326)

# import the pilot areas that I identified long ago: baltimore, houston, and boston (nassau and suffolk were redefined so those are reprocessed above) 
boston <- st_read("data/processed/geo/pilot/boston-area.geojson") |> 
  mutate(voucher_area = "boston") |>
  select(state, voucher_area, district_name, level, district_id, geometry) |> 
  st_transform(4326)
houston <- st_read("data/processed/geo/pilot/houston-area.geojson") |> 
  mutate(voucher_area = "houston") |>
  select(state, voucher_area, district_name, level, district_id, geometry) |> 
  st_transform(4326)
baltimore <- st_read("data/processed/geo/pilot/baltimore-area.geojson") |> 
  mutate(voucher_area = "baltimore") |>
  select(state, voucher_area, district_name, level, district_id, geometry) |> 
  st_transform(4326)


voucher_sds_geo <- voucher_intersection |> 
  left_join(st_drop_geometry(raw_sd_22)) |> 
  select(state, district_id, district_name, level, geometry) |> 
  st_transform(4326) %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) |> 
  rbind(voucher_sds_poly) |> ## add in the poly above
  ms_simplify(keep = 0.15) |>
  left_join(overlapping_list, by = "district_id") |> 
  select(state, voucher_area, everything()) |> 
  rbind(columbus_oh_sd) |>
  rbind(boston) |>
  rbind(houston) |>
  rbind(baltimore) |>
  mutate(voucher_area = tolower(voucher_area)) |> 
  filter(!(state == "42" & voucher_area == "rochester")) |> 
  filter(!(district_id == "3620580" & voucher_area == "nassau")) |> # new york city sd is being caught, remove
  st_make_valid() |> 
  distinct() |> 
  arrange(voucher_area)

# write_sf(voucher_sds_geo, "data/temp/voucher_sds_20250628.geojson")

leaflet() |> 
  addTiles() |> 
  addPolygons(data = voucher_sds_geo |> 
                        filter(voucher_area == "rochester"))


unique(voucher_sds_geo$voucher_area)

# Define output directory
output_dir <- "data/processed/geo/voucher_area_sds/"

# Loop over unique voucher areas and save as separate GeoJSON files
voucher_sds_geo %>%
  split(.$voucher_area) %>%  # Split dataframe by voucher_area
  walk(~ st_write(.x, paste0(output_dir, gsub(" ", "-", .x$voucher_area[1]), ".geojson"), 
                  driver = "GeoJSON", delete_dsn = TRUE))  # Write each as a GeoJSON

#### Add new area to list of all sds included

all_new_sds <- st_drop_geometry(voucher_sds_geo) |> 
  select(district_id, voucher_area)

### if you want to see the list of current sds in the tool, look here:
all_sds <- read_csv("data/final/all_sds_in_tool.csv") 

#check which districts area changing
differences <- all_sds |> 
  full_join(all_new_sds, by = "district_id")


# all_new_sds <- st_drop_geometry(all_sds) |> 
#   # filter(voucher_area != "buffalo")  |> 
#   select(district_id, voucher_area) |> 
#   distinct() |> 
#   rbind(final_dataframe_df) |> 
#   distinct() |> 
#   arrange(voucher_area) |> 
#   filter(!(voucher_area == "westchester" & district_id == "3620580"))

unique(all_new_sds$voucher_area)

### write it out
write_csv(all_new_sds, "data/final/all_sds_in_tool.csv")

