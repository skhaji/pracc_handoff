#### script to create shapes for areas with descriptions

library(tidyverse)
library(sf)
library(cleangeo)
library(openxlsx)
library(readxl)
library(rmapshaper)
library(tidycensus)

options(scipen = 999)

# New Haven zip to area

raw_cts <- read_excel("data/raw/voucher_areas/New Haven Opportunity Areas/CT901 - CT004 Opportunity Areas 192 CTs.xlsx")

cts <- raw_cts |> 
  mutate(GEOID = paste0("09170", `CT 901 - CT 004`))

ct_tracts <- get_acs(geography = "tract",
                           state = "CT",
                           variables = "B04006_001",
                           geometry = T,
                           year = 2022)

opp_tracts <- ct_tracts |> 
  right_join(cts, by = "GEOID") |> 
  select(GEOID, geometry) |> 
  mutate(voucher_area = "new haven") |> 
  select(GEOID, voucher_area, geometry) |> 
  group_by(voucher_area) |> 
  summarize(GEOID = n())

write_sf(opp_tracts, "data/raw/voucher_areas/New Haven Opportunity Areas/new_haven_opp_areas_from192cts.geojson")

#### Dallas

raw_dallas <- st_read("data/raw/voucher_areas/Dallas (Inclusive Communities Project)/WTA_2020_v2.shp") |> 
  filter(Walker_Tar == "Yes")

dallas <- raw_dallas |> 
  mutate(voucher_area = "dallas") |> 
  select(GEOID, voucher_area, geometry) |> 
  group_by(voucher_area) |> 
  summarize(GEOID = n())

write_sf(dallas, "data/raw/voucher_areas/Dallas (Inclusive Communities Project)/dallas_inclusive_communities_fromcts.geojson")

