# process educator race/ethnicity data

library(tidyverse)
library(readxl)

### Source for educator data
# https://tntp.org/wp-content/uploads/2024/08/K12-demo-data-documentation.pdf

### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") |> 
  select(-state, -district)

### import state district id
raw_id <- read_csv("data/raw/nces/ELSI_csv_export_6386364957699203083203_state_id.csv", 
                   na = c("†", "‡", "–"), skip = 5)

##################################
####### Boston ###################
##################################

state_id <- raw_id |> 
  filter(`State Name [District] Latest available year` == "Massachusetts") |> 
  select(`Agency Name`, `Agency ID - NCES Assigned [District] Latest available year`,
         `State Agency ID [District] 2022-23`) |>
  rename(district_id = `Agency ID - NCES Assigned [District] Latest available year`,
         state_district_id = `State Agency ID [District] 2022-23`) |> 
  mutate(state_district_id = substr(state_district_id, 4, length(state_district_id)),
         state_district_id = paste0(state_district_id, "0000"))

#  data source: https://educationtocareer.data.mass.gov/Students-and-Teachers/Staffing-Race-Ethnicity-and-Gender/j5ue-xkfn/about_data
raw_ed_race <- read_csv("data/raw/educator_data/MassEd/Staffing__Race_Ethnicity_and_Gender_20241004.csv")

ed_type <- unique(raw_ed_race$JOBCLASS_CAT)

mass_ed_race <- raw_ed_race |> 
  filter(JOBCLASS_CAT == "Instructional Staff" & ORG_TYPE == "District" & JOBCLASS == "Teacher" & SY == 2024) |> 
  select(SY, DIST_CODE, DIST_NAME, FTE_TOTAL, AFRAME_PCT, ASIAN_PCT, HISPANIC_PCT, NATAME_PCT, 
         NATHPI_PCT, MULTINH_PCT, WHITE_PCT, FEMALE_PCT, MALE_PCT) |> 
  rename_with(tolower) |> 
  mutate(across(aframe_pct:male_pct, ~ .x / 100)) |> 
  right_join(state_id, by = c("dist_code" = "state_district_id")) |> 
  select(district_id, dist_name:male_pct) |> 
  rename(pct_black_educ = aframe_pct,
         pct_asian_educ = asian_pct,
         pct_latinx_educ = hispanic_pct,
         pct_native_educ = natame_pct,
         pct_hawpi_educ = nathpi_pct,
         pct_two_plus_educ = multinh_pct,
         pct_white_educ = white_pct) |> 
  select(district_id, dist_name, pct_black_educ, pct_asian_educ, 
         pct_latinx_educ, pct_native_educ, pct_hawpi_educ, pct_two_plus_educ, 
         pct_white_educ)
  
# 
# write_csv(ed_race, "data/processed/mass_educator_diversity.csv")


##################################
####### New York #################
##################################
raw_ny_educ_race <- read_excel("data/raw/educator_data/NYEd/staff-race-and-ethnicity-data-2018-on.xlsx", 
                               sheet = "TeacherRace18-23")

state_id_ny <- raw_id |> 
  filter(`State Name [District] Latest available year` == "New York") |> 
  select(`Agency Name`, `Agency ID - NCES Assigned [District] Latest available year`,
         `State Agency ID [District] 2022-23`) |>
  rename(district_id = `Agency ID - NCES Assigned [District] Latest available year`,
         STATE_DISTRICT_ID = `State Agency ID [District] 2022-23`) |> 
  mutate(STATE_DISTRICT_ID = str_remove(STATE_DISTRICT_ID, "NY-"))

ny_ed_race <- raw_ny_educ_race |> 
  filter(REPORT_SCHOOL_YEAR == "2022-23") |> 
  group_by(DISTRICT_NAME, STATE_DISTRICT_ID) |> 
  summarise(educ = sum(Total, na.rm = T),
            black_educ = sum(`Black or African American`, na.rm = T),
            asian_educ = sum(`Asian`, na.rm = T),
            latinx_educ = sum(`Hispanic`, na.rm = T),
            native_educ = sum(`American Indian Or Alaska Native`, na.rm = T),
            hawpi_educ = sum(`Native Hawaiian`, na.rm = T),
            two_plus_educ = sum(`Multiracial`, na.rm = T),
            white_educ = sum(`White`, na.rm = T)) |> 
  mutate(pct_black_educ = round(black_educ/educ, 3),
         pct_asian_educ = round(asian_educ/educ, 3),
         pct_latinx_educ = round(latinx_educ/educ, 3),
         pct_native_educ = round(native_educ/educ, 3),
         pct_hawpi_educ = round(hawpi_educ/educ, 3),
         pct_two_plus_educ = round(two_plus_educ/educ, 3),
         pct_white_educ = round(white_educ/educ, 3)) |> 
  left_join(state_id_ny, by = "STATE_DISTRICT_ID") |> 
  rename(dist_name = DISTRICT_NAME) |> 
  select(district_id, dist_name, pct_black_educ, pct_asian_educ, 
         pct_latinx_educ, pct_native_educ, pct_hawpi_educ, pct_two_plus_educ, 
         pct_white_educ)


##################################
####### Houston ###################
##################################

houston_ed_race <- read_csv("data/raw/educator_data/houston_educ_race_23.csv") |> 
  select(district_id, pct_black_educ, pct_asian_educ, 
         pct_latinx_educ, pct_native_educ, 
         pct_white_educ)

ed_race <- mass_ed_race |> 
  rbind(ny_ed_race) |> 
  select(district_id, pct_black_educ, pct_asian_educ, 
         pct_latinx_educ, pct_native_educ, 
         pct_white_educ) |>
  rbind(houston_ed_race) |>
  inner_join(sds_list, by = "district_id") 
  
write_csv(ed_race, "data/processed/educator_diversity.csv")
