### calculate theil's index using segregation package

library(tidyverse)
library(segregation)
library(educationdata)


### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") 

nces_data <- read_csv("data/processed/nces_data.csv")

### import nces data about type and level of school from nces
reg_school_nces <- read_csv("data/processed/nces_school_type_info.csv") |> 
  select(-school_type) |> 
  filter(school_level != "Prekindergarten") |> 
  filter(!grepl("CLOSED", school_name))

### import raw frl data from ELSI
raw_frl <- read_csv("data/raw/nces/school_enrollment_by_frl/ELSI_csv_export_6386537491701063699292.csv", 
                       na = c("†", "‡", "–"), skip = 5)

### get schools enrollment by race from Urban
raw_enrollment <- get_education_data(
  level = 'schools', 
  source = 'ccd', 
  topic = 'enrollment', 
  subtopic = 'race',
  filters = list(grade = 'grade-99',
                 year = 2022),
  add_labels = TRUE
)


### Calculate Theil's H for race using only regular schools
reg_school_enrollment <- raw_enrollment |> 
  rename(school_id = ncessch) |> 
  right_join(reg_school_nces, by = "school_id") |> 
  select(district_id, school_id, race, enrollment) |> 
  filter(race != "Unknown", !is.na(race)) |> 
  filter(race != "Total")

### calculate Theil's H
within <- mutual_within(reg_school_enrollment, "race", "school_id",
                         within = "district_id", weight = "enrollment", wide = TRUE)

theils_race <- nces_data |> 
  left_join(within, by= "district_id") |> 
  select(district_id, district, H) |> 
  rename(theils_H_race = H)


### Calculate Theil's H for FRL using only regular schools 
reg_school_frl <- raw_frl |> 
  rename(school_id = `School ID - NCES Assigned [Public School] Latest available year`,
         frl = `Free and Reduced Lunch Students [Public School] 2021-22`) |> 
  mutate(frl = ifelse(is.na(frl), `Direct Certification [Public School] 2021-22`, frl),
    nonfrl = `Total Students All Grades (Excludes AE) [Public School] 2021-22` - frl) |> 
  select(school_id, frl, nonfrl) |> 
  right_join(reg_school_nces, by = "school_id") |> 
  select(district_id, school_id, frl, nonfrl) |> 
  pivot_longer(cols = c(frl, nonfrl), 
               names_to = "frl_status", 
               values_to = "enrollment")

### calculate Theil's H
within_frl <- mutual_within(reg_school_frl, "frl_status", "school_id",
                        within = "district_id", weight = "enrollment", wide = TRUE)

theils_frl <- nces_data |> 
  left_join(within_frl, by= "district_id") |> 
  select(district_id, H) |> 
  rename(theils_H_frl = H)


segregation <- theils_race |> 
  left_join(theils_frl, by = "district_id") |> 
  mutate(theils_race_cat = case_when(theils_H_race < 0.05 ~ "low",
                                       theils_H_race >= 0.05 & theils_H_race < 0.15 ~ "mid-low",
                                       theils_H_race >= 0.15 & theils_H_race < 0.25 ~ "mid-high",
                                       theils_H_race >= 0.25 ~ "high"),
         theils_frl_cat = case_when(theils_H_frl < 0.05 ~ "low",
                                       theils_H_frl >= 0.05 & theils_H_frl < 0.15 ~ "mid-low",
                                       theils_H_frl >= 0.15 & theils_H_frl < 0.25 ~ "mid-high",
                                       theils_H_frl >= 0.25 ~ "high")) |> 
  select(-district)

write_csv(segregation, "data/processed/theils_segregation_index.csv")

#### Test to match SID_Beneficiaries brief - not quite but close enough considering I don't know what schools they are removing
# ### get schools enrollment by race from Urban
# raw_enrollment19_4th <- get_education_data(
#   level = 'schools', 
#   source = 'ccd', 
#   topic = 'enrollment', 
#   subtopic = 'race',
#   filters = list(grade = 'grade-4',
#                  year = 2019),
#   add_labels = TRUE
# )
# 
# reg_school_enrollment19_4th <- raw_enrollment19_4th |> 
#   rename(school_id = ncessch,
#          district_id = leaid) |>
#   # right_join(reg_school_nces, by = "school_id") |>
#   select(district_id, school_id, race, enrollment) |> 
#   filter(race != "Unknown", !is.na(race)) |> 
#   filter(race != "Total")
# 
# ### calculate Theil's H
# within19_4th <- mutual_within(reg_school_enrollment19_4th, "race", "school_id",
#                         within = "district_id", weight = "enrollment", wide = TRUE)
# 
# theils19_4th <- nces_data |> 
#   left_join(within19_4th, by= "district_id") |> 
#   select(state, district_id, district, H, everything())
