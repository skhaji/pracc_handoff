### process EDGE poverty and employment data

# https://nces.ed.gov/programs/edge/TableViewer/acsProfile/2022

library(tidyverse)


### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") |> 
  select(district_id)

raw_poverty <- read_delim("data/raw/edge/EDGE_Export_913154920138 - RELEVANT CHILDREN - ENROLLED PUBLIC/CDP03.4_105_USSchoolDistrictAll_913154917544.txt",
                          col_names = T, delim = "|")

poverty <- raw_poverty |> 
  select(LEAID, Geography, CDP03_54pct, CDP03_54pctmoe) |> 
  rename(district_id = LEAID,
         pct_poverty = CDP03_54pct,
         pct_poverty_moe = CDP03_54pctmoe) |> 
  right_join(sds_list, by = "district_id") 


raw_parental_employment <- read_delim("data/raw/edge/EDGE_Export_913155410470 - PARENTS OF RELEVANT CHILDREN - ENROLLED PUBLIC/PDP03.1_205_USSchoolDistrictAll_91315546330.txt",
                          col_names = T, delim = "|")

# PDP03_2est	Number; Estimate; EMPLOYMENT STATUS; Population 16 years and over; In labor force
# PDP03_2moe	Number; Margin of Error; EMPLOYMENT STATUS; Population 16 years and over; In labor force
# PDP03_4est	Number; Estimate; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Civilian labor force; Employed
# PDP03_4moe	Number; Margin of Error; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Civilian labor force; Employed
# PDP03_4pct	Percent; Estimate; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Civilian labor force; Employed
# PDP03_4pctmoe	Percent; Margin of Error; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Civilian labor force; Employed
# PDP03_6est	Number; Estimate; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Armed Forces
# PDP03_6moe	Number; Margin of Error; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Armed Forces
# PDP03_6pct	Percent; Estimate; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Armed Forces
# PDP03_6pctmoe	Percent; Margin of Error; EMPLOYMENT STATUS; Population 16 years and over; In labor force; Armed Forces

###  % employed = (# in labor force-employed civilian + # in labor force-employed Armed Forces) / in labor force

parental_employment <- raw_parental_employment |> 
  select(LEAID, PDP03_2est, PDP03_2moe, PDP03_4est, PDP03_4moe, PDP03_6est, PDP03_6moe, 
         PDP03_4pct, PDP03_4pctmoe, PDP03_6pct, PDP03_6pctmoe) |> 
  rename(pct_employed_civilian = PDP03_4pct,
         pct_employed_civilian_moe = PDP03_4pctmoe,
         pct_employed_military = PDP03_6pct,
         pct_employed_military_moe = PDP03_6pctmoe) |> 
  mutate(pct_employed = round((PDP03_4est + PDP03_6est)/PDP03_2est, 3))

civilian_employment <- parental_employment |> 
  select(LEAID, pct_employed_civilian, pct_employed_civilian_moe) |> 
  rename(district_id = LEAID) |> 
  right_join(sds_list, by = "district_id") 



raw_language <- read_delim("data/raw/edge/EDGE_Export_913155756929 - RELEVANT CHILDREN - ENROLLED PUBLIC/CDP02.9_105_USSchoolDistrictAll_913155753336.txt",
                                      col_names = T, delim = "|")

# language <- raw_language |>
#   select(LEAID, CDP02_53est:CDP02_64pct)

language <- raw_language |>
  # select(LEAID, CDP02_55pct, CDP02_55pctmoe) |>
  rename(district_id = LEAID,
         pop = CDP02_53est,
         pop_moe = CDP02_53moe,
         pct_only_english = CDP02_54pct,
         pct_only_english_moe = CDP02_54pctmoe,
         pct_other_language = CDP02_55pct,
         pct_other_language_moe = CDP02_55pctmoe,
         pct_spanish_athome = CDP02_57pct,
         pct_spanish_athome_moe = CDP02_57pctmoe,
         pct_indo_european_athome = CDP02_59pct,
         pct_indo_european_athome_moe = CDP02_59pctmoe,
         pct_aspi_athome = CDP02_61pct,
         pct_aspi_athome_moe = CDP02_61pctmoe) |>
  select(district_id, pop, pop_moe, pct_only_english, pct_only_english_moe,
         pct_other_language, pct_other_language_moe,
         pct_spanish_athome, pct_spanish_athome_moe,
         pct_indo_european_athome, pct_indo_european_athome_moe,
         pct_aspi_athome, pct_aspi_athome_moe) |>  
  mutate(across(pct_only_english:pct_aspi_athome_moe, ~ . / 100 )) |> 
  right_join(sds_list, by = "district_id")
  
edge_data <- poverty |> 
  left_join(civilian_employment, by = "district_id") |> 
  left_join(language, by = "district_id")


write_csv(edge_data, "data/processed/edge_data_2018_22.csv")
