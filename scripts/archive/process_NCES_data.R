### process NCES data downloaded on 9/12/2024 from https://nces.ed.gov/ccd/elsi/tableGenerator.aspx
# enrollment saved on ELSI as table id 650898
# finance saved on ESLI as table id 650899
# school-level school lunch and school id table is saved as 650929.

library(tidyverse)

### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") |> 
  select(-state)

### import raw data
raw_enroll <- read_csv("data/raw/nces/ELSI_csv_export_6386175644790434446515.csv", 
                       na = c("†", "‡", "–"), skip = 5)

raw_finance <- read_csv("data/raw/nces/ELSI_csv_export_6386175737162797218403.csv", 
                       na = c("†", "‡", "–"), skip = 5)

raw_hs_enroll <- read_csv("data/raw/nces/high_school_enrollment_23/ELSI_csv_export_6386537694419276564522.csv", 
                        na = c("†", "‡", "–"), skip = 5)

hs_enroll <- raw_hs_enroll |> 
  rename(district_id = `Agency ID - NCES Assigned [District] Latest available year`,
         hs_enroll_23 = `Grades 9-12 Students [District] 2022-23`) |> 
  filter(district_id != "") |> 
  select(district_id, hs_enroll_23) 

### process enrollment
enroll <- raw_enroll |> 
  rename(district = `Agency Name`,
         state =`State Name [District] Latest available year`,
         district_id = `Agency ID - NCES Assigned [District] Latest available year`,
         website = `Web Site URL [District] 2022-23`,
         enroll_23 = `Total Students All Grades (Excludes AE) [District] 2022-23`,
         enroll_native_23 = `American Indian/Alaska Native Students [District] 2022-23`,
         aapi_enroll_23 = `Asian or Asian/Pacific Islander Students [District] 2022-23`,
         latinx_enroll_23 = `Hispanic Students [District] 2022-23`,
         black_enroll_23 = `Black or African American Students [District] 2022-23`,
         white_enroll_23 = `White Students [District] 2022-23`,
         hawpi_enroll_23 = `Nat. Hawaiian or Other Pacific Isl. Students [District] 2022-23`,
         two_plus_race_enroll_23 = `Two or More Races Students [District] 2022-23`,
         student_teacher_ratio_23 = `Pupil/Teacher Ratio [District] 2022-23`,
         guidance_counselors_23 = `Total Guidance Counselors [District] 2022-23`,
         psychologists_23 = `School Psychologist [District] 2022-23`,
         support_staff_23 = `Student Support Services Staff (w/o Psychology) [District] 2022-23`) |> 
  filter(district_id != "") |>
  select(district, state, district_id, website, enroll_23, enroll_native_23, aapi_enroll_23, 
         latinx_enroll_23, black_enroll_23, white_enroll_23, hawpi_enroll_23, two_plus_race_enroll_23, 
         student_teacher_ratio_23, guidance_counselors_23, psychologists_23, support_staff_23) |> 
  left_join(hs_enroll, by = "district_id") |> 
  filter(enroll_23 > 0) |> 
  mutate(guidance_counselors_per_student_23 = round(guidance_counselors_23/enroll_23, 2),
         students_per_guidance_couselor_23 = ifelse(guidance_counselors_23 > 0, 
                                                 round(enroll_23/guidance_counselors_23, 2),
                                                 0),
         hs_students_per_guidance_couselor_23 = ifelse(guidance_counselors_23 > 0, 
                                                    round(hs_enroll_23/guidance_counselors_23, 2),
                                                    0),
         state = str_to_title(state))


### process finance data
finance <- raw_finance |> 
  rename(district_id = `Agency ID - NCES Assigned [District] Latest available year`,
         expend_per_pupil_21 = `Total Expenditures (TOTALEXP) per Pupil (V33) [District Finance] 2020-21`,
         instruct_expend_per_pupil_21 = `Total Current Expenditures - Instruction (TCURINST) per Pupil (V33) [District Finance] 2020-21`) |> 
  select(district_id, expend_per_pupil_21, instruct_expend_per_pupil_21)

                   
### combine data and calculate the Thiel index
nces_data <- enroll |> 
  select(state, district, district_id, enroll_23:two_plus_race_enroll_23, student_teacher_ratio_23, 
         hs_students_per_guidance_couselor_23) |> 
  left_join(finance, by = "district_id") |> 
  right_join(sds_list, by = "district_id", "state") |> 
  mutate(total_race = enroll_native_23 + aapi_enroll_23 + latinx_enroll_23 + black_enroll_23 + white_enroll_23 + hawpi_enroll_23 + two_plus_race_enroll_23,
    pct_native_23 = round(enroll_native_23/total_race, 3),
         pct_aapi_23 = round(aapi_enroll_23/total_race, 3),
         pct_latinx_23 = round(latinx_enroll_23/total_race, 3),
         pct_black_23 = round(black_enroll_23/total_race, 3),
         pct_white_23 = round(white_enroll_23/total_race, 3),
         pct_hawpi_23 = round(hawpi_enroll_23/total_race, 3),
         pct_two_plus_23 = round(two_plus_race_enroll_23/total_race, 3)) |> 
  rename(district = district.y) |> 
  select(-district.x) |> 
  select(district_id, district, enroll_23:expend_per_pupil_21, pct_native_23:pct_two_plus_23) 

write_csv(nces_data, "data/processed/nces_data.csv")


######## school data

#### Getting data from ELSI to get enrollments by grade and race for AP
# Your saved table id is 651239.

raw_nces_enroll <- read_csv("data/raw/nces/school_enrollment_by_grade_race/ELSI_csv_export_6386512574599202106968.csv", 
                            na = c("†", "‡", "–"), skip = 6)

nces_school_info <- raw_nces_enroll |> 
  rename(district_id = `Agency ID - NCES Assigned [Public School] Latest available year`,
         school_id = `School ID - NCES Assigned [Public School] Latest available year`,
         enroll21_nces = `Total Students All Grades (Includes AE) [Public School] 2020-21`,
         # enroll_no_ae = `Total Students All Grades (Excludes AE) [Public School] 2020-21`,
         school_name = `School Name`,
         school_type = `School Type [Public School] 2020-21`,
         school_level = `School Level (SY 2017-18 onward) [Public School] 2020-21`) |> 
  select(district_id, school_id, school_name, school_type, school_level, enroll21_nces) |> 
  filter(school_type == "1-Regular school")

write_csv(nces_school_info, "data/processed/nces_school_type_info.csv")

### use this data to create a high school to district key for Social Capital atlas data, and to create school lunch data     

high_schools_reg <- nces_school_info |> 
  filter(school_level == "High")


write_csv(high_schools_reg, "data/processed/nces_regular_high_schools.csv")

# raw_schools <- read_csv("data/raw/nces/ELSI_csv_export_6386218878841238957757.csv", 
#                         na = c("†", "‡", "–"), skip = 5)

### use this data to create a high school to district key for Social Capital atlas data, and to create school lunch data      

# high_schools <- raw_schools |> 
#   rename(school = `School Name`,
#          school_id = `School ID - NCES Assigned [Public School] Latest available year`,
#          district = `Agency Name [Public School] 2021-22`,
#          district_id = `Agency ID - NCES Assigned [Public School] Latest available year`,
#          level = `School Level (SY 2017-18 onward) [Public School] 2021-22`) |> 
#   select(school, school_id, district_id, level) |> 
#   filter(!is.na(school_id)) |> 
#   filter(level == "High") |> 
#   right_join(sds_list, by = "district_id")
# 
# write_csv(high_schools, "data/processed/nces_high_schools.csv")

### frl
frl_school <- raw_schools |> 
  rename(school = `School Name`,
         school_id = `School ID - NCES Assigned [Public School] Latest available year`,
         district = `Agency Name [Public School] 2021-22`,
         district_id = `Agency ID - NCES Assigned [Public School] Latest available year`,
         level = `School Level (SY 2017-18 onward) [Public School] 2021-22`,
         enroll = `Total Students All Grades (Excludes AE) [Public School] 2021-22`,
         frl_enroll = `Free and Reduced Lunch Students [Public School] 2021-22`,
         charter = `Charter School [Public School] 2021-22`) |> 
  filter(!is.na(school_id)) |> 
  select(school, school_id, charter, district_id, level, enroll, frl_enroll) |> 
  filter(frl_enroll > 0) 

frl_district <- frl_school |> 
  group_by(district_id) |> 
  summarise(school = n(),
            enroll_d = sum(enroll),
            frl_enroll_d = sum(frl_enroll)) |> 
  mutate(pct_frl = round(frl_enroll_d/enroll_d, 3))

write_csv(frl_district, "data/processed/nces_pct_frl.csv")
  