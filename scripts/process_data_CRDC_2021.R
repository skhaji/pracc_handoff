# explore and process CRDC data

# data user manual - https://civilrightsdata.ed.gov/assets/downloads/Public-Use-Data-File-Manual-2015-16.pdf

library(tidyverse)
options(scipen = 999)


### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") |> 
  select(district_id)

### import processed district-level nces data
nces_enroll <- read_csv("data/processed/nces_data.csv")

raw_crdc_lea <- read_csv("data/raw/2020-21-crdc-data/CRDC/LEA/LEA Characteristics.csv")

# raw_crdc_lea_1718 <- read_csv("data/raw/2020-21-crdc-data/CRDC/LEA/LEA Characteristics.csv")

## Survey sent to LEAs - https://civilrightsdata.ed.gov/assets/downloads/2020-21-crdc-lea-form.pdf
## Civil Rights Coordinator
# • Indicate whether the LEA had appointed one or more responsible employees to coordinate efforts to
# comply with and carry out its responsibilities under federal law prohibiting discrimination against students
# and others on the basis of sex, race/color/nation origin, and/or disability.
# • Civil rights coordinators may be part-time or full-time.

### Harassment and Bullying Policy
# Did the LEA have a written policy (or policies) prohibiting
# discriminatory harassment or bullying of students on the basis of sex, race/color/national origin,
# and disability? 

# crdc <- raw_crdc_lea |> 
#   rename(state = LEA_STATE,
#          district_id = LEAID,
#          district = LEA_NAME,
#          civil_rights_coord_race = LEA_CRCOORD_RAC_IND,
#          civil_rights_coord_sex = LEA_CRCOORD_SEX_IND,
#          civil_rights_coord_dis = LEA_CRCOORD_DIS_IND,
#          deseg_plan = LEA_DESEGPLAN,
#          hb_policy = LEA_HBPOLICY_IND,
#          hb_url = LEA_HBPOLICY_URL) |> 
#   select(state, district_id, district, civil_rights_coord_sex:civil_rights_coord_dis,
#          deseg_plan:hb_url) |> 
#   right_join(sds_list, by = "district_id") 

#### none of this is really that useful --- on to the school data



#### School-level data

### import nces data about type and level of school from nces
reg_school_nces <- read_csv("data/processed/nces_school_type_info.csv") |> 
  select(-school_type) |> 
  filter(school_level != "Prekindergarten") |> 
  filter(!grepl("CLOSED", school_name))

### create HS flag
raw_school_characteristics <- read_csv("data/raw/2020-21-crdc-data/CRDC/School/School Characteristics.csv")

hs_flag <- raw_school_characteristics |> 
  select(COMBOKEY, SCH_NAME, SCH_GRADE_G09, SCH_GRADE_G10, SCH_GRADE_G11, SCH_GRADE_G12) |> 
  filter(SCH_GRADE_G09 == "Yes" | SCH_GRADE_G10 == "Yes" | SCH_GRADE_G11 == "Yes" | SCH_GRADE_G12 == "Yes") |> 
  rename(school_id = COMBOKEY) |> 
  mutate(crdc_9_12 = "Yes")

### list of schools where level = "High" from NCES
nces_reg_high_schools <- read_csv("data/processed/nces_regular_high_schools.csv")

high_schools <- nces_reg_high_schools |> 
  full_join(hs_flag, by = "school_id")

# https://civilrightsdata.ed.gov/assets/downloads/2020-21-crdc-school-form.pdf

### CRDC enrollment
raw_school_enrollment <- read_csv("data/raw/2020-21-crdc-data/CRDC/School/Enrollment.csv")

schools_enroll <- raw_school_enrollment |> 
  select(LEAID:COMBOKEY, SCH_ENR_HI_M:TOT_ENR_F) |> 
  rename(district_id = LEAID,
         district = LEA_NAME,
         school_id = COMBOKEY) |> 
  right_join(reg_school_nces, by = c("school_id", "district_id")) |> 
  mutate(across(SCH_ENR_HI_M:TOT_ENR_F, ~ ifelse(.x == -9 | .x == -11 , NA, .x))) |> 
  mutate(enroll21 = TOT_ENR_M + TOT_ENR_F,
         enroll_latinx21 = SCH_ENR_HI_M + SCH_ENR_HI_F,
         enroll_native21 = SCH_ENR_AM_M + SCH_ENR_AM_F,
         enroll_asian21 = SCH_ENR_AS_M + SCH_ENR_AS_F,
         enroll_hawpi21 = SCH_ENR_HP_M + SCH_ENR_HP_F,
         enroll_black21 = SCH_ENR_BL_M + SCH_ENR_BL_M,
         enroll_white21 = SCH_ENR_WH_M + SCH_ENR_WH_F,
         enroll_two_plus21 = SCH_ENR_TR_M + SCH_ENR_TR_F,
         enroll_diff = enroll21 - enroll21_nces) |>
  select(enroll_diff, district_id, district, school_id, school_name:enroll_two_plus21)
  

district_enroll <- schools_enroll |> 
  group_by(district_id) |> 
  summarise(schools = n(),
            district = first(district),
            d_enroll21 = sum(enroll21, na.rm = T),
            d_enroll_latinx21 = sum(enroll_latinx21, na.rm = T),
            d_enroll_native21 = sum(enroll_native21, na.rm = T),
            d_enroll_asian21 = sum(enroll_asian21, na.rm = T),
            d_enroll_hawpi21 = sum(enroll_hawpi21, na.rm = T),
            d_enroll_black21 = sum(enroll_black21, na.rm = T),
            d_enroll_white21 = sum(enroll_white21, na.rm = T),
            d_enroll_two_plus21 = sum(enroll_two_plus21, na.rm = T)) 

### district enrollment commparison with nces and crdc
# district_check <- district_enroll |> 
#   right_join(sds_list, by = "district_id")  |> 
#   rename_with(.fn = ~paste0(.x, "_crdc"), .cols = d_enroll21:d_enroll_two_plus21) |> 
#   left_join(nces_enroll, "district_id") |> 
#   mutate(enroll_diff = d_enroll21_crdc - enroll_23) |> 
#   select(enroll_diff, everything()) 

# |> 
#   right_join(voucher_areas, by = "district_id")


### suspensions
raw_school_suspensions <- read_csv("data/raw/2020-21-crdc-data/CRDC/School/Suspensions.csv")

schools_oos <- raw_school_suspensions |> 
  rename(district_id = LEAID,
         district = LEA_NAME,
         school_id = COMBOKEY) |> 
  mutate(across(SCH_DISCWODIS_SINGOOS_HI_M:SCH_DISCWODIS_MULTOOS_LEP_F, ~ ifelse(.x < 0, 0, .x))) |> 
  mutate(oos_latinx21 = SCH_DISCWODIS_SINGOOS_HI_M + SCH_DISCWODIS_SINGOOS_HI_F + SCH_DISCWODIS_MULTOOS_HI_M + SCH_DISCWODIS_MULTOOS_HI_F,
         oos_native21 = SCH_DISCWODIS_SINGOOS_AM_M + SCH_DISCWODIS_SINGOOS_AM_F + SCH_DISCWODIS_MULTOOS_AM_M + SCH_DISCWODIS_MULTOOS_AM_F,
         oos_asian21 = SCH_DISCWODIS_SINGOOS_AS_M + SCH_DISCWODIS_SINGOOS_AS_F + SCH_DISCWODIS_MULTOOS_AS_M + SCH_DISCWODIS_MULTOOS_AS_F,
         oos_hawpi21 = SCH_DISCWODIS_SINGOOS_HP_M + SCH_DISCWODIS_SINGOOS_HP_F + SCH_DISCWODIS_MULTOOS_HP_M + SCH_DISCWODIS_MULTOOS_HP_F,
         oos_black21 = SCH_DISCWODIS_SINGOOS_BL_M + SCH_DISCWODIS_SINGOOS_BL_F + SCH_DISCWODIS_MULTOOS_BL_M + SCH_DISCWODIS_MULTOOS_BL_F,
         oos_white21 = SCH_DISCWODIS_SINGOOS_WH_M + SCH_DISCWODIS_SINGOOS_WH_F + SCH_DISCWODIS_MULTOOS_WH_M + SCH_DISCWODIS_MULTOOS_WH_F,
         oos_twoplus21 = SCH_DISCWODIS_SINGOOS_TR_M + SCH_DISCWODIS_SINGOOS_TR_F + SCH_DISCWODIS_MULTOOS_TR_M + SCH_DISCWODIS_MULTOOS_TR_F,
         oos_lep21 = SCH_DISCWODIS_SINGOOS_LEP_M + SCH_DISCWODIS_SINGOOS_LEP_F + SCH_DISCWODIS_MULTOOS_LEP_M + SCH_DISCWODIS_MULTOOS_LEP_F,
         oos21 = TOT_DISCWODIS_SINGOOS_M + TOT_DISCWODIS_SINGOOS_F + TOT_DISCWODIS_MULTOOS_M + TOT_DISCWODIS_MULTOOS_F) |> 
  select(district_id, district, school_id, oos_latinx21:oos21) |> 
  right_join(reg_school_nces |> select(school_id, school_level), by = "school_id") 

district_oos <- schools_oos |> 
  group_by(district_id) |> 
  summarise(schools_with_oos = n(),
            district = first(district),
            d_oos = sum(oos21, na.rm = T),
            d_oos_latinx = sum(oos_latinx21, na.rm = T),
            d_oos_native = sum(oos_native21, na.rm = T),
            d_oos_asian = sum(oos_asian21, na.rm = T),
            d_oos_hawpi = sum(oos_hawpi21, na.rm = T),
            d_oos_black = sum(oos_black21, na.rm = T),
            d_oos_white = sum(oos_white21, na.rm = T),
            d_oos_twoplus = sum(oos_twoplus21, na.rm = T)) |> 
  left_join(district_enroll |> select(-district), by = "district_id") |> 
  mutate(pct_oos_d = round(d_oos/d_enroll21, 2),
         pct_latinx_oos_d = round(d_oos_latinx/d_enroll_latinx21, 2),
         pct_native_oos_d = round(d_oos_native/d_enroll_native21, 2),
         pct_asian_oos_d = round(d_oos_asian/d_enroll_asian21, 2),
         pct_hawpi_oos_d = round(d_oos_hawpi/d_enroll_hawpi21, 2),
         pct_black_oos_d = round(d_oos_black/d_enroll_black21, 2),
         pct_white_oos_d = round(d_oos_white/d_enroll_white21, 2),
         pct_twoplus_oos_d = round(d_oos_twoplus/d_enroll_two_plus21, 2)) |> 
  mutate(across(pct_oos_d:pct_twoplus_oos_d, ~ ifelse(is.nan(.x), NA, .x))) |>
  mutate(across(pct_oos_d:pct_twoplus_oos_d, ~ ifelse(is.infinite(.x), NA, .x))) |>
  right_join(sds_list, by = "district_id") |> 
  select(district_id, schools, schools_with_oos, pct_oos_d:pct_twoplus_oos_d)


## Advanced Placement
raw_ap <- read_csv("data/raw/2020-21-crdc-data/CRDC/School/Advanced Placement.csv")

### process schools data to calculate the number of students enrolled in at least one ap course
### and the number of students in a high school, secondary school, or school with AP
schools_ap <- raw_ap |> 
  rename(district_id = LEAID,
         district = LEA_NAME,
         school_id = COMBOKEY,
         state = LEA_STATE,
         ap_classes = SCH_APCOURSES,
         ap_latinx_m = SCH_APENR_HI_M,
         ap_latinx_f = SCH_APENR_HI_F,
         ap_native_m = SCH_APENR_AM_M,
         ap_native_f = SCH_APENR_AM_F,
         ap_asian_m = SCH_APENR_AS_M,
         ap_asian_f = SCH_APENR_AS_F,
         ap_hawpi_m = SCH_APENR_HP_M,
         ap_hawpi_f = SCH_APENR_HP_F,
         ap_black_m = SCH_APENR_BL_M,
         ap_black_f = SCH_APENR_BL_F,
         ap_white_m = SCH_APENR_WH_M,
         ap_white_f = SCH_APENR_WH_F,
         ap_twoplus_m = SCH_APENR_TR_M,
         ap_twoplus_f = SCH_APENR_TR_F,
         ap_m = TOT_APENR_M,
         ap_f = TOT_APENR_F,
         ap_ind = SCH_APENR_IND) |> 
  select(state, district_id: ap_f) |> 
  right_join(reg_school_nces, by = c("school_id", "district_id")) |> 
  mutate(across(ap_ind:ap_f, ~ ifelse(.x == -9 | .x == -11 , NA, .x))) |>
  mutate(ap_enroll = ap_m + ap_f,
         ap_enroll_latinx = ap_latinx_m + ap_latinx_f,
         ap_enroll_native = ap_native_m + ap_native_f,
         ap_enroll_asian = ap_asian_m + ap_asian_f,
         ap_enroll_hawpi = ap_hawpi_m + ap_hawpi_f,
         ap_enroll_black = ap_black_m + ap_black_f,
         ap_enroll_white = ap_white_m + ap_white_f,
         ap_enroll_twoplus = ap_twoplus_m + ap_twoplus_f) |> 
  select(state, district_id, district, school_id, SCH_NAME, school_level, 
         ap_ind, ap_classes, enroll21_nces:ap_enroll_twoplus) |> 
  left_join(schools_enroll |> 
              select(-school_name, -district, -enroll_diff, -enroll21_nces, -school_level), 
            by = c("district_id", "school_id")) |> 
  filter(school_level == "High" | school_level == "Secondary" | ap_ind == "Yes") 
        

district_ap <- schools_ap |> 
  group_by(district_id) |> 
  summarise(state = first(state),
            high_schools = n(),
            hs_with_ap = n(),
            district = first(district),
            d_ap_enroll = sum(ap_enroll, na.rm = T),
            d_ap_enroll_latinx = sum(ap_enroll_latinx, na.rm = T),
            d_ap_enroll_native = sum(ap_enroll_native, na.rm = T),
            d_ap_enroll_asian = sum(ap_enroll_asian, na.rm = T),
            d_ap_enroll_hawpi = sum(ap_enroll_hawpi, na.rm = T),
            d_ap_enroll_black = sum(ap_enroll_black, na.rm = T),
            d_ap_enroll_white = sum(ap_enroll_white, na.rm = T),
            d_ap_enroll_twoplus = sum(ap_enroll_twoplus, na.rm = T),
            d_hs_enroll = sum(enroll21, na.rm = T),
            d_hs_enroll_latinx = sum(enroll_latinx21, na.rm = T),
            d_hs_enroll_native = sum(enroll_native21, na.rm = T),
            d_hs_enroll_asian = sum(enroll_asian21, na.rm = T),
            d_hs_enroll_hawpi = sum(enroll_hawpi21, na.rm = T),
            d_hs_enroll_black = sum(enroll_black21, na.rm = T),
            d_hs_enroll_white = sum(enroll_white21, na.rm = T),
            d_hs_enroll_twoplus = sum(enroll_two_plus21, na.rm = T)) |> 
  mutate(pct_ap_enroll_d = round(d_ap_enroll/d_hs_enroll, 2),
         pct_latinx_ap_enroll_d = round(d_ap_enroll_latinx/d_hs_enroll_latinx, 2),
         pct_native_ap_enroll_d = round(d_ap_enroll_native/d_hs_enroll_native, 2),
         pct_asian_ap_enroll_d = round(d_ap_enroll_asian/d_hs_enroll_asian, 2),
         pct_hawpi_ap_enroll_d = round(d_ap_enroll_hawpi/d_hs_enroll_hawpi, 2),
         pct_black_ap_enroll_d = ifelse(d_ap_enroll_black/d_hs_enroll_black > 1, 1.00, round(d_ap_enroll_black/d_hs_enroll_black, 2)),
         pct_white_ap_enroll_d = round(d_ap_enroll_white/d_hs_enroll_white, 2),
         pct_twoplus_ap_enroll_d = round(d_ap_enroll_twoplus/d_hs_enroll_twoplus, 2)) |> 
  mutate(across(pct_ap_enroll_d:pct_twoplus_ap_enroll_d, ~ ifelse(is.nan(.x), NA, .x))) |>
  mutate(across(pct_ap_enroll_d:pct_twoplus_ap_enroll_d, ~ ifelse(is.infinite(.x), NA, .x))) |>
  right_join(sds_list, by = "district_id") 


district_crdc_data <- district_ap |> 
  left_join(district_oos, by = "district_id") |>
  mutate(pct_hs_with_ap = round(hs_with_ap/high_schools, 3)) |> 
  select(district_id, district, high_schools, hs_with_ap, pct_hs_with_ap, d_ap_enroll:pct_twoplus_oos_d)

write_csv(district_crdc_data, "data/processed/crdc_data_21.csv")

