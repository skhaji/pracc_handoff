
### create full school district dataset

library(tidyverse)
library(openxlsx)
library(readxl)
library(sf)
library(jsonlite)

options(scipen = 999)

##### import processed data and select variables
voucher_areas <- read_csv("data/final/all_sds_in_tool.csv") |> 
  mutate(district_id = as.character(district_id))

included_areas <- sort(unique(voucher_areas$voucher_area))

frl_district <- read_csv("data/processed/nces_pct_frl.csv") |> 
  select(district_id, pct_frl)

# nces_data <- read_csv("data/processed/nces_data.csv") |> 
#   select(district_id, district, enroll_23, student_teacher_ratio_23:expend_per_pupil_21, pct_native_23:pct_two_plus_23) |> 
#   mutate(pct_bipoc_23 = 1-pct_white_23)

nces_data <- read_csv("data/processed/nces_data_24.csv") |> 
  select(district_id, district, enroll_24, student_teacher_ratio_24:expend_per_pupil_22, pct_native_24:pct_two_plus_24) |> 
  mutate(pct_bipoc_24 = 1-pct_white_24)

crdc_data <- read_csv("data/processed/crdc_data_21.csv") |> 
  select(-district)

edge_data <- read_csv("data/processed/edge_data_2018_22.csv") |> 
  select(-Geography)

friendship_data <- read_csv("data/processed/sce_cross_class_friendships_data_22.csv") |> 
  select(district_id, pct_cc_friends)

theils <- read_csv("data/processed/theils_segregation_index.csv") |> 
  mutate(theils_race_cat_desc = paste0(theils_race_cat, " (", round(theils_H_race, 2), ")"),
         theils_frl_cat_desc = paste0(theils_frl_cat, " (", round(theils_H_frl, 2), ")"))

normalized_exposure <- read_csv("data/processed/normalized_exposure_22.csv") |> 
  select(district_id, ns_wht_blk:ns_frl_nfr)

# isolation <- read_csv("data/processed/isolation_22.csv") |> 
#   select(district_id, ss_wht_blk:ss_frl_nfr)
# 
# dissimilarity <- read_csv("data/processed/dissimilarity_22.csv") |> 
#   select(district_id, ds_wht_blk:ds_frl_nfr)

performance_21 <- read_excel("data/processed/performance_21.xlsx") 

ed_race <- read_csv("data/processed/educator_diversity.csv") |> 
  mutate(district_id = as.character(district_id))

ecd_test_trend <- read_csv("data/processed/achievement_trend_econ_distress_0919.csv") |> 
  rename(li_achieve_trend = ecd_gcs_mn_coh_eb) |> 
  mutate(li_achieve_trend = round(li_achieve_trend, 3))

# not including anymore
# qual <- read_csv("data/processed/qual_measures_test.csv") |> 
#   select(-Area, -District) |> 
#   mutate(district_id = as.character(district_id)) |> 
#   rename(cult_resp_curr	 = `Culturally responsive curriculum binary`,
#          cult_resp_curr_narr	 = `Culturally responsive curriculum narrative...5`,
#          aff_groups	= `Presence of student affinity groups binary`,
#          aff_groups_narr	= `Culturally responsive curriculum narrative...7`,
#          inclusion	 = `District Commitment to Inclusion binary`,
#          inclusion_narr	= `District Commitment to Inclusion narrative`,
#          board_profile = `School Board Profile`) |> 
#   mutate(inclusion = ifelse(district_id == "3605850", "Positive", inclusion))


#### data for all areas
sd_data <- nces_data |> 
  left_join(frl_district, by = "district_id") |>
  left_join(crdc_data, by = "district_id") |> 
  left_join(edge_data, by = "district_id") |> 
  left_join(performance_21, by = "district_id") |> 
  left_join(ecd_test_trend, by = "district_id") |> 
  left_join(friendship_data, by = "district_id") |> 
  left_join(theils, by = "district_id") |> 
  left_join(normalized_exposure, by = "district_id") |>
  left_join(ed_race, by = "district_id") |> 
  left_join(voucher_areas, by = "district_id") |> 
  #left_join(qual, by = "district_id") |> 
  select(district_id, district, voucher_area, everything()) |> 
  mutate(li_achieve_trend = round(li_achieve_trend, 4),
         pct_poverty = round(pct_poverty/100, 3),
         pct_employed_civilian = round(pct_employed_civilian/100, 3))

# write_csv(sd_data, "data/final/sd_quant_data_FULL.csv")

# data for Can
voucher_area_sd_data <- sd_data |> 
  filter(!is.na(voucher_area))

voucher_area_sd_data_small <- voucher_area_sd_data |>
  select(voucher_area, district_id, district, enroll_24,
         pct_black_24, pct_latinx_24, pct_aapi_24, pct_native_24, 
         pct_white_24, pct_hawpi_24, pct_two_plus_24, theils_race_cat, theils_frl_cat, theils_race_cat_desc, theils_frl_cat_desc, 
         pct_only_english, pct_other_language, pct_spanish_athome, pct_aspi_athome, pct_cc_friends, pct_poverty, 
         pct_frl, pct_employed_civilian, 
         grad_rate_21_cat, grad_rate_21_black_nh_cat, grad_rate_21_latinx_cat,
         grad_rate_21_aapi_cat, grad_rate_21_white_nh_cat, grad_rate_21_two_plus_cat, 
         grad_rate_21_cat_desc, grad_rate_21_black_nh_cat_desc, grad_rate_21_latinx_cat_desc,
         grad_rate_21_aapi_cat_desc, grad_rate_21_white_nh_desc, grad_rate_21_two_plus_cat_desc, 
         pct_oos_d, pct_black_oos_d, 
         pct_latinx_oos_d, pct_asian_oos_d, pct_white_oos_d, 
         pct_black_ap_enroll_d, pct_latinx_ap_enroll_d, 
         pct_asian_ap_enroll_d, pct_native_ap_enroll_d, pct_white_ap_enroll_d, 
         read_21_cat, read_21_black_nh_cat, read_21_latinx_cat, read_21_aapi_cat, 
         read_21_native_cat, read_21_two_plus_cat, read_21_white_nh_cat,
         read_21_cat_desc, read_21_black_nh_cat_desc, read_21_latinx_cat_desc, read_21_aapi_cat_desc, 
         read_21_native_cat_desc, read_21_two_plus_cat_desc, read_21_white_nh_cat_desc,
         math_21_cat, math_21_black_nh_cat, math_21_latinx_cat, math_21_aapi_cat, 
         math_21_native_cat, math_21_two_plus_cat, math_21_white_nh_cat,
         math_21_cat_desc, math_21_black_nh_cat_desc, math_21_latinx_cat_desc, math_21_aapi_cat_desc, 
         math_21_native_cat_desc, math_21_two_plus_cat_desc, math_21_white_nh_cat_desc,
         expend_per_pupil_22, li_achieve_trend, pct_black_educ, 
         pct_latinx_educ, pct_asian_educ, pct_native_educ, 
         pct_white_educ, student_teacher_ratio_24, 
         hs_students_per_guidance_couselor_24) |> 
         # cult_resp_curr:board_profile)  |> 
  mutate(district_id = as.numeric(district_id)) |> 
  mutate(pct_bipoc_educ = NA)

# tried to remove "suppressed but it breaks the report
# ,
#          across(
#            grad_rate_21_cat:grad_rate_21_two_plus_cat_desc,
#            ~na_if(., "suppressed")
#          ),
#          across(
#            read_21_cat:math_21_white_nh_cat_desc,
#            ~na_if(., "suppressed")
#          ))

         
#### You will need to replace this to be the filepath to your sdat folder
write_csv(voucher_area_sd_data_small, "sdat/sdat-app/static/data/voucher_sds_quant_data_small.csv")



