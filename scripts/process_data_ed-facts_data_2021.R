### process Ed-Facts achievement and graduation data

library(tidyverse)
library(openxlsx)


### import math, reading and graduation data by race and total
raw_math <- read_csv("data/raw/ed-facts/SY2021_FS175_DG583_LEA_data_files/SY2021_FS175_DG583_LEA.csv")
raw_reading <- read_csv("data/raw/ed-facts/SY2021_FS178_DG584_LEA_data_files/SY2021_FS178_DG584_LEA.csv")
raw_acgr <- read_csv("data/raw/ed-facts/SY2021_FS150_FS151_DG695_DG696_LEA_data_files/SY2021_FS150_FS151_DG695_DG696_LEA.csv")

### Graduation rates

grad_21 <- raw_acgr |> 
  rename(district_id = `NCES LEA ID`,
         graduation_rate = Value) |> 
  select(district_id, graduation_rate, Subgroup) |> 
  filter(district_id != "Not Applicable") %>%
  pivot_wider(., names_from = Subgroup, values_from = graduation_rate) |>
  rename(grad_rate_21 = `All Students in LEA`,
         grad_rate_21_native = `American Indian/Alaska Native/Native American`,
         grad_rate_21_aapi = `Asian/Pacific Islander`,
         grad_rate_21_black_nh = `Black (not Hispanic) African American`,
         grad_rate_21_latinx = `Hispanic/Latino`,
         grad_rate_21_two_plus = `Multicultural/Multiethnic/Multiracial/other`,
         grad_rate_21_white_nh = `White or Caucasian (not Hispanic)`) |> 
         # grad_rate_21_econ_dis = `Economically Disadvantaged`,
         # grad_rate_21_ell = `English Learner`,
         # grad_rate_21_disab = `Children with disabilities`) |> 
  select(district_id, grad_rate_21:grad_rate_21_white_nh) %>%
         # grad_rate_21_econ_dis, grad_rate_21_ell, grad_rate_21_disab) %>% 
  mutate(across(everything(), ~ifelse(. == "S", "Suppressed", .))) |> 
  mutate(across(c(grad_rate_21, grad_rate_21_native, grad_rate_21_aapi, 
                  grad_rate_21_black_nh, grad_rate_21_latinx, grad_rate_21_two_plus, 
                  grad_rate_21_white_nh),
                  # grad_rate_21_white_nh, grad_rate_21_econ_dis, grad_rate_21_ell,
                  # grad_rate_21_disab), 
                ~ ifelse(. == "<50%", "<=49%", .))) %>%
  mutate(across(c(grad_rate_21, grad_rate_21_native, grad_rate_21_aapi, 
                  grad_rate_21_black_nh, grad_rate_21_latinx, grad_rate_21_two_plus, 
                  grad_rate_21_white_nh),
                  # grad_rate_21_white_nh, grad_rate_21_econ_dis, grad_rate_21_ell,
                  # grad_rate_21_disab), 
                ~ case_when(
                  . == "Suppressed" ~ "suppressed",
                  str_detect(., "^[<>=]*[0-9]+%") ~ case_when(
                    as.numeric(str_remove_all(., "[%<>=]+")) <= 24 ~ "low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 25 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 49 ~ "mid-low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 50 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 74 ~ "mid-high",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 75 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 94 ~ "high",
                    as.numeric(str_remove_all(., "[%<>=]+")) > 94  ~ "excellent"
                  ),
                  str_detect(., "^[0-9]+-[0-9]+%") ~ case_when(
                    as.numeric(str_extract(., "^[0-9]+")) >= 0  & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 24 ~ "low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 25 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 49 ~ "mid-low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 50 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 74 ~ "mid-high",
                    as.numeric(str_extract(., "^[0-9]+")) >= 75 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 94 ~ "high",
                    as.numeric(str_extract(., "[0-9]+$")) > 94  ~ "excellent" 
                  ),
                  TRUE ~ NA_character_ 
                ),
                .names = "{.col}_cat"  # Add "_cat" to the new column names
  )) %>%
  mutate(across(ends_with("_cat"), ~ factor(., levels = c("low", "mid-low", "mid-high", "high", "excellent", "suppressed"))))

# test50 <- grad_rate_21 |>
#   # filter(is.na(grad_cat)) |>
#   select(grad_rate_21_econ_dis, grad_rate_21_econ_dis_cat) |>
#   distinct()


### 4th grade math

math_21 <- raw_math |> 
  rename(district_id = `NCES LEA ID`,
         math_4th = Value) |> 
  filter(`Age/Grade` == "Grade 4") |> 
  filter(!is.na(Subgroup)) |> 
  select(district_id, math_4th, Subgroup) %>%
  pivot_wider(., names_from = Subgroup, values_from = math_4th) |>
  rename(math_21 = `All Students in LEA`,
         math_21_native = `American Indian/Alaska Native/Native American`,
         math_21_aapi = `Asian/Pacific Islander`,
         math_21_black_nh = `Black (not Hispanic) African American`,
         math_21_latinx = `Hispanic/Latino`,
         math_21_two_plus = `Multicultural/Multiethnic/Multiracial/other`,
         math_21_white_nh = `White or Caucasian (not Hispanic)`) |> 
         # math_21_econ_dis = `Economically Disadvantaged`,
         # math_21_ell = `English Learner`,
         # math_21_homeless = `Homeless`) |> 
  select(district_id, math_21:math_21_white_nh) %>%
  # select(district_id, math_21:math_21_white_nh, math_21_econ_dis, 
  #        math_21_ell, math_21_homeless) %>% 
  mutate(across(everything(), ~ifelse(. == "S", "Suppressed", .))) |> 
  mutate(across(c(math_21, math_21_native, math_21_aapi, 
                  math_21_black_nh, math_21_latinx, math_21_two_plus, 
                  math_21_white_nh),
                  # math_21_white_nh, math_21_econ_dis, math_21_ell), 
                ~ ifelse(. == "<50%", "<=49%", .))) %>%
  mutate(across(c(math_21, math_21_native, math_21_aapi, 
                  math_21_black_nh, math_21_latinx, math_21_two_plus, 
                  math_21_white_nh),
                  # math_21_white_nh, math_21_econ_dis, math_21_ell), 
                ~ case_when(
                  . == "Suppressed" ~ "suppressed",
                  str_detect(., "^[<>=]*[0-9]+%") ~ case_when(
                    as.numeric(str_remove_all(., "[%<>=]+")) <= 24 ~ "low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 25 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 49 ~ "mid-low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 50 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 74 ~ "mid-high",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 75 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 94 ~ "high",
                    as.numeric(str_remove_all(., "[%<>=]+")) > 94  ~ "excellent"
                  ),
                  str_detect(., "^[0-9]+-[0-9]+%") ~ case_when(
                    as.numeric(str_extract(., "^[0-9]+")) >= 0  & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 24 ~ "low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 25 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 49 ~ "mid-low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 50 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 74 ~ "mid-high",
                    as.numeric(str_extract(., "^[0-9]+")) >= 75 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 94 ~ "high",
                    as.numeric(str_extract(., "[0-9]+$")) > 94  ~ "excellent" 
                  ),
                  TRUE ~ NA_character_ 
                ),
                .names = "{.col}_cat"  # Add "_cat" to the new column names
  )) %>%
  mutate(across(ends_with("_cat"), ~ factor(., levels = c("low", "mid-low", "mid-high", "high", "excellent", "suppressed"))))

### 4th grade reading

reading_21 <- raw_reading |> 
  rename(district_id = `NCES LEA ID`,
         read_4th = Value) |> 
  filter(`Age/Grade` == "Grade 4") |> 
  filter(!is.na(Subgroup)) |> 
  select(district_id, read_4th, Subgroup) %>%
  pivot_wider(., names_from = Subgroup, values_from = read_4th) |>
  rename(read_21 = `All Students in LEA`,
         read_21_native = `American Indian/Alaska Native/Native American`,
         read_21_aapi = `Asian/Pacific Islander`,
         read_21_black_nh = `Black (not Hispanic) African American`,
         read_21_latinx = `Hispanic/Latino`,
         read_21_two_plus = `Multicultural/Multiethnic/Multiracial/other`,
         read_21_white_nh = `White or Caucasian (not Hispanic)`) |> 
         # read_21_econ_dis = `Economically Disadvantaged`,
         # read_21_ell = `English Learner`,
         # read_21_homeless = `Homeless`) |> 
  # select(district_id, read_21:read_21_white_nh, read_21_econ_dis, 
  #        read_21_ell, read_21_homeless) %>% 
  select(district_id, read_21:read_21_white_nh) %>% 
  mutate(across(everything(), ~ifelse(. == "S", "Suppressed", .))) |> 
  mutate(across(c(read_21, read_21_native, read_21_aapi, 
                  read_21_black_nh, read_21_latinx, read_21_two_plus, 
                  read_21_white_nh), 
                  # read_21_white_nh, read_21_econ_dis, read_21_ell), 
                ~ ifelse(. == "<50%", "<=49%", .))) %>%
  mutate(across(c(read_21, read_21_native, read_21_aapi, 
                  read_21_black_nh, read_21_latinx, read_21_two_plus, 
                  # read_21_white_nh, read_21_econ_dis, read_21_ell), 
                  read_21_white_nh), 
                ~ case_when(
                  . == "Suppressed" ~ "suppressed",
                  str_detect(., "^[<>=]*[0-9]+%") ~ case_when(
                    as.numeric(str_remove_all(., "[%<>=]+")) <= 24 ~ "low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 25 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 49 ~ "mid-low",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 50 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 74 ~ "mid-high",
                    as.numeric(str_remove_all(., "[%<>=]+")) >= 75 &
                      as.numeric(str_remove_all(., "[%<>=]+")) <= 94 ~ "high",
                    as.numeric(str_remove_all(., "[%<>=]+")) > 94  ~ "excellent"
                  ),
                  str_detect(., "^[0-9]+-[0-9]+%") ~ case_when(
                    as.numeric(str_extract(., "^[0-9]+")) >= 0  & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 24 ~ "low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 25 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 49 ~ "mid-low",
                    as.numeric(str_extract(., "^[0-9]+")) >= 50 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 74 ~ "mid-high",
                    as.numeric(str_extract(., "^[0-9]+")) >= 75 & 
                      as.numeric(str_extract(., "^[0-9]+")) <= 94 ~ "high",
                    as.numeric(str_extract(., "[0-9]+$")) > 94  ~ "excellent" 
                  ),
                  TRUE ~ NA_character_ 
                ),
                .names = "{.col}_cat"  # Add "_cat" to the new column names
  )) %>%
  mutate(across(ends_with("_cat"), ~ factor(., levels = c("low", "mid-low", "mid-high", "high", "excellent", "suppressed"))))

performance <- grad_21 |> 
  left_join(math_21, by = "district_id") |> 
  left_join(reading_21, by = "district_id") |> 
  mutate(grad_rate_21_cat_desc = ifelse(grad_rate_21_cat == "suppressed", grad_rate_21_cat , paste(grad_rate_21_cat, "(", grad_rate_21, ")")),
         grad_rate_21_black_nh_cat_desc = ifelse(grad_rate_21_black_nh_cat == "suppressed", "suppressed", paste(grad_rate_21_black_nh_cat, "(", grad_rate_21_black_nh, ")")),
         grad_rate_21_latinx_cat_desc = ifelse(grad_rate_21_latinx_cat == "suppressed", "suppressed", paste(grad_rate_21_latinx_cat, "(", grad_rate_21_latinx, ")")),
         grad_rate_21_aapi_cat_desc = ifelse(grad_rate_21_aapi_cat == "suppressed", "suppressed", paste(grad_rate_21_aapi_cat, "(", grad_rate_21_aapi, ")")),
         grad_rate_21_white_nh_desc = ifelse(grad_rate_21_white_nh == "suppressed", "suppressed", paste(grad_rate_21_white_nh_cat, "(", grad_rate_21_white_nh, ")")),
         grad_rate_21_two_plus_cat_desc = ifelse(grad_rate_21_two_plus_cat == "suppressed", "suppressed", paste(grad_rate_21_two_plus_cat, "(", grad_rate_21_two_plus, ")")),
         read_21_cat_desc = ifelse(read_21_cat == "suppressed", "suppressed", paste(read_21_cat, "(", read_21, ")")),
         read_21_black_nh_cat_desc = ifelse(read_21_black_nh_cat == "suppressed", "suppressed", paste(read_21_black_nh_cat, "(", read_21_black_nh, ")")),
         read_21_latinx_cat_desc = ifelse(read_21_latinx_cat == "suppressed", "suppressed", paste(read_21_latinx_cat, "(", read_21_latinx, ")")),
         read_21_aapi_cat_desc = ifelse(read_21_aapi_cat == "suppressed", "suppressed", paste(read_21_aapi_cat, "(", read_21_aapi, ")")),
         read_21_native_cat_desc = ifelse(read_21_native_cat == "suppressed", "suppressed", paste(read_21_native_cat, "(", read_21_native, ")")),
         read_21_two_plus_cat_desc = ifelse(read_21_two_plus_cat == "suppressed", "suppressed", paste(read_21_two_plus_cat, "(", read_21_two_plus, ")")),
         read_21_white_nh_cat_desc = ifelse(read_21_white_nh_cat == "suppressed", "suppressed", paste(read_21_white_nh_cat, "(", read_21_white_nh, ")")),
         math_21_cat_desc = ifelse(math_21_cat == "suppressed", "suppressed", paste(math_21_cat, "(", math_21, ")")),
         math_21_black_nh_cat_desc = ifelse(math_21_black_nh_cat == "suppressed", "suppressed", paste(math_21_black_nh_cat, "(", math_21_black_nh, ")")),
         math_21_latinx_cat_desc = ifelse(math_21_latinx_cat == "suppressed", "suppressed", paste(math_21_latinx_cat, "(", math_21_latinx, ")")),
         math_21_aapi_cat_desc = ifelse(math_21_aapi_cat == "suppressed", "suppressed", paste(math_21_aapi_cat, "(", math_21_aapi, ")")),
         math_21_native_cat_desc = ifelse(math_21_native_cat == "suppressed", "suppressed", paste(math_21_native_cat, "(", math_21_native, ")")),
         math_21_two_plus_cat_desc = ifelse(math_21_two_plus_cat == "suppressed", "suppressed", paste(math_21_two_plus_cat, "(", math_21_two_plus, ")")),
         math_21_white_nh_cat_desc = ifelse(math_21_white_nh_cat == "suppressed", "suppressed", paste(math_21_white_nh_cat, "(", math_21_white_nh, ")")))

write.xlsx(performance, "data/processed/performance_21.xlsx")
