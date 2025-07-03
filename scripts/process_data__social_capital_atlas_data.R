### process data from Social Capital Explorer
# https://data.humdata.org/dataset/social-capital-atlas#

#### metadata: https://s3.us-east-1.amazonaws.com/hdx-production-filestore/resources/fbe5b0b9-e81c-41c7-a9f2-3ebf8212cf64/data_release_readme_31_07_2022_nomatrix.pdf?AWSAccessKeyId=AKIAXYC32WNAUK27T3AH&Signature=w8YWGIKGdTl4EtI7XDWsNtJCKx4%3D&Expires=1726603382

library(tidyverse)


raw_sce_hs <- read_csv("data/raw/social_capital_atlas_2022/social_capital_high_school.csv")

high_schools <- read_csv("data/processed/nces_high_schools_csv")

sce_hs <- raw_sce_hs |> 
  rename(school_id = high_school) |> 
  right_join(high_schools, by = "school_id") |> 
  select(state, district_id, district, school_id, high_school_name, ec_own_ses_hs, ec_own_ses_se_hs) |> 
  filter(!is.na(ec_own_ses_hs))

# Baseline definition of economic connectedness: two times the share
# of high-SES friends within three birth cohorts among low-SES individuals, averaged over all low-SES individuals in the school. See
# equations (1), (2), and (3) of Chetty et al. (2022a) for a formal definition. We estimate SES as in Supplementary Information B.1 of
# Chetty et al. (2022a). We add noise to protect privacy, as described
# in Section 3 of this document. This variable is used in Supplementary
# Information Figure 3A of Chetty et al. (2022b).
####This definition measures how connected low-SES students are to high-SES students in their school, 
#### based on their friendships. A higher value means more mixing between the two groups.

cross_class_district <- sce_hs |> 
  group_by(district_id) |> 
  summarise(high_schools = n(),
            mean_cc_friends = mean(ec_own_ses_hs, na.rm = T),
            max_cc_friends = max(ec_own_ses_hs, na.rm = T),
            min_cc_friends = min(ec_own_ses_hs, na.rm = T)) |> 
  mutate(pct_cc_friends = round(mean_cc_friends/2, 3))

write_csv(cross_class_district, "data/processed/sce_cross_class_friendships_data_22.csv")
