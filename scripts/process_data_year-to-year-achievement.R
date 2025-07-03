### year-to-year achievement

# https://stacks.stanford.edu/file/druid:cs829jn7849/SEDA_documentation_v5.0.pdf
# The poolsub files contain the average test
# score mean in math and in RLA (averaged across grades and years), the average “learning rate”
# across grades in math and in RLA, and the average “trend” in test scores across cohorts in math
# and in RLA, along with their standard errors. The pooled overall files contain the average test
# score mean (averaged across grades, years, and subjects), the average “learning rate” across
# grades, the average “trend” in test scores across cohorts, and the average difference between
# math and RLA test scores, along with their standard errors. In all files, estimates are reported for
# all students and by demographic subgroups. 

### using the seda_admindist_poolsub_gcs_5.0

# this is the best description of these variables
#  https://nda.nih.gov/data-structure/led_school_part_101

#### SUBGROUPS:
# 𝑟 = subgroup
# 𝑎𝑙𝑙 = all students
# 𝑎𝑠𝑛 = Asian
# 𝑏𝑙𝑘 = Black
# ℎ𝑠𝑝 = Hispanic
# 𝑚𝑡𝑟 = Multiracial
# 𝑛𝑎𝑚 = Native American
# 𝑤ℎ𝑡 = White
# 𝑓𝑒𝑚 = female
# 𝑚𝑎𝑙 = male
# 𝑒𝑐𝑑 = economically disadvantaged
# 𝑛𝑒𝑐 = not economically disadvantaged
# 𝑤𝑎𝑔 = White-Asian gap
# 𝑤𝑏𝑔 = White-Black gap
# 𝑤ℎ𝑔 = White-Hispanic gap
# 𝑤𝑚𝑔 = White-Multiracial gap
# 𝑤𝑛𝑔 = White-Native American gap
# 𝑚𝑓𝑔 = male-female gap
# 𝑛𝑒𝑔 =not economically disadvantaged-economically disadvantaged gap

library(tidyverse)
library(readxl)

options(scipen = 999)

#raw_seda_gcs_poolsub <- read_csv("data/raw/ed-opportunity/achievement/seda_admindist_poolsub_gcs_5.0_updated_20240319.csv")
raw_seda_gcs_pool <- read_csv("data/raw/ed-opportunity/achievement/seda_admindist_pool_gcs_5.0_updated_20240319.csv")

#raw_seda_cs <- read_csv("data/raw/ed-opportunity/achievement/seda_admindist_poolsub_cs_5.0_updated_20240319.csv")

# raw_seda_2023 <- read_csv("data/raw/ed-opportunity/achievement/seda2022_admindist_poolsub_gys_beta.csv")

# seda_codebook <- read_excel("data/raw/ed-opportunity/achievement/seda_codebook_cov_admindist_5.0.xlsx", 
#                                               skip = 1)
# 
# seda_pool_codebook <- read_excel("data/raw/ed-opportunity/achievement/seda_codebook_cov_admindist_5.0.xlsx", 
#                             sheet = "seda_cov_admindist_pool_5.0", 
#                             skip = 1)

### Trend in test scores by subject
## test scores increased an average of xxx grade levels each year in math: gcs_mn_coh_mth_eb
## test scores increased an average of xxx grade levels each year in reading: gcs_mn_coh_rla_eb
### Learning Rates by subject
## students learn xx% more/less each grade than the US average: 
##


### Trend in test scores 
## test scores increased an average of xxx grade levels each year: gcs_mn_coh_eb
### Learning Rates by subject
## students learn xx% more/less each grade than the US average: gcs_mn_grd_eb

### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") 

### will use the trend in test scores
ecd_gcs_pool <- raw_seda_gcs_pool |> 
  filter(subcat == "ecd") |> 
  filter(subgroup == "ecd") |> 
  select(sedaadmin, gcs_mn_coh_eb, gcs_mn_coh_eb_se) |>
  rename(district_id = sedaadmin,
         ecd_gcs_mn_coh_eb = gcs_mn_coh_eb,
         ecd_gcs_mn_coh_eb_se = gcs_mn_coh_eb_se) |> 
  mutate(district_id = as.character(str_pad(district_id, width = 7, pad = "0"))) 

ecd_trend <- ecd_gcs_pool |> 
  right_join(sds_list, by = "district_id") |> 
  select(-state, -district) 

write_csv(ecd_trend, "data/processed/achievement_trend_econ_distress_0919.csv")

# velma <- raw_seda |> 
#   filter(sedaadmin == "4031020")
# 
# shelby23 <- raw_seda_2023 |> 
#   filter(sedaadmin == "103030")
