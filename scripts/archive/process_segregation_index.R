### test intra-district segregation

library(tidyverse)
library(sf)

### download from the Ed Opportunity Project: https://edopportunity.org/segregation/data/downloads/

### documentation: https://stacks.stanford.edu/file/druid:gm391gj1253/school_seg_documentation_1.0.pdf

# For example, a Black-White exposure value of 0.3 indicates that the average share of White
# students in a Black student’s school is 0.3 (considering only White and Black students as the total population).

# For example,a Hispanic-White normalized exposure value of 0.5 indicates that the proportion of Hispanic students in
# the average Hispanic student’s school is 50 percentage points higher than in the average White student’s
# school. (The normalized exposure measure ignores the presence of other groups aside from the racial
#          dyad of interest.) The normalized exposure index ranges from 0 to 1. A value of 0 implies no segregation
# — the two groups have equal exposure to one group (all schools have identical proportions of the two
#                                                    groups). A value of 1 implies complete segregation— the two groups have no exposure to one another
# (no Hispanic student attends a school with any White students, and vice versa). For more on the
# normalized exposure index, see our brief here. 

# [segregation measure][subunit]_[group1]_[group2]
# Segregation measures: x (exposure), s (isolation), n (normalized exposure), d (dissimilarity), h (information
#                                                                                                   theory), and z (racial-economic segregation)
# Subunit: s (between schools), d (between administrative districts, g (between geographic districts), and c
#                                  (between charter and non-charter schools)
#                                  Groups: wht (non-Hispanic White), blk (non-Hispanic Black), hsp (Hispanic), asn (non-Hispanic Asian,
#                                                                                                                   Pacific Islander, or Hawaiian Native), nam (Native American), min (blk+hsp+nam), was (wht+asn), nwh
#                                  (asn+blk+hsp+nam), flu (FL eligible), nfl (not FL eligible), frl (FRL eligible), and nfr (not FRL eligible)
#                                  The variable ns_wht_blk, for example, is White-Black normalized exposure between schools.

# Groups: wht (non-Hispanic White), blk (non-Hispanic Black), hsp (Hispanic), asn (non-Hispanic Asian,
#  Pacific Islander, or Hawaiian Native), nam (Native American), min (blk+hsp+nam), was (wht+asn), nwh
# (asn+blk+hsp+nam), flu (FL eligible), nfl (not FL eligible), frl (FRL eligible), and nfr (not FRL eligible)

raw_seg <- read_csv("data/raw/ed-opportunity/school_seg_lea_1.0_sh.csv")

### import list of school districts with geography (from EDGE)
sds_list <- read_csv("data/processed/all_school_districts_geo.csv") 

normalized_exposure_22 <- raw_seg |> 
  select(year:leaname, ns_wht_blk:ns_frl_nfr) |> 
  filter(year == 2022) |> 
  rename(district_id = leaid) |> 
  right_join(sds_list, by = "district_id")

isolation_22 <- raw_seg |> 
  select(year:leaname, ss_wht_blk:ss_frl_nfr) |> 
  filter(year == 2022) |> 
  rename(district_id = leaid) |> 
  right_join(sds_list, by = "district_id")

dissimilarity_22 <- raw_seg |> 
  select(year:leaname, ds_wht_blk:ds_frl_nfr) |> 
  filter(year == 2022) |> 
  rename(district_id = leaid) |> 
  right_join(sds_list, by = "district_id")

# Two-group isolation measures estimate the average proportion of one’s own group in a
# school (as a share of the two groups); a Black-White isolation value of 0.7 indicates that the average share
# of Black students in a Black student’s school is 0.7 (considering only White and Black students’
#                                                       populations).

write_csv(normalized_exposure_22, "data/processed/normalized_exposure_22.csv")
write_csv(isolation_22, "data/processed/isolation_22.csv")
write_csv(dissimilarity_22, "data/processed/dissimilarity_22.csv")

# nces_data <- read_csv("data/processed/nces_data.csv")
# 
# boston_sds_22 <- st_read("data/processed/Boston/sds_in_voucher_areas.geojson")
# 
# boston_nces_25pct <- boston_sds_22 |> 
#   left_join(nces_data, by = c("GEOID" = "district_id")) |> 
#   filter(pct_black_23 > .25 | pct_latinx_23 > .25 | pct_aapi_23 > .25 | pct_two_plus_23 > .25)
# 
# boston_nces_10pct <- boston_sds_22 |> 
#   left_join(nces_data, by = c("GEOID" = "district_id")) |> 
#   filter(pct_black_23 > .1 | pct_latinx_23 > .1 | pct_aapi_23 > .1 | pct_two_plus_23 > .1)

# |> 
#   mutate(pct_black_latinx = pct_black_23 + pct_latinx_23) |> 
#   filter(pct_black_latinx < .9)


