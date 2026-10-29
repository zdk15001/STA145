# Reproduce the page's full-sample printouts in R.
# Needs the HCMST public data file and the haven and survey packages:
#   install.packages(c("haven", "survey"))
# Put the .dta file in your working directory (or change the path below).

library(haven)
library(survey)

raw <- zap_labels(read_dta("HCMST_2017_to_2022_small_public_version_2.2.dta"))

hcmst <- data.frame(
  # Q34 is coded 1 = excellent ... 5 = very poor; flip it so higher = better
  rel_quality     = ifelse(raw$w1_q34 %in% 1:5, 6 - raw$w1_q34, NA),
  how_met         = ifelse(raw$w1_either_internet_adjusted == 1, "online", "offline"),
  marital         = ifelse(raw$w1_partnership_status == 1, "married",
                    ifelse(raw$w1_partnership_status == 2, "not_married", NA)),
  living          = ifelse(raw$w1_q19 == 1, "together",
                    ifelse(raw$w1_q19 == 2, "apart", NA)),
  couple_type     = ifelse(raw$w1_same_sex_couple == 1, "same_sex", "different_sex"),
  met_friends     = ifelse(raw$w1_q24_met_through_friend == 1, "friends", "other_ways"),
  age_met         = raw$w1_age_when_met,
  years_to_couple = raw$w1_time_from_met_to_rel,
  years_to_movein = raw$w1_time_from_rel_to_cohab,
  age_gap         = abs(raw$w1_ppage - ifelse(raw$w1_q9 >= 18, raw$w1_q9, NA)),
  age             = raw$w1_ppage,
  app_use         = ifelse(raw$w1_otherdate_app_all == 2, "used_app",
                    ifelse(raw$w1_otherdate_app_all == 1, "no_app", NA)),
  rel_years       = ifelse(raw$w1_relate_duration_in2017_years >= 0,
                           raw$w1_relate_duration_in2017_years, NA),
  w1_weight_combo = raw$w1_weight_combo
)

# Unweighted (the Explore tab's R console)
t.test(rel_quality ~ how_met, data = hcmst)
t.test(rel_quality ~ marital, data = hcmst)
t.test(rel_quality ~ living, data = hcmst)
t.test(rel_quality ~ couple_type, data = hcmst)
t.test(rel_quality ~ met_friends, data = hcmst)
t.test(age_met ~ how_met, data = hcmst)
t.test(years_to_couple ~ how_met, data = hcmst)
t.test(years_to_movein ~ how_met, data = hcmst)
t.test(age_gap ~ couple_type, data = hcmst)
t.test(age_gap ~ marital, data = hcmst)
t.test(age ~ app_use, data = hcmst)
t.test(rel_years ~ how_met, data = hcmst)

# Weighted ("See weighted results!")
hcmst_design <- svydesign(ids = ~1, weights = ~w1_weight_combo, data = hcmst)
svyttest(rel_quality ~ how_met, design = hcmst_design)
svyttest(rel_quality ~ marital, design = hcmst_design)
svyttest(age_met ~ how_met, design = hcmst_design)
svyttest(age_gap ~ couple_type, design = hcmst_design)
