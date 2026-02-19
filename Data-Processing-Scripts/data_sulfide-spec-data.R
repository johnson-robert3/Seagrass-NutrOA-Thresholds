#~~~
# Script to import and process spectrophotometer data for sulfide samples
#
#~~~

setwd("C:/Users/rajohnson6/Box/Projects/Mote Seagrass Thresholds/Mote Data")

library(tidyverse)

# need to run "data_S-std-curves" script first


#- Functions for processing raw spec data sheets

# Function to check standard concentrations for each run
check_stds = function(.dat, .std_curve) {
   
   .dat %>%
      filter(vial %in% c(paste0('L', c(2:40)))) %>%
      # correct for blank absorbance
      mutate(abs = abs_667 - (.dat %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean)) %>%
      # concentration
      mutate(stdS = calc_S_conc(abs, .std_curve),
             expected = parse_number(sample_id),
             diff = expected - stdS,
             perc_diff = (diff / expected) * 100) %>%
      print(n=Inf)
}

# Function to remove Zeros, Blanks, Standards, and Checks vials from data
rm_zbsc = function(.dat) {
   
   .dat %>%
      filter(!(sample_id=="Zero" | sample_id=="Blank"),
             !(str_detect(sample_id, pattern="chk")),
             !(sample_id %in% c(paste0('L', c(2:40)))))
}

# Function to process datasheet and calculate sulfide concentration in microcentrifuge vial
calc_vial_S = function(.processed, .raw, .std_curve) {
   
   data_flags = c(
      'L',  # sample absorbance too low, need to rerun w/ lower pre-color dilution 
      'H',  # sample absorbance too high, need to rerun w/ higher pre-color dilution
      # 'F',  # flocculent material in vial, sample not run
      'M'   # misc. issue, see Notes column for sample
   )
   
   .processed %>%
      # correct absorbance
      mutate(
         # for blanks
         abs_blk_corr = abs_667 - (.raw %>% filter(sample_id=="Blank") %>% pull(abs_667) %>% mean),
         # for post-color dilution
         abs_corr = abs_blk_corr * dilution_post) %>%
      # remove samples with data flags indicating issues 
      filter(!(flag %in% data_flags)) %>%
      # remove sample dupes
      filter(!(str_detect(sample_id, pattern="dup"))) %>%
      # sulfide concentration in microcentrifuge vial that diamine reagent was added to (units = uM)
      mutate(vial_S_uM = calc_S_conc(abs_corr, .std_curve)) %>%
      # for samples that were below detection limit, replace concentration with half the DL (DL=2uM, so replace with 1) 
      #  below DL means abs < 0.05 at dilution 1:1, or abs < 0.02 at dilution 1:2
      mutate(vial_S_uM = replace(vial_S_uM, flag %in% c("DL"), 1))
}
   


#--
# Spec Data
#--

#- Run 1: Week 2 samples -# 
# Standard curve to use: Dec. 2025

# raw data
raw_sulf_wk2 = read_csv("Sulfide spectrophotometer data/2025.12.18 - porewater sulfide_wk2.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_sulf_wk2, std_dec25)  # low end stds (L8) are reading low (30% off)...

# Pre-process data sheets, remove unnecessary data/rows
sulf_wk2 = rm_zbsc(raw_sulf_wk2)

# check agreement between sample dupes
sulf_wk2 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))

# Sulfide concentration in vials (units = uM)
sulf_wk2 = calc_vial_S(sulf_wk2, raw_sulf_wk2, std_dec25)


#- Run 2: Week 2 reruns and Week 3 samples -#
# Standard curve to use: Dec. 2025

# raw data
raw_sulf_wk3 = read_csv("Sulfide spectrophotometer data/2025.12.19 - porewater sulfide_wk2 reruns and wk3.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_sulf_wk3, std_dec25)  # high end stds (L40) are reading low (12% off)...

# Pre-process data sheets, remove unnecessary data/rows
sulf_wk3 = rm_zbsc(raw_sulf_wk3)

# check agreement between sample dupes
sulf_wk3 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))

# Sulfide concentration in vials (units = uM)
sulf_wk3 = calc_vial_S(sulf_wk3, raw_sulf_wk3, std_dec25)


#- Run 3: Week 6 samples -#
# Standard curve to use: Dec. 2025

# raw data
raw_sulf_wk6 = read_csv("Sulfide spectrophotometer data/2026.01.02 - porewater sulfide_wk6.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_sulf_wk6, std_dec25)  # looks good (L2 is a bit low)

# Pre-process data sheets, remove unnecessary data/rows
sulf_wk6 = rm_zbsc(raw_sulf_wk6)

# check agreement between sample dupes
sulf_wk6 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))

# Sulfide concentration in vials (units = uM)
sulf_wk6 = calc_vial_S(sulf_wk6, raw_sulf_wk6, std_dec25) %>%
   # remove the sample that was rerun (T1-H165; estimated based on color formation), but original was within the curve
   filter(!str_detect(notes, "disregard") | is.na(notes))


#- Run 4: Week 9 samples -#
# Standard curve to use: Dec. 2025

# raw data
raw_sulf_wk9 = read_csv("Sulfide spectrophotometer data/2026.01.20 - porewater sulfide_wk9.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_sulf_wk9, std_dec25)  # looks good 

# Pre-process data sheets, remove unnecessary data/rows
sulf_wk9 = rm_zbsc(raw_sulf_wk9)

# check agreement between sample dupes
sulf_wk9 %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))

# Sulfide concentration in vials (units = uM)
sulf_wk9 = calc_vial_S(sulf_wk9, raw_sulf_wk9, std_dec25)

   # View samples that were maybe too high at 1:1, and were then maybe too low at 1:20 dilutions
   sulf_wk9 %>% filter(flag %in% c("comp")) %>% mutate(scint_S_uM = vial_S_uM * dilution_pre) %>% print(n=Inf)
      # differences in calculated concentration were 17%, 28%, and 40% different depending on the dilution
      # need to rerun these 3 samples at a 1:5 pre_dilution (T1-H116-w9, T2-L159-w9, T4-H012-w9)
   
   ## probably want to rerun all wk9 samples that had an abs. below 0.10 at a dilution of 1:20; rerun at a dilution of 1:5
   raw_sulf_wk9 %>% filter(dilution_pre == 20 & abs_667 < 0.10 & !(str_detect(sample_id, pattern="dup"))) %>% print(n=Inf)
      # but don't need to rerun the 3 samples where the original (at 1:1 dilution) was within the curve (notes for these 3 at 1:20 say "disregard...")
   # sample ID list
   wk9_reruns = raw_sulf_wk9 %>% 
      filter(dilution_pre == 20 & abs_667 < 0.10 & !(str_detect(sample_id, pattern="dup"))) %>%
      filter(!str_detect(notes, "disregard") | is.na(notes)) %>%
      select(sample_id)
   

# remove sample IDs that need to be rerun from the calculated sulf_wk9 df
sulf_wk9 = sulf_wk9 %>% anti_join(wk9_reruns)


#- Run 5: Week 9 sample reruns -#
# Standard curve to use: Dec. 2025

# raw data
raw_sulf_wk9_reruns = read_csv("Sulfide spectrophotometer data/2026.02.06 - porewater sulfide_wk9_reruns.csv") %>%
   janitor::remove_empty(which = 'rows')

# check measured concentration of standards
check_stds(raw_sulf_wk9_reruns, std_dec25)  # a bit off at the low end (L2) 

# Pre-process data sheets, remove unnecessary data/rows
sulf_wk9_reruns = rm_zbsc(raw_sulf_wk9_reruns)

# check agreement between sample dupes
sulf_wk9_reruns %>% filter(str_detect(sample_id, "dup") | lead(str_detect(sample_id, "dup")))

# Sulfide concentration in vials (units = uM)
sulf_wk9_reruns = calc_vial_S(sulf_wk9_reruns, raw_sulf_wk9_reruns, std_dec25)
   # still missing two samples (T2-H151-w9, T2-H159-w9) that weren't included in the rerun for some reason...



#--
# Sulfide Concentration
#--

# Combine spec runs and calculate sulfide concentration (units = uM)
sulfide = bind_rows(sulf_wk2, sulf_wk3, sulf_wk6, sulf_wk9, sulf_wk9_reruns) %>%
   # correct measured sulfide concentration for any dilution prior to adding diamine reagent (units = uM)
   mutate(scint_S_uM = vial_S_uM * dilution_pre) %>%
   # for samples below DL at dilution 1:2 (abs < 0.02), vial_S becomes 2.0, but should be 1.0uM, replace conc with half the DL (DL=2uM)
   mutate(scint_S_uM = replace(scint_S_uM, flag %in% c("DL"), 1))




