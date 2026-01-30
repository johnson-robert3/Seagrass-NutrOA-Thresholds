#~~~
# Script to import and clean datasets
#
# Project: Mote Seagrass Initiative (MSI25)
# By: R. Johnson
#~~~


# directory
setwd("C:/Users/rajohnson6/Box/Projects/Mote Seagrass Thresholds/Mote Data")

library(tidyverse)
library(lubridate)



#--
# Plant ID and Treatment Info
#--

# Raw data
plant_ids_raw = read_csv("MSI25_experiment_plantID_treatment_info.csv")

# Initial cleaning
plant_dat = plant_ids_raw %>%
   janitor::remove_empty(which="rows") %>%
   # drop plant IDs that didn't get used
   filter(notes!="plant ID not used" | is.na(notes))


#--
# Seagrass Leaf Morphometry
#--

# Raw data
morph_raw = read_csv("MSI25_experiment_leaf_morphometry.csv")
morph_wk5_raw = read_csv("MSI25_experiment_leaf_morphometry_wk5.csv")  # week 5 data w/ a length correction for all measurements

# Initial cleaning
morph = morph_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))

morph_wk5 = morph_wk5_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


# Combine week 5 data with rest of dataset after adding the length correction to all BL measurements
morphometry = morph %>%
   bind_rows(morph_wk5 %>%
                # add correction length to all BL measurements
                mutate(across(starts_with("length_cm"), ~ . + correction_cm)) %>%
                # remove correction length column
                select(-correction_cm))

rm(morph, morph_wk5)


#--
# Seagrass Leaf and Shoot Counts
#--

# Raw data
counts_raw = read_csv("MSI25_experiment_leaf_shoot_counts.csv")

# Initial cleaning
leaf_counts = counts_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date)) %>%
   # drop plant IDs that didn't get used
   filter(notes!="plant ID not used" | is.na(notes))


#--
# Seagrass Shoot Biomass
#--

# Raw data
shoot_biomass_raw = read_csv("MSI25_experiment_aboveground_shoot_biomass.csv")

# Initial cleaning
shoot_biomass = shoot_biomass_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# Seagrass Rhizome Biomass
#--

# Raw data
rhizome_biomass_raw = read_csv("MSI25_experiment_rhizome_biomass.csv")

# Initial cleaning
rhiz_biomass = rhizome_biomass_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# Seagrass Leaf Epiphytes
#--

# Raw data
epiphytes_raw = read_csv("MSI25_experiment_epiphyte_biomass.csv")

# Initial cleaning
epiphytes = epiphytes_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# Porewater Sample Data
#--

# Raw data
porewater_raw = read_csv("MSI25_experiment_porewater_samples.csv")

# Initial cleaning
porewater = porewater_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# Sediments
#--

# Raw data
sed_dbd_raw = read_csv("MSI25_experiment_sediment_bulk_density.csv")
sed_trs_raw = read_csv("MSI25_experiment_sediment_TRS_samples.csv")

# Initial cleaning
dbd_dat = sed_dbd_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))

trs_dat = sed_trs_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# pH in CAOS Tables
#--

# Raw data
table_ph_raw = read_csv("MSI25_experiment_table_pH.csv")

# Initial cleaning
table_ph = table_ph_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(week = paste0("w", week),
          date = mdy(date))


#--
# Nutrient Packs
#--

# Raw data
np_mass_raw = read_csv("MSI25_experiment_nutrient_pack_masses.csv")

# Initial cleaning
np_mass = np_mass_raw %>%
   janitor::remove_empty(which="rows") %>%
   mutate(date = mdy(date))

# Process
np_mass = np_mass %>%
   rename(pull_date = date) %>%
   mutate(burial_date = as_date("2025-08-29")) %>%
   relocate(treatment_nutrients, burial_date, pull_date, .after = table) %>%
   # calculations
   mutate(days_buried = as.numeric(pull_date - burial_date),       # number of days that nutr packs were buried
          mass_loss_g = starting_mass_g - final_mass_g,            # mass of nutrients lost during burial period
          perc_mass_loss = (mass_loss_g / starting_mass_g) * 100,  # percent mass lost during burial period
          mass_loss_rate = mass_loss_g / days_buried,              # rate of mass loss (g/day)
          perc_mass_loss_rate = perc_mass_loss / days_buried)      # rate mass loss (%/day)


#--
# HOBO Loggers
#--

# Raw data
hobo_t1_raw = read_csv("HOBO logger data/T1-22182670 2025-10-19 09_44_26 CDT.csv")
hobo_t2_raw = read_csv("HOBO logger data/T2-22182671 2025-10-19 09_45_24 CDT.csv")
hobo_t4_raw = read_csv("HOBO logger data/T4-22182675 2025-10-19 09_46_26 CDT.csv")
hobo_t5_raw = read_csv("HOBO logger data/T5-22182669 2025-10-19 09_47_01 CDT.csv")

# Clean, combine, and process
temp_dat = bind_rows(hobo_t1_raw %>% janitor::clean_names() %>% select(date_time_edt, temperature_c, light_lux) %>% mutate(table = "T1"),
                     hobo_t2_raw %>% janitor::clean_names() %>% select(date_time_edt, temperature_c, light_lux) %>% mutate(table = "T2"),
                     hobo_t4_raw %>% janitor::clean_names() %>% select(date_time_edt, temperature_c, light_lux) %>% mutate(table = "T4"),
                     hobo_t5_raw %>% janitor::clean_names() %>% select(date_time_edt, temperature_c, light_lux) %>% mutate(table = "T5")) %>%
   drop_na(temperature_c) %>%
   mutate(date_time = mdy_hms(date_time_edt),
          date = date(date_time),
          time = format(date_time, format="%H:%M:%S")) %>%
   select(-date_time_edt) %>%
   relocate(temperature_c, light_lux, .after = time)



