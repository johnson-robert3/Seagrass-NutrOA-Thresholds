#~~~
# Porewater data
#
# By: R. Johnson
#~~~



#--
# Porewater Sulfide 
#--

# Porewater sample data ('porewater' df created in the 'data_import-and-process' script)

#- Sulfide concentration in original porewater
porewater_plant = porewater %>%
   # create sample_id column to match spec data for joining
   mutate(sample_id = paste0("MSI25-", table, "-", plant_id, "-", week)) %>%
   relocate(sample_id) %>%
   rename(notes_sample = notes) %>%
   # add sulfide concentration from spec data
   left_join(sulfide %>%
                 select(sample_id, scint_S_uM, flag_spec = flag, notes_spec = notes)) %>%
   # calculate porewater total dissolved sulfide concentration (units = uM)
   mutate(
      # total S in vial (concentration times total aqueous volume)
      scint_S_umol = scint_S_uM * ((h2s_vol_ml + h2s_znac_vol_ml) / 1000),
      # concentration of S in porewater
      porewater_S_uM = scint_S_umol / (h2s_vol_ml / 1000)) %>%
   # add treatment and plant info
   left_join(plant_dat %>% select(plant_id, treatment_ph, treatment_nutrients, site)) %>%
   # remove the outlier sulfide data point
   #  sample T2-L152-w2 has a PW conc. of ~5000 uM (5 mM)  (next highest is < 1000 uM)
   mutate(porewater_S_uM = replace_when(porewater_S_uM, porewater_S_uM > 4000 ~ NA_real_))


# Calculate mean and SE for each treatment over time
porewater_trt = porewater_plant %>%
   # treatment mean and SE
   summarize(across(c(pH, porewater_S_uM), list(mean = ~mean(., na.rm=TRUE), se = se), .names = "{.fn}_{.col}"), 
             .by = c(treatment_ph, treatment_nutrients, week))









#- Estimating H2S concentration -#

## return to this after the TAC meeting...going to take a bit more work...

#--- start

# need to get Temperature data for the tables from the HOBO time series
# get salinity data from the CarbCHEM dataset

tmp = temp_dat %>%
   filter(date %in% c(porewater %>% pull(date) %>% unique())) %>% 
   select(-date_time) %>%
   filter(between(.$time, "06:00:00", "12:00:00")) %>%
   summarize(temp_c = mean(temperature_c), .by = c(table, date))


# To estimate the fraction of dissolved sulfide that is H2S

calc_h2s = function(.dat, temp_c, salinity, pH) {  # function not done yet!! need to be able to apply to a data frame (like 'calc_S_conc' function)
   
   # equations from MacLeod et al. 2023 and Millero 1986
   
   # from MacLeod et al. 2023
   
   # [H2S]/[ST] = 1 / (10^(pH - pK) + 1)
   
      # ST = total dissolved sulfide concentration
      # pK = pK at in situ temperature and salinity from equation 17 in Millero 1986
   
   
   # from Millero 1986
   
   # temp in Kelvin
   temp_k = 273.15 + temp_c
   
   # constants
   A = -0.2391 + (35.685 / temp_k)  # eq. 18
   B = 0.0109 - (0.3776 / temp_k)   # eq. 19
   
   # eq. 12
   pk1 = 32.55 + (1519.44 / temp_k) - (15.672 * (log10(temp_k))) + (0.02722 * temp_k)
   
   # eq. 17
   pK = pk1 + (A * salinity^0.5) + (B * salinity)
   
   
   # calculations
   
   fraction = 1 / (10^(pH - pK) + 1)
   
   conc_h2s = porewater_S_uM * fraction

}

#--- end


