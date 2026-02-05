#~~~
# Calculate and view seagrass plant mortality/survivorship at intervals throughout the experiment
#
# By: R. Johnson
#~~~

# combine leaf/shoot count data with plant_ID data
plants = leaf_counts %>% rename(count_notes = notes) %>% left_join(plant_dat) %>%
   # 7 plant IDs were duplicated between 8/28 and 8/29
   # keep only the 8/29 data for these plants (Hw data is the same across both dates; Tt plants died on 8/29 for these 7 IDs)
   filter(!(date == "2025-08-28" & plant_id %in% c(leaf_counts %>% filter(date == "2025-08-29") %>% pull(plant_id)))) %>%
   # plant A018 was removed before the experiment began b/c all seagrass died during acclimation period
   # just omit here b/c the pot was never assigned to treatments
   filter(!(plant_id == "A018"))


# list of all plant IDs used in experiment
w0_ids = plants %>% pull(plant_id) %>% unique()

# plant IDs that were harvested for genetics at wk6
harvest_ids = plant_dat %>% filter(str_detect(removal_notes, "genetics")) %>% pull(plant_id)



#-- HALODULE --#

# Plant IDs with living Hw at wk2
hw_w2_ids = plants %>% filter(week == "w2" & Hw_blades > 0) %>% pull(plant_id)

# Hw alive at wk 6
hw_w6_ids = plants %>% filter(week == "w6" & Hw_blades > 0) %>% pull(plant_id)

# Hw alive at wk 9
hw_w9_ids = plants %>% filter(week == "w9" & Hw_blades > 0) %>% pull(plant_id)

# Hw alive at wk 2 that were not harvested for genetics at wk 6 (to calculate survivorship of these at wk 9)
# however, can't just exclude all the plant IDs that were harvested in wk 6, b/c three of these plants had the Hw die before wk 6,
# so those 3 plant IDs need to be retained and counted toward the survivorship at wk 9
# only exclude the genetics IDs that still had living Hw at wk 6 (b/c this wasn't mortality due to the experiment)
# (it's actually only 2 plant IDs (A002, H051), b/c Hw was already dead at wk 2 in the third (A010))

   # view the plants harvested at wk 6 where the Hw was already dead
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades == 0) %>% View
   
   # genetic IDs that had living Hw at wk 6
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades > 0) %>% View

# so the final set of plant IDs from wk 2 to be used for calculating survivorship at wk 9 is...
hw_w2_no_genetics = plants %>% filter(week == "w2" & Hw_blades > 0) %>% 
   filter(!(plant_id %in% 
               c(plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades > 0) %>% pull(plant_id))
            )) %>% 
   pull(plant_id)


#- Halodule survivorship over time

# survivorship at wk 6 relative to wk 2
length(hw_w6_ids) / length(hw_w2_ids) * 100

# survivorship at wk 9 relative to wk 2
length(hw_w9_ids) / length(hw_w2_no_genetics) * 100

   # - but these are just total, need to split it by treatments


# make a df denoting whether a plant ID was alive at time points
hw_surv = plants %>%
   mutate(
      alive_w0 = 1,
      alive_w2 = if_else(plant_id %in% hw_w2_ids, 1, 0),
      alive_w6 = if_else(plant_id %in% hw_w6_ids, 1, 0),
      alive_w9 = if_else(plant_id %in% hw_w9_ids, 1, 0),
      w2_for_w9 = if_else(plant_id %in% hw_w2_no_genetics, 1, 0)) #%>%
   # filter(alive_w2 != 0)


# view survivorship by treatment
hw_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w2:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w6_perc = alive_w6 / alive_w2 * 100,
          survive_w9_perc = alive_w9 / w2_for_w9 * 100) %>%
   View

   # view ambient ph and 2g treatment, why does w9 have higher survivorship than wk 6?
   hw_surv %>% filter(treatment_ph=="ambient" & treatment_nutrients=="2g") %>% arrange(plant_id, week) %>% View

   # view plants that are recorded as dead in wk 6 but then alive in wk 9
   hw_surv %>% filter(alive_w6==0 & alive_w9==1) %>% arrange(plant_id, week) %>% View
      # L006 was accidentally not counted in wk 6 (and missing values are counted as dead in the code above)
      # L171 had 0 blades in wk 6, and then 1 blade in wk 9; must have been missed in wk 6, or roots survived and a new blade grew by wk 9
      # - just omit these two for survivorship counts


# df of Halodule survivorship
hw_surv = hw_surv %>% filter(!(plant_id %in% c("L006", "L171")))

# view survivorship by treatment
hw_mort = hw_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w0:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w2_perc = alive_w2 / alive_w0 * 100,   # percent that survived initial acclimation period
          survive_w6_perc = alive_w6 / alive_w2 * 100,   # percent that survived the experimental period (relative to those living at wk 2)
          survive_w9_perc = alive_w9 / w2_for_w9 * 100)  # percent that survived to the end (relative to those living at wk 2, not included those harvested at wk 6)



#-- THALASSIA --#

# Plant IDs with living Thalassia at wk2
tt_w2_ids = plants %>% filter(week == "w2" & Tt_blades > 0) %>% pull(plant_id)

# Tt alive at wk 6
tt_w6_ids = plants %>% filter(week == "w6" & Tt_blades > 0) %>% pull(plant_id)

# Tt alive at wk 9
tt_w9_ids = plants %>% filter(week == "w9" & Tt_blades > 0) %>% pull(plant_id)

# Tt alive at wk 2 that were not harvested for genetics at wk 6 (to calculate survivorship of these at wk 9)
# however, can't just exclude all the plant IDs that were harvested in wk 6, b/c five of these plants had the Tt die before wk 6,
# so those 5 plant IDs need to be retained and counted toward the survivorship at wk 9
# only exclude the genetics IDs that still had living Tt at wk 6 (b/c this wasn't mortality due to the experiment)

   # view the plants harvested at wk 6 where the Tt was already dead
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades == 0) %>% View
   
   plants %>% filter(plant_id %in% harvest_ids) %>% filter(Tt_blades == 0) %>% arrange(plant_id, week) %>% View
      # L163 was recorded as 0 Tt in wk 2 and wk 3, but then 5 Tt blades in wk 6; leaves fell off but must not have been dead, and then re-grew
   
   # genetic IDs that had living Tt at wk 6
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades > 0) %>% View
   
      # view IDs that are recorded as alive in wk 6, but were "dead" in wk 2
      setdiff(tt_w6_ids, tt_w2_ids)
      # two plants: L163, H117
      # both of these plants were recorded as having Tt blades in wk 6, but not in any other weeks
      # - just omit these two for survivorship counts

# so the final set of plant IDs from wk 2 to be used for calculating survivorship at wk 9 is...
tt_w2_no_genetics = plants %>% filter(week == "w2" & Tt_blades > 0) %>% 
   filter(!(plant_id %in% 
               c(plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades > 0) %>% pull(plant_id))
            )) %>% 
   pull(plant_id)


#- Thalassia survivorship over time

# survivorship at wk 6 relative to wk 2
length(tt_w6_ids) / length(tt_w2_ids) * 100

# survivorship at wk 9 relative to wk 2
length(tt_w9_ids) / length(tt_w2_no_genetics) * 100

   # - but these are just total, need to split it by treatments


# make a df denoting whether a plant ID was alive at time points
tt_surv = plants %>%
   mutate(
      alive_w0 = 1,
      alive_w2 = if_else(plant_id %in% tt_w2_ids, 1, 0),
      alive_w6 = if_else(plant_id %in% tt_w6_ids, 1, 0),
      alive_w9 = if_else(plant_id %in% tt_w9_ids, 1, 0),
      w2_for_w9 = if_else(plant_id %in% tt_w2_no_genetics, 1, 0)) #%>%
   # filter(alive_w2 != 0)

# df of Thalassia survivorship 
# (remove the two plants identified above that were only recorded as alive at wk 6)
tt_surv = tt_surv %>% filter(!(plant_id %in% c("L163", "H117"))) 


# view survivorship by treatment
tt_mort = tt_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w0:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w2_perc = alive_w2 / alive_w0 * 100,   # percent that survived initial acclimation period
          survive_w6_perc = alive_w6 / alive_w2 * 100,   # percent that survived the experimental period (relative to those living at wk 2)
          survive_w9_perc = alive_w9 / w2_for_w9 * 100)  # percent that survived to the end (relative to those living at wk 2, not included those harvested at wk 6)


