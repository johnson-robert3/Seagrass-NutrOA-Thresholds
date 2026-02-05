#~~~
# Calculate and view shoot morphology (number of blades and shoots) and morphometry throughout the experiment
#
# By: R. Johnson
#~~~


se = function(x) { sd(x, na.rm=TRUE) / sqrt(length(na.omit(x))) }


#--
# Blade Morphometry
#-- 

# Calculate Tt blade area for each plant ID (pot) at each time point
morph_allblades = morphometry %>%
   # label shoot number to account for times with a second Tt shoot
   mutate(shoot_num = case_when(notes %in% c("second shoot") ~ 2,
                                notes %in% c("third shoot") ~ 3,
                                .default = 1)) %>%
   # lengthen the df for blade lengths
   pivot_longer(cols = starts_with("length"), names_to = "blade", values_to = "length_cm", values_drop_na=TRUE) %>%
   # calculate blade area using length and width (this is area for each individual blade) (units = cm^2)
   mutate(blade_area_cm = length_cm * width_cm)


# Calculate mean values (and total leaf surface area) for each plant ID (pot)
morph_plant = morph_allblades %>%
   # mean values for each shoot (for when there's more than one Tt shoot)
   summarize(blade_length = mean(length_cm, na.rm=TRUE),     # mean blade length for each shoot
             blade_width = mean(width_cm, na.rm=TRUE),       # mean blade width for each shoot
             blade_area = mean(blade_area_cm, na.rm=TRUE),   # mean blade area for each shoot
             BPS = n(),                                      # number of blades per shoot (based on number of blade length measurements)
             tot_leaf_area = sum(blade_area_cm, na.rm=TRUE), # total leaf surface area for each shoot (summing blade area for all blades on the shoot)
             .by = c(plant_id, week, species, shoot_num)) %>%
   # values for each pot: means or totals (across multiple Tt shoots)
   summarize(num_shoots = max(shoot_num),        # number of shoots in the pot (only applicable to Tt; we only measured morph on 1 Hw shoot per pot)
             num_blades = sum(BPS),              # total number of blades in the pot (only applicable to Tt)
             across(c(blade_length, blade_width, blade_area, BPS), ~mean(.)),   # mean of blade measurements and mean blades-per-shoot in the pot
             tot_leaf_area = sum(tot_leaf_area), # total leaf surface area (one-sided) in the pot
             .by = c(plant_id, week, species))


# Calculate mean and SE for each treatment over time
morph_trt = morph_plant %>%
   # add treatment information
   left_join(plant_dat %>% select(plant_id, treatment_ph, treatment_nutrients)) %>%
   # mean/SE by treatment
   summarize(across(c(num_shoots:tot_leaf_area), list(mean=mean, se=se), .names = "{.fn}_{.col}"), n=n(),
             .by=c(species, treatment_ph, treatment_nutrients, week))



#--
# Shoot Structure
#--

# Number of shoots and number of blades in each pot over time for each species

# Create  a df similar to 'plants' from the 'data_plant-mortality' script, but omit plant IDs with identified issues

# Combine leaf/shoot count data with plant_ID data
shoots_all = leaf_counts %>% rename(count_notes = notes) %>% left_join(plant_dat) %>%
   # 7 plant IDs were duplicated between 8/28 and 8/29
   # keep only the 8/29 data for these plants (Hw data is the same across both dates; Tt plants died on 8/29 for these 7 IDs)
   filter(!(date == "2025-08-28" & plant_id %in% c(leaf_counts %>% filter(date == "2025-08-29") %>% pull(plant_id)))) %>%
   # omit the plant IDs with issues identified in the 'data_plant-mortality' script
   filter(!(plant_id %in% c("A018", "L006", "L171", "L163", "H117")))


# Add a variable for Thalassia shoot count (this accounts for times when a second (or third) Tt shoot was recorded in the notes column)
shoots_all = shoots_all %>%
   # number of Tt shoots within each pot
   mutate(tt_shoots = case_when(str_detect(count_notes, "two new Tt shoots") ~ 3,
                                str_detect(count_notes, "new Tt shoot") ~ 2,
                                Tt_blades > 0 | is.na(Tt_blades) ~ 1,
                                .default = 0)) %>%
   # rename for consistency
   rename(hw_shoots = Hw_shoots,                  # number of Hw shoots in each pot
          hw_blades = Hw_blades,                  # total number of Hw blades in each pot
          tt_blades_og_shoot = Tt_blades) %>%     # number of Tt blades for original shoot (num. for 2nd/3rd shoots listed in notes)
   relocate(tt_shoots, .before=tt_blades_og_shoot)


# Extract number of blades for extra shoots from the notes column
shoots_all = shoots_all %>%
   mutate(tt_blades_xtra_shoots = case_when(
      plant_id=="H076" & str_detect(count_notes, "tiny new Tt shoot/leaf") ~ 1,
      plant_id=="L121" & tt_shoots==3 ~ 7,
      plant_id=="H071" & tt_shoots==3 ~ 6,
      tt_shoots == 2 ~ parse_number(count_notes),
      .default = 0))


# Calculate total blades and blades-per-shoot for each species for each plant ID (pot) at each time point
shoots_plant = shoots_all %>%
   # total number of Tt blades in each pot
   mutate(tt_blades = tt_blades_og_shoot + tt_blades_xtra_shoots) %>%
   # calculate blades-per-shoot for each species (based on total number of blades and shoots counted in each pot)
   mutate(tt_bps = case_when(tt_blades_og_shoot > 0 ~ tt_blades / tt_shoots,
                             is.na(tt_blades_og_shoot) ~ NA_real_,
                             .default = 0),
          hw_bps = case_when(hw_blades > 0 ~ hw_blades / hw_shoots,
                             is.na(hw_blades) ~ NA_real_,
                             .default = 0)) %>%
   # remove variables that are no longer needed
   select(-tt_blades_og_shoot, -tt_blades_xtra_shoots) %>%
   relocate(tt_blades, .after = tt_shoots) %>%
   relocate(tt_bps, hw_bps, .after = hw_blades)




#===
# within the leaf and shoot counts dataset (leaf_counts df), the column for number of Thalassia blades (Tt_blades) is only the blade count for the original Tt shoot
# when a second (or third) shoot was present, this is listed in the notes column, along with the number of blades on that new shoot
# the Thalassia blade count (Tt_blades) is not a total count of leaves in the pot
# there are not additional rows in the CSV dataset for second/third shoots (as is the case in the morphometry dataset)

# view occasions when a second Tt shoot was recorded
leaf_counts %>%
   filter(str_detect(notes, "new Tt shoot")) %>% 
   select(plant_id, week, notes) %>%
   mutate(blades = parse_number(notes)) %>%
   View
# don't deal with this right now (can come back to later to accurately account for additional Tt shoots)
# omitting additional shoots for now won't have much effect on treatment means; just use first/original shoot data


## this works for getting/adding Tt shoot count variable
## but this can't be used to calculate Tt BPS, because the blade count (Tt_blades) is only for the original shoot, 
##  not the total number of blades like it is for Hw
shoots %>%
   mutate(tt_shoots = case_when(str_detect(count_notes, "two new Tt shoots") ~ 3,
                                str_detect(count_notes, "Tt shoot") ~ 2,
                                Tt_blades > 0 | is.na(Tt_blades) ~ 1,
                                .default = 0)) %>%
   View
#===



# treatment means of blades and shoots using the leaf_counts dataset (does not account for times with more than 1 Tt shoot)
shoots_trt = shoots %>%
   # remove wks 3 and 8 (only dead/missing plants recorded these weeks; number of blades/shoots were not counted if plant was present)
   filter(!(week %in% c('w3', 'w8'))) %>%
   summarize(across(c(Tt_blades, Hw_blades, Hw_shoots), list(mean=~mean(., na.rm=TRUE), se=se), .names="{.fn}_{.col}"), n=n(),
             .by=c(treatment_ph, treatment_nutrients, week)) 




#--------------

# For treatment means, and viewing differences among treatments during the experiment, for Thalassia...

# Should I use BPS calculated from the number of blade length measurements? 
# (morph was not measured on all Tt plants at each time, so there are fewer data to work with using this approach)

# Or should I use BPS from the 'leaf_counts' df (variable Tt_blades) which was directly measured? 
# (number of blades was typically measured on all available Tt plants, so there are more data using this approach)
# (but this approach does not account for BPS of second or third Tt shoots) (this info is in the Notes column, but not currently in the data)

# Or should I use the leaf_counts df and add a Tt shoot count variable as above, and just use the measured BPS from this dataset,
# even though it doesn't account for BPS on extra Tt shoots (so mean BPS estimates may be slightly inaccurate)
# (but there really aren't that many instances with more than 1 Tt shoot)


# I think we did leaf/shoot counts on all pots when we counted this (except maybe in 'off' weeks, like 3/5/8)
# and I think morphometry was always on a more limited subset of pots 


# view the number of plant IDs with morph measurements for Tt, to see how many plants I'd have for blade counts using the morph dataset
morph_plant %>% filter(species=='Tt' & week %in% c('w2', 'w6', 'w9')) %>% summarize(n_distinct(plant_id), .by=week)

# view the number of plant IDs for Tt for blade counts using the count dataset
shoots %>% filter(Tt_blades > 0 & week %in% c('w2', 'w6', 'w9')) %>% summarize(n_distinct(plant_id), .by=week)



# compare the different methods, how similar are the treatment means calculated each way? 

x = morph_trt %>%
   filter(species=="Tt") %>%
   select(treatment_ph, treatment_nutrients, week, mean_BPS, n_morph = n)

y = shoots_trt %>%
   select(treatment_ph, treatment_nutrients, week, mean_Tt_blades, n_count = n)

w = full_join(x, y)

plot(w$mean_BPS, w$mean_Tt_blades, pch=16)
# add a dashed 1:1 line to the plot
abline(a=0, b=1, lty=2, col="red")
# BPS is almost always higher from the morph df (means plants w/ low BPS are underrepresented)
# should use Tt BPS from the leaf_counts dataset

hist(w$mean_BPS)
hist(w$mean_Tt_blades)
hist(w$mean_BPS - w$mean_Tt_blades)






