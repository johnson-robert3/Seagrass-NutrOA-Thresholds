#~~~
# Figures to view leaf and shoot variables
#
# By: R. Johnson
#~~~


#--
# Shoot Structure
#--

#-- Halodule wrightii --#

#- Hw Number of shoots

# Halodule - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = hw_shoots, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = hw_shoots, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw shoots (per pot)")


#- Hw Number of leaves

# Halodule - number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = hw_blades, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = hw_blades, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw leaves (per pot)")

# Halodule - number of leaves at wk 9, as mean + SE
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_blades = mean(hw_blades, na.rm=TRUE),
                    se_blades = se(hw_blades),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_blades, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_blades, ymin = mean_blades - se_blades, ymax = mean_blades + se_blades, color = treatment_ph), width=0.2) +
   labs(title = "Hw number of leaves at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Number of Hw leaves (per pot)")
   # will need to somehow relate this to the number of leaves at the start (wk2) (b/c plants started with different numbers of leaves)



#-- Thalassia testudinum --#

#- Tt Number of shoots 

# Thalassia - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = tt_shoots, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = tt_shoots, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt shoots (per pot)")


#- Tt Number of leaves

# Thalassia - total number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = tt_blades, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = tt_blades, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt leaves (per pot)")

# Thalassia - total number of leaves at wk 9, as mean + SE
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_blades = mean(tt_blades, na.rm=TRUE),
                    se_blades = se(tt_blades),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_blades, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_blades, ymin = mean_blades - se_blades, ymax = mean_blades + se_blades, color = treatment_ph), width=0.2) +
   labs(title = "Tt number of leaves at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Number of Tt leaves (per pot)")


# Thalassia - blades per shoot at wk 9, as mean + SE
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_bps = mean(tt_bps, na.rm=TRUE),
                    se_bps = se(tt_bps),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_bps, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_bps, ymin = mean_bps - se_bps, ymax = mean_bps + se_bps, color = treatment_ph), width=0.2) +
   labs(title = "Tt blades per shoot at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt blades per shoot")



#--
# Leaf Morphometry
#--

#-- Blade Length --#

# Hw blade length at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_length = mean(blade_length, na.rm=TRUE),
                    se_length = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_length, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_length, ymin = mean_length - se_length, ymax = mean_length + se_length, color = treatment_ph), width=0.2) +
   labs(title = "Hw blade length at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw blade length (cm)")

# Hw blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw blade length at week 9",
        x = "Nutrient treatment",
        y = "Hw blade length (cm)")


# Tt blade length at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_length = mean(blade_length, na.rm=TRUE),
                    se_length = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) + 
   geom_point(aes(x = treatment_nutrients, y = mean_length, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_length, ymin = mean_length - se_length, ymax = mean_length + se_length, color = treatment_ph), width =0.2) +
   labs(title = "Tt blade length at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt blade length (cm)")

# Tt blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt blade length at week 9",
        x = "Nutrient treatment",
        y = "Tt blade length (cm)")



#-- Blade Area --#
# mean surface area of individual blades

# Hw individual blade area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_area = mean(blade_area, na.rm=TRUE),
                    se_area = se(blade_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_area, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area, color = treatment_ph), width=0.2) +
   labs(title = "Hw blade area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw blade size (cm^2)")


# Tt individual blade area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_area = mean(blade_area, na.rm=TRUE),
                    se_area = se(blade_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_area, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area, color = treatment_ph), width=0.2) +
   labs(title = "Tt blade area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt blade size (cm^2)")



#-- Leaf Surface Area --#
# total surface area of all leaves in each pot

# Hw total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_area = mean(tot_leaf_area, na.rm=TRUE),
                    se_area = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_area, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area, color = treatment_ph), width=0.2) +
   labs(title = "Hw total leaf area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw leaf surface area (cm^2)")


# Tt total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_area = mean(tot_leaf_area, na.rm=TRUE),
                    se_area = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_area, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area, color = treatment_ph), width=0.2) +
   labs(title = "Tt total leaf area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt leaf surface area (cm^2)")




#--
# Leaf Biomass
#-- 

# Hw leaf biomass at wk 9, as mean + SE
ggplot(biomass_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hw shoot biomass at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw shoot biomass (g)")


# Tt leaf biomass at wk 9, as mean + SE
ggplot(biomass_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Tt shoot biomass at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt shoot biomass (g)")




