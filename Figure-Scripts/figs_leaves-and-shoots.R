#~~~
# Figures to view leaf and shoot variables
#
# By: R. Johnson
#~~~


# Values for figures

# pH treatment color for Halodule (blues)
ph_col_hw = c('ambient' = "#4988C4", 'OA' = "#0F2854")  # ambient - light; OA - dark
# pH treatment color for Thalassia (greens)
ph_col_tt = c('ambient' = "#00B7B5", 'OA' = "#005461")  # ambient - light; OA - dark


# function for aesthetics
fig_theme = function (.fig) {
   
   .fig +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks = element_line(color='black'), 
            axis.text = element_text(color='black', size=10),
            axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
            axis.title = element_text(color="black", size=12), 
            axis.title.x = element_text(margin = margin(t=5, 'line')),
            axis.title.y = element_text(margin = margin(r=7, 'line')))
}



#--
# Shoot Structure
#--

#-- Number of shoots --#


#-- Number of leaves --#

# Hw - week 9
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed" & hw_blades>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(hw_blades, na.rm=TRUE),
                    se = se(hw_blades),
                    .by = c(treatment_ph, treatment_nutrients))) +
   
   # will still need to somehow relate this to the number of leaves at the start (wk2) (b/c plants started with different numbers of shoots)
   
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule number leaves - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of leaves (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# Tt - week 9
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed" & tt_blades>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tt_blades, na.rm=TRUE),
                    se = se(tt_blades),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Tt number leaves - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of leaves (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#-- Blades per shoot --#

# Tt - week 9
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed" & tt_bps>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean_bps = mean(tt_bps, na.rm=TRUE),
                    se_bps = se(tt_bps),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean_bps, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_bps, ymin = mean_bps - se_bps, ymax = mean_bps + se_bps, color = treatment_ph), width=0.2) +
   labs(title = "Tt blades per shoot at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt blades per shoot")



# Box and whisker plots with jittered data points
{
# Halodule - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = hw_shoots, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = hw_shoots, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw shoots (per pot)")


# Halodule - number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = hw_blades, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = hw_blades, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw leaves (per pot)")


# Thalassia - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = tt_shoots, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = tt_shoots, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt shoots (per pot)")


# Thalassia - total number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(week=="w9" & treatment_nutrients!="pulsed" & tt_blades>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = tt_blades, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = tt_blades, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt leaves (per pot)")
}



#--
# Leaf Morphometry
#--

#-- Blade Length --#

# Hw - week 9
windows(height=4, width=5)
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule height - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_BL_w9.png", height=4, width=5, units="in", dpi=300)


# Hw weeks 6 and 9 together (example code for all symbols together)
{
ggplot(morph_plant %>% filter(week %in% c('w6','w9') & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   # both (open data points for wk 6, closed data points for wk 9)
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph, shape=week), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, linetype=week, group = interaction(week, treatment_ph)), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, 
                     color = treatment_ph, group = interaction(week, treatment_ph)), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_shape_manual(values = c('w6' = 1, 'w9' = 19), name = "Week") +
   scale_linetype_manual(values = c('w6' = 2, 'w9' = 1), name = "Week") +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   #
   labs(title = "Halodule height",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}


# Tt - week 9
windows(height=4, width=5)
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia height - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_BL_w9.png", height=4, width=5, units="in", dpi=300)



# BL - Box and whisker plots with jittered data points
{
# Hw blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw blade length at week 9",
        x = "Nutrient treatment",
        y = "Hw blade length (cm)")


# Tt blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt blade length at week 9",
        x = "Nutrient treatment",
        y = "Tt blade length (cm)")
}



#-- Blade Area --#
# mean surface area of individual blades

# Hw - week 9
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_area, na.rm=TRUE),
                    se = se(blade_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hw blade area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw blade size (cm^2)")


# Tt - week 9
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_area, na.rm=TRUE),
                    se = se(blade_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Tt blade area at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt blade size (cm^2)")



#-- Leaf Surface Area --#
# total surface area of all leaves in each pot

# Hw total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule total leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   theme_classic() %>%
   fig_theme()


# Tt total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Thalassia total leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   theme_classic() %>%
   fig_theme()




#--
# Leaf Biomass
#-- 

# Hw leaf biomass at wk 6, as mean + SE
ggplot(biomass_plant %>% filter(week=="w6" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Hw shoot biomass at week 6 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Hw shoot biomass (g)") +
   theme_classic() %>%
   fig_theme()


# Hw leaf biomass at wk 9, as mean + SE
windows(height=4, width=5)
ggplot(biomass_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule shoot biomass - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_biomass_w9.png", height=4, width=5, units="in", dpi=300)


# Tt leaf biomass at wk 6, as mean + SE
ggplot(biomass_plant %>% filter(week=="w6" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Tt shoot biomass at week 6 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Tt shoot biomass (g)") +
   theme_classic() %>%
   fig_theme()


# Tt leaf biomass at wk 9, as mean + SE
windows(height=4, width=5)
ggplot(biomass_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia shoot biomass - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_biomass_w9.png", height=4, width=5, units="in", dpi=300)





