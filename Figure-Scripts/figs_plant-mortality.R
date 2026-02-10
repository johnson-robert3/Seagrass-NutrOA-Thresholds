#~~~
# Viewing plant survivorship/mortality
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



## Create temporary datasets for figures

# use this df for viewing mortality over time (each nutrient treatment as its own line)
mort.ts = hw_mort %>% mutate(species = "Hw") %>%
   # combine Hw and Tt data
   full_join(tt_mort %>% mutate(species = "Tt")) %>%
   relocate(species) %>%
   # recalculate survival as the percentage remaining compared to the start at week 0
   #  this method is better for viewing time-series, but is not correct for viewing trt differences at specific time points
   select(-starts_with("survive")) %>%
   mutate(survive_w0 = 100,
          survive_w2 = alive_w2 / alive_w0 * 100,
          survive_w6 = alive_w6 / alive_w0 * 100,
          survive_w9 = alive_w9 / alive_w0 * 100) %>%
   select(-alive_w0:-w2_for_w9) %>%
   pivot_longer(cols = starts_with("survive"),
                names_to = "week",
                values_to = "survival") %>%
   mutate(week = parse_number(week),
          week = paste0('w', week))


# use this df for viewing differences among nutrient treatments at specific time points (each pH treatment or species as its own line)
mort.comp = hw_mort %>% mutate(species = "Hw") %>%
   # combine Hw and Tt data
   full_join(tt_mort %>% mutate(species = "Tt")) %>%
   relocate(species) %>%
   select(-alive_w0:-w2_for_w9) %>%
   pivot_longer(cols = starts_with("survive"),
                names_to = "week",
                values_to = "survival") %>%
   mutate(week = parse_number(week),
          week = paste0('w', week))



#-- Time Series --#

# Hw surival over time
ggplot(mort.ts %>% filter(treatment_ph == 'ambient' & species=="Hw")) +
   geom_line(aes(x = week, y = survival, color = treatment_nutrients)) +
   geom_point(aes(x = week, y = survival, color = treatment_nutrients)) +
   lims(y = c(0, 100)) +
   labs(title = "Hw survival (ambient)")

ggplot(mort.ts %>% filter(treatment_ph == 'OA' & species=="Hw")) +
   geom_line(aes(x = week, y = survival, color = treatment_nutrients)) +
   geom_point(aes(x = week, y = survival, color = treatment_nutrients)) +
   lims(y = c(0, 100)) +
   labs(title = "Hw survival (OA)")


# Tt surival over time
ggplot(mort.ts %>% filter(treatment_ph == 'ambient' & species=="Tt")) +
   geom_line(aes(x = week, y = survival, color = treatment_nutrients)) +
   geom_point(aes(x = week, y = survival, color = treatment_nutrients)) +
   lims(y = c(0, 100)) +
   labs(title = "Tt survival (ambient)")

ggplot(mort.ts %>% filter(treatment_ph == 'OA' & species=="Tt")) +
   geom_line(aes(x = week, y = survival, color = treatment_nutrients)) +
   geom_point(aes(x = week, y = survival, color = treatment_nutrients)) +
   lims(y = c(0, 100)) +
   labs(title = "Tt survival (OA)")





#-- Differences across nutrient treatments --# 

# Hw - week 6
ggplot(mort.comp %>% filter(species=="Hw" & week=='w6' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   lims(y = c(0, 100)) +
   labs(title = "Halodule survival - Stress (wk 6)",
        x = "Nutrient treatment (g)",
        y = "Survival (%)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_mort_w6.png", height=4, width=5, units="in", dpi=300)


# Hw - week 9
ggplot(mort.comp %>% filter(species=="Hw" & week=='w9' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   lims(y = c(0, 100)) +
   labs(title = "Halodule survival - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Survival (%)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_mort_w9.png", height=4, width=5, units="in", dpi=300)


# Halodule - both weeks
ggplot(mort.comp %>% filter(species=="Hw" & week %in% c('w6','w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   # both (open data points for wk 6, closed data points for wk 9)
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph, shape=week), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, linetype=week, group = interaction(week, treatment_ph)), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   #
   scale_shape_manual(values = c('w6' = 1, 'w9' = 19), name = "Week") +
   scale_linetype_manual(values = c('w6' = 2, 'w9' = 1), name = "Week") +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   #
   lims(y = c(0, 100)) +
   labs(title = "Hw survival by treatment and period",
        x = "Nutrient treatment",
        y = "Survival (%)")



# Tt - week 6
ggplot(mort.comp %>% filter(species=="Tt" & week=='w6' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   lims(y = c(0, 100)) +
   labs(title = "Thalassia survival - Stress (wk 6)",
        x = "Nutrient treatment (g)",
        y = "Survival (%)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_mort_w6.png", height=4, width=5, units="in", dpi=300)


# Tt - week 9
ggplot(mort.comp %>% filter(species=="Tt" & week=='w9' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   lims(y = c(0, 100)) +
   labs(title = "Thalassia survival - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Survival (%)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_mort_w9.png", height=4, width=5, units="in", dpi=300)


# Thalassia - both weeks
ggplot(mort.comp %>% filter(species=="Tt" & week %in% c('w6','w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   # both (open data points for wk 6, closed data points for wk 9)
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph, shape=week), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, linetype=week, group = interaction(week, treatment_ph)), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   #
   scale_shape_manual(values = c('w6' = 1, 'w9' = 19), name = "Week") +
   scale_linetype_manual(values = c('w6' = 2, 'w9' = 1), name = "Week") +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   #
   lims(y = c(0, 100)) +
   labs(title = "Tt survival by treatment and period",
        x = "Nutrient treatment",
        y = "Survival (%)") +
   theme_classic() %>%
   fig_theme()











# comparing species and treatments at week 6
ggplot(mort.comp %>% filter(week=='w6' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph, shape = species), size=2) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = interaction(treatment_ph, species))) +
   lims(y = c(0, 100)) +
   labs(title = "Week 6 survival by species",
        x = "Nutrient treatment",
        y = "Survival (%)")


# comparing species and treatments at week 9
ggplot(mort.comp %>% filter(week=='w9' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = treatment_ph, shape = species), size=2) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = treatment_ph, group = interaction(treatment_ph, species))) +
   lims(y = c(0, 100)) +
   labs(title = "Week 9 survival by species",
        x = "Nutrient treatment",
        y = "Survival (%)")





# both species together at wk 9 (Ambient = closed; OA = open)
ggplot(mort.comp %>% filter(week=='w9' & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = survival, color = species, shape = treatment_ph), size=2) +
   geom_line(aes(x = treatment_nutrients, y = survival, color = species, group = interaction(treatment_ph, species))) +
   #
   scale_color_manual(values = c("Hw" = "#0D4C92", "Tt" = "#59C1BD")) +
   scale_shape_manual(values = c("ambient" = 19, "OA" = 1)) +
   #
   lims(y = c(0, 100)) +
   labs(title = "Week 9 survival by species",
        x = "Nutrient treatment",
        y = "Survival (%)") +
   theme_classic()






