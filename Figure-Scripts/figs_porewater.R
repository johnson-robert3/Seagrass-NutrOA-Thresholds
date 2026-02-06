#~~~
# Figures for porewater
#
# By: R. Johnson
#~~~


#--
# pH
#--

# porewater pH between treatments at wk 2, mean + SE
ggplot(porewater_trt %>% filter(week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_pH, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_pH, ymin = mean_pH - se_pH, ymax = mean_pH + se_pH, color = treatment_ph), width=0.2) +
   labs(title = "Porewater pH at week 2",
        x = "Nutrient treatment",
        y = "pH")


# porewater pH between treatments at wk 6, mean + SE
ggplot(porewater_trt %>% filter(week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_pH, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_pH, ymin = mean_pH - se_pH, ymax = mean_pH + se_pH, color = treatment_ph), width=0.2) +
   labs(title = "Porewater pH at week 6",
        x = "Nutrient treatment",
        y = "pH")


# porewater pH between treatments at wk 9, mean + SE
ggplot(porewater_trt %>% filter(week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_pH, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_pH, ymin = mean_pH - se_pH, ymax = mean_pH + se_pH, color = treatment_ph), width=0.2) +
   labs(title = "Porewater pH at week 9",
        x = "Nutrient treatment",
        y = "pH")




#--
# Sulfide
#--

# porewater DS between treatments at wk 2, mean + SE
ggplot(porewater_trt %>% filter(week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 2",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# porewater DS between treatments at wk 6, mean + SE
ggplot(porewater_trt %>% filter(week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 6",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# porewater DS between treatments at wk 9, mean + SE
ggplot(porewater_trt %>% filter(week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 9",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")




