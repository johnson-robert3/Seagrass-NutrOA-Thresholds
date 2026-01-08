#~~~
# Process spectrophotometer data for suflide standard curves
#
#~~~

setwd("C:/Users/rajohnson6/Box/Projects/Mote Seagrass Thresholds/Mote Data")

library(tidyverse)


#--
# Low curve, Dec. 2025
#--

# Standard curve using MQ water instead of ZnAc for all standards, blanks, and dilutions

   # S std date: 6/20/2023
   # Diamine reag. (L) date: 4/05/2025
   # Run by: RA Johnson & MG Teter


# raw data from spec run
std_curve_dec25 = read_csv("Sulfide spectrophotometer data/Sulfide_Low_standard_curve_Dec_2025.csv") %>%
   janitor::remove_empty(which = 'rows')


# Clean up the data
std_dec25 = std_curve_dec25 %>%
   # remove 'zero'
   filter(!(vial == "Zero")) %>%
   # remove checks
   filter(!(str_detect(vial, pattern='chk')))


# Calculate corrected absorbance for standards
std_dec25 = std_dec25 %>%
   filter(!(vial == "Blank")) %>%
   # correct for blank absorbance
   mutate(abs_blk_corr = abs_667 - (std_dec25 %>% filter(vial == "Blank") %>% pull(abs_667) %>% mean)) %>%
   # correct for dilution factor
   mutate(abs_corr = abs_blk_corr * dilution)


# View the curve
windows(); ggplot(std_dec25, aes(x = abs_corr, y = conc_uM)) + geom_point()

summary(lm(conc_uM ~ abs_corr, data = std_dec25))  # R2 = 0.9965



#~~~
# Equation for concentration - absorbance relationship
#~~~

# lm(conc_uM ~ abs_df_corr, data = std_curve) %>% coef()

calc_S_conc = function(.abs, .std_curve) {
   
   intercept = coef(lm(conc_uM ~ abs_corr, data = .std_curve))[[1]]
   slope = coef(lm(conc_uM ~ abs_corr, data = .std_curve))[[2]]
   
   concentration = (.abs * slope) + intercept
   
   return(concentration)
}



# End
graphics.off()
rm(list = ls(pattern = "_curve_"))

