# Process MAGICC temperature data for LTS analysis
# Data from PNNL 7/30/2021 - Yang Ou

# 7/30/2021
# CH
# updated 10/19/2021

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
#### Import data

# Specify sheet by its name
# units deg C relative 1850-1900
temp_1p5 <- as_tibble(read_excel("input/LTS_scenario_data_0730.xlsx", sheet = "MAGICC7-1p5") %>%
                        filter(VARIABLE == "GCAM-runs|Temperature|Global Mean|MAGICCv7.5.1|MED") %>%
                      select(-REGION, -MODEL, -UNIT, -VARIABLE, -SCENARIO) %>%
          pivot_longer(col = c(`1995`: `2100`),
                 names_to = "year",
                 values_to = "value" ) ) %>%
  mutate(scenario = "1p5") %>%
  mutate(variable = "Global Mean Temperature Change (degC)")

rf_1p5 <- as_tibble(read_excel("input/LTS_scenario_data_0730.xlsx", sheet = "GCAM-forcing-1p5", skip = 1)) %>%
  select(-scenario, -region, -`forcing-total`, -Units) %>%
  pivot_longer(col = c(`1980`: `2100`),
               names_to = "year",
               values_to = "value" ) %>%
  mutate(scenario = "1p5") %>%
  mutate(variable = "Radiative Forcing (Wm-2)")

temp_paris <- as_tibble(read_excel("input/LTS_scenario_data_0730.xlsx", sheet = "MAGICC7-ParisIncr") %>%
                        filter(VARIABLE == "GCAM-runs|Temperature|Global Mean|MAGICCv7.5.1|MED") %>%
                        select(-REGION, -MODEL, -UNIT, -VARIABLE, -SCENARIO) %>%
                        pivot_longer(col = c(`2000`: `2100`),
                                     names_to = "year",
                                     values_to = "value" ) ) %>%
  mutate(scenario = "Paris") %>%
  mutate(variable = "Global Mean Temperature Change (degC)")


rf_paris <- as_tibble(read_excel("input/LTS_scenario_data_0730.xlsx", sheet = "GCAM-forcing-ParisIncr", skip = 1)) %>%
  select(-scenario, -region, -`forcing-total`, -Units) %>%
  pivot_longer(col = c(`1980`: `2100`),
               names_to = "year",
               values_to = "value" ) %>%
  mutate(scenario = "Paris") %>%
  mutate(variable = "Radiative Forcing (Wm-2)")

temp_7wm2 <- as_tibble(read.csv("input/20210811_corinne-marcus-output.csv")) %>%
                          filter(variable == "Surface Temperature (GSAT)") %>%
  filter(quantile == "0.5") %>%
  select(-model, -region, -unit, -climate_model, -reference_period_start_year,
        -reference_period_end_year, -quantile, -variable, -scenario) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "value",
               values_drop_na = TRUE
               ) %>%
  mutate(variable = "Global Mean Temperature Change (degC)") %>%
  mutate(scenario = "Reference")


rf_7wm2 <- as_tibble(read.csv("input/20210811_corinne-marcus-output.csv")) %>%
  filter(variable == "Effective Radiative Forcing") %>%
  filter(quantile == "0.5") %>%
  select(-model, -region, -unit, -climate_model, -reference_period_start_year,
         -reference_period_end_year, -quantile, -variable, -scenario) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "value",
               values_drop_na = TRUE
  ) %>%
  mutate(variable = "Radiative Forcing (Wm-2)") %>%
  mutate(scenario = "Reference")

combine <- rbind(temp_1p5, temp_paris, temp_7wm2, rf_1p5, rf_paris, rf_7wm2)

 combine$year <- as.integer(combine$year)

 #### Plot up Temperature and Radiative Forcing ####

 ggplot(combine, aes(year, value,  color = scenario)) +
  geom_line(size = 2) +
  xlim(2015,2100)+
  scale_color_manual(values=c("Blue2","Green3", "Red2")) +
   theme_minimal() +
   theme(text = element_text(size = 15))+
  facet_wrap(~variable, scales = "free")

#### Convert to CONUS ####
#  Global to CONUS mean temperature change estimated as CONUS Temp = 1.3*Global Temp + 0.34
 # from Sarofim et al., 2021 - climatic change
 temp_1p5 <- mutate(temp_1p5, value = (0.34 + value * 1.3))
 temp_paris <- mutate(temp_paris, value = (0.34 + value * 1.3))
 temp_7wm2 <- mutate(temp_7wm2, value = (0.34 + value * 1.3))

 #### Temperature relative to 1986-2005 ####
 # using 1995 as mid point
 
 avg <- as.numeric(summarise(filter(temp_1p5, year %in% 1995), mean(value)))
 temp_1p5 <- mutate(temp_1p5, value = value - avg)
 temp_paris <- mutate(temp_paris, value = value - avg)
 
 avg <- as.numeric(summarise(filter(temp_7wm2, year %in% 1995), mean(value)))
 temp_7wm2 <- mutate(temp_7wm2, value = value - avg)
 
 #### Save outputs ####
write.csv(temp_1p5, file = "1-temp_1p5.csv", row.names = F)
write.table(temp_paris, file = "1-temp_paris.csv", row.names = F)
write.table(temp_7wm2, file = "1-temp_7wm2.csv", row.names = F)
