# Aggregate FrEDI impacts to level we are interested in
# National Totals
# Ref - 1p5 scenario

# CH
# 9/24/2021
# updated 10/19/2021



temp_1p5 <- read.csv("2-FrEDI_magicc_temp_1p5.csv", sep = ",") %>% 
  aggregate_impacts(columns = c("annual_impacts"), aggLevels=c("all")) %>% 
  mutate(scenario = "magicc_1p5") %>% 
  filter(adaptation %in% c("2011 Emissions", "Reactive Adaptation", "N/A", 
                           "Adaptation", "Reasonably Anticipated Adaptation")) %>%
  select(-impactYear, -impactType)

unique(temp_1p5$sector) # double check we have all sectors

temp_paris <- read.csv("2-FrEDI_magicc_temp_paris.csv", sep = ",") %>% 
  aggregate_impacts(columns = c("annual_impacts"), aggLevels=c("all")) %>% 
  mutate(scenario = "magicc_paris") %>% 
  filter(adaptation %in% c("2011 Emissions", "Reactive Adaptation", "N/A", 
                           "Adaptation", "Reasonably Anticipated Adaptation")) %>%
  select(-impactYear, -impactType)

temp_7wm2 <- read.csv("2-FrEDI_magicc_temp_7wm2.csv", sep = ",") %>% 
  aggregate_impacts(columns = c("annual_impacts"), aggLevels=c("all")) %>% 
  mutate(scenario = "magicc_7wm2") %>% 
  filter(adaptation %in% c("2011 Emissions", "Reactive Adaptation", "N/A", 
                           "Adaptation", "Reasonably Anticipated Adaptation")) %>%
  select(-impactYear, -impactType)

# Combine dfs, filter for region = National Total, model = Average, 
# rename sectors, drop Asphalt Roads, convert to billions

combined <- as_tibble(rbind(temp_1p5,temp_paris, temp_7wm2)) %>% 
  filter(region == "National Total") %>% 
  filter(model %in% c("Average", "Interpolation")) %>% 
  select(sector, year, annual_impacts, scenario) %>% 
  pivot_wider(names_from = sector, 
              values_from = annual_impacts) %>% 
  rename("Ozone and PM2.5 Mortality" = "Air Quality")  %>% 
  rename("Labor (lost wages)" = "Labor") %>% 
  rename("Tropical Storm Wind Damages" = "Wind Damage") %>% 
  rename("Wildfire Health Effects & Suppression" = "Wildfire") %>% 
  rename("Extreme Temperature Mortality" = "Extreme Temperature") %>% 
  rename("Southwest Dust Health Effects" = "Southwest Dust") %>% 
  rename("Inland Flooding Damages" = "Inland Flooding") %>% 
  select(-"Asphalt Roads") %>% 
  pivot_longer(cols = c(3:19),
               values_to = "annual_impacts",
               names_to = "sector") %>% 
  mutate(annual_impacts = annual_impacts/10^9) 


timeseries_1p5 <-as_tibble(filter(combined, scenario == "magicc_1p5"))

timeseries_7wm2 <-as_tibble(filter(combined, scenario == "magicc_7wm2"))

diff <- mutate(timeseries_7wm2, diff = timeseries_7wm2$annual_impacts - timeseries_1p5$annual_impacts) %>% 
  select(-annual_impacts) %>% 
  rename("annual_impacts" = "diff") %>% 
  mutate(scenario = "Ref-1.5")

write.csv(diff, file = "3-climateimpacts.csv", row.names = F)

#### calculate national total ####
nt_2050 <- filter(diff, year == 2050) %>% 
  mutate(sum(annual_impacts))

nt_2100 <- filter(diff, year == 2090) %>% 
  mutate(sum(annual_impacts))

