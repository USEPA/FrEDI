# Code to produce LTS figure using FrEDI 
# Morgan Browning

# updated 10/19/2021

# bring in design elements from NCS-LTS figures
source("themes.R")

# Categories

labor_cat <- c("Labor (lost wages)")
water_cat <- c("Water Quality", "Winter Recreation", "Inland Flooding Damages")
coastal_cat <- c("Coastal Properties","High Tide Flooding and Traffic","Tropical Storm Wind Damages")
health_cat <- c("Ozone and PM2.5 Mortality","Valley Fever","Extreme Temperature Mortality","Wildfire Health Effects & Suppression","Southwest Dust Health Effects")
infrastructure_cat <- c("Rail","Roads","Electricity Demand and Supply","Urban Drainage","Electricity Transmission and Distribution")

# read in csv file
avoided_costs_data <- read.csv("3-climateimpacts.csv", sep = ",") %>%
  # adding a new column for high-level category
  mutate(category = case_when(
    sector %in% labor_cat ~ "Labor",
    sector %in% water_cat ~ "Water Resources",  
    sector %in% coastal_cat ~ "Coastal",
    sector %in% health_cat ~ "Health Impacts",
    sector %in% infrastructure_cat ~ "Infrastructure",
    TRUE ~ NA_character_)) %>%
  rename(annual_impacts15 = annual_impacts) %>%
  # conversion from 2015$ to 2017$ to match Non-CO2 MACCs
  mutate(annual_impacts17 = annual_impacts15*107.747/104.691) %>% 
  mutate(gutcheck = annual_impacts17-annual_impacts15) %>% # diff between 2015$ and 2017$ values
  # show only 2020 and onward
  filter(year >= 2020)

avoided_costs_aggregate <- avoided_costs_data %>%
  group_by(year,category) %>%
  # summing annual_impact values across all sectors within a category
  summarize(annual_impacts = sum(annual_impacts17), .groups = "drop") %>%
  # order for categories
  mutate(category = factor(category, levels = c("Water Resources","Labor","Coastal","Infrastructure","Health Impacts")))

avoided_costs_colors <- c("Water Resources" = `Fossil w/ CCS`,
                          "Labor" = `Nuclear`,
                          "Coastal" = `Renewables`,
                          "Infrastructure" = `Fossil`,
                          "Health Impacts" = `Non-CO2`)

# Area Chart

avoided_costs_plot <- avoided_costs_aggregate %>%
  ggplot() +
  geom_area(aes(x=year,y=annual_impacts,fill=category,group=category)) +
 labs(x="",y="Billion 2017$ US",fill="") +
  ylim(0,350) +
  scale_x_continuous(breaks = seq(2020,2090, by = 10), expand = c(0,.5)) 

avoided_costs_plot

# Bar Chart with 2050, 2070, 2090

avoided_costs_bars <- avoided_costs_aggregate %>%
  filter(year %in% c(2050,2070,2090)) %>%
  ggplot() +
  geom_bar(aes(x=year,y=annual_impacts,fill=category,group=category),stat="identity",position="stack",width=10) +
  labs(x="",y="Billion 2017$ US",fill="") +
  ylim(0,350) +
  scale_x_continuous(breaks = c(2050,2070,2090))

avoided_costs_bars
