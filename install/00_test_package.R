###### Load Packages ######
require(devtools)
require(tidyverse)
###### Package Source Directory ######
### Relative to documents in the home directory, i.e. "~/Documents/
package_name      <- "FrEDI"
package_location  <- getwd(); package_location
package_sourceDir <- file.path(package_location, "..", package_name)

###### Library ######
list.files(package_sourceDir)
list.dirs(package_sourceDir, recursive = F)

###### Test Functions ######
### Uncomment to generate and test example data
load_all(package_sourceDir)
testx <- run_fredi()

load_all(package_sourceDir)
# plotx <- get_plots(testx)
# # (testx %>% filter(region=="National Total") %>% filter(model_type=="GCM") %>% mutate(impact_billions = annual_impacts/10^9))$impact_billions %>% range(na.rm=T)
plotx <- testx %>% filter(sector=="Air Quality") %>% get_plots(plotTypes = "ribbon")
plotx$ribbon$`Air Quality`$regional
# plotx$heatmaps$GCM
# plotx <- testx %>% filter(region=="National Total") %>% filter(model_type=="SLR") %>% get_plots(plotTypes = "heatmaps")
# plotx$heatmaps$SLR
# # plotx <- testx %>% filter(region=="National Total") %>% filter(model_type=="SLR") %>% get_plots(plotTypes = "heatmaps")
plotx <- get_plots(testx, save=T, directory = package_location %>% file.path("..", "misc", "test_plots"))

# plotx$heatmaps$GCM
(!((testx %>% filter(region=="National Total") %>% filter(model_type=="GCM"))$annual_impacts == 0)) %>% which %>% length
(!is.na((testx %>% filter(region=="National Total") %>% filter(model_type=="GCM"))$annual_impacts == 0)) %>% which %>% length

(!((testx %>% filter(region=="National Total") %>% filter(sector=="Air Quality"))$annual_impacts == 0)) %>% which %>% length

testx %>% filter(region=="National Total") %>%
  # filter(model_type=="GCM") %>%
  # filter(sector=="Coastal Properties") %>%
  filter(year %in% seq(2010, 2090, by=5)) %>%
  filter(sector=="Air Quality") %>%
  # filter(annual_impacts!=0) %>%
  mutate(impact_billions=annual_impacts/10^9) %>%
  # arrange(desc(group_name)) %>%
  # mutate(valueColumn = valueColumn / (10^3)^heat_power1000 ) %>%
  mutate(group_name = sector %>% paste(adaptation, sep=", ") %>% factor) %>%
  # mutate(group_name = group_name %>% factor(levels=rev(levels(group_name)))) %>%
  ggplot(., aes(x=year, y=group_name, fill=impact_billions)) +
  # ggplot(., aes(x=year, y=group_name, fill=annual_impacts)) +
  geom_tile(color = "white") +
  scale_fill_gradient2("Billions", low="darkblue", high="darkred") +
  scale_x_continuous("Year", breaks=seq(2020, 2080, 20), limits = c(2010, 2090)) +
  scale_y_discrete("Year") +
  facet_grid(region~model) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    # legend.key.width = unit(def_heat_lgd_w, "cm"),
    axis.text = element_text(size=8)
  ) +
  ggtitle("Heatmap")
