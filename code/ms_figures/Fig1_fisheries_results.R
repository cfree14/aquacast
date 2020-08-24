

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/capture_projections/data"

# Read data
gdata <- readRDS(file.path(datadir, "Free_etal_2020_global_projections_with_pop_data.Rds"))
cdata <- readRDS(file.path(datadir, "Free_etal_2020_national_projections_with_pop_data.Rds"))


# Format
gdata <- gdata %>% 
  mutate(scenario=recode_factor(scenario, 
                                "No Adaptation"="Business-as-usual",
                                "Full Adaptation"="Progressive reforms"))


# Build data
################################################################################

# Calculate change in per capita meat supply from 2012 to 2100 by RCP, scenario, country
stats_rcp <- cdata %>% 
  filter(year%in%c(2012,2100)) %>% 
  select(rcp, scenario, country, iso3, year, meat_kg_person) %>% 
  spread(key="year", value="meat_kg_person") %>% 
  rename(meat_kg_person_2012="2012", meat_kg_person_2100="2100") %>% 
  mutate(meat_kg_person_diff=meat_kg_person_2100-meat_kg_person_2012) %>%   
  # Cap values for plotting
  mutate(meat_kg_person_diff_cap=pmax(meat_kg_person_diff, -30) %>% pmin(., 30))

# Calculate mean change in per capita meat supply from 2012 to 2100 across RCPS by scenario, country
stats_avg <- stats_rcp %>% 
  # Calculate mean
  group_by(scenario, country, iso3) %>% 
  summarize(meat_kg_person_diff_avg=mean(meat_kg_person_diff)) %>% 
  ungroup() %>% 
  # Reduce to full adaptation
  filter(scenario=="Full Adaptation") %>% 
  # Cap values for plotting
  mutate(meat_kg_person_diff_avg_cap=pmax(meat_kg_person_diff_avg, -30) %>% pmin(., 30))


# Inspect distrivution to determine cap
hist(stats_avg$meat_kg_person_diff_avg_cap, breaks=seq(-1000,6000,1))
abline(v=c(-30,30))

# Get world
world <- rnaturalearth::ne_countries(scale="small", type = "countries", returnclass = "sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))

# Add data to SF
world_sf <- world %>% 
  left_join(stats_avg, by="iso3")


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=8),
                    plot.title=element_blank(),
                    plot.tag=element_text(size=10, face="bold"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot global results
##############################

# Seafood production
g1 <- ggplot(gdata, aes(x=year, y=meat_mt/1e6, color=rcp, linetype=scenario)) +
  # Add rectangles
  # geom_rect(xmin=2051, ymin=0, xmax=2060, ymax=max(gdata$meat_mt)/1e6, fill="grey80", color=NA, inherit.aes = F) +
  # geom_rect(xmin=2091, ymin=0, xmax=2100, ymax=max(gdata$meat_mt)/1e6, fill="grey80", color=NA, inherit.aes = F) +
  # Add lines
  geom_line() +
  # Limits
  ylim(c(0,NA)) +
  scale_x_continuous(breaks=c(2012, seq(2020, 2100, 10))) +
  # Labels
  labs(x="", y="Seafood production\n(millions of mt of edible meat)", tag="a") +
  # Legend
  scale_color_manual(name="Climate scenario", values=rev(RColorBrewer::brewer.pal(4, "RdBu"))) +
  scale_linetype_manual(name="Management scenario", values=c(2,1)) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position=c(0.4,0.25),
        legend.box = "horizontal",
        legend.spacing=unit(0.01, 'cm'),
        legend.box.spacing = unit(0.01, 'cm'),
        legend.key.size = unit(0.3, 'cm'),
        axis.title.x=element_blank())
g1  

# Per capita seafood production
gdata1 <- gdata %>% filter(!is.na(meat_kg_person))
g2 <- ggplot(gdata1, aes(x=year, y=meat_kg_person, color=rcp, linetype=scenario)) +
  # Add rectangles
  # geom_rect(xmin=2051, ymin=0, xmax=2060, ymax=max(gdata1$meat_kg_person), fill="grey80", color=NA, inherit.aes = F) +
  # geom_rect(xmin=2091, ymin=0, xmax=2100, ymax=max(gdata1$meat_kg_person), fill="grey80", color=NA, inherit.aes = F) +
  geom_line() +
  # Axes
  ylim(c(0,NA)) + 
  scale_x_continuous(breaks=c(2012, seq(2020, 2100, 10))) +
  # Labels
  labs(x="", y="Seafood production per capita\n(kg of meat per person)", tag="b") +
  # Legend
  scale_color_manual(name="Climate scenario", values=rev(RColorBrewer::brewer.pal(4, "RdBu"))) +
  scale_linetype_manual(name="Management scenario", values=c(2,1)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title.x=element_blank())
g2

# Plot map
##############################

# Plot map
plot_title <- "Change in seafood production\nper capita from 2012 to 2100\n(Δ kg of meat per person)"
g3 <- ggplot() +
  geom_sf(data=world_sf, mapping=aes(fill=meat_kg_person_diff_avg_cap), lwd=0.05, color="grey30") +
  # Labels
  labs(tag="c") +
  # Legend
  scale_fill_gradientn(name=plot_title, colors=RColorBrewer::brewer.pal(n=9, "RdBu"), na.value = "grey80",
                       breaks=seq(-30,30,10), labels=c("≤-30", seq(-20,20,10), "≥30")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.13, 0.3),
        legend.direction = "horizontal",
        axis.title.x=element_blank())
g3


# Histograms
##############################

# Build data for clever histogram plots
hdata <- stats_rcp %>% 
  # Add bin
  mutate(delta_meat_bin=cut(meat_kg_person_diff_cap, breaks=c(-1000, seq(-10, 10, 1), 1000))) %>% 
  # Summarize by bin
  group_by(rcp, scenario, delta_meat_bin) %>% 
  summarize(n=n()) %>% 
  mutate(n_plot=ifelse(scenario=="No Adaptation", n*-1, n)) %>% 
  ungroup() %>% 
  # Reclass mgmt scenario
  mutate(scenario=recode_factor(scenario,
                                "No Adaptation"="Business-as-usual", 
                                "Full Adaptation"="Progressive reforms"))

g4 <- ggplot(hdata, aes(x=delta_meat_bin, y=n_plot, fill=scenario)) +
  geom_bar(stat="identity") +
  facet_wrap(~rcp, nrow=1) + 
  coord_flip() +
  # Reference line
  geom_vline(xintercept=11.5) +
  # Axis
  scale_y_continuous(breaks=seq(-40,40,20), labels=abs(seq(-40,40,20)), lim=c(-40,40)) +
  # Labels
  labs(y="Number of countries", x=plot_title, tag="d") +
  scale_fill_manual(name="Management scenario", values=RColorBrewer::brewer.pal(4, "RdBu")[c(1, 4)]) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="bottom",
        legend.margin=unit(0, "cm"))
g4




# Merge and export
##############################

# Merge plots and export
layout_matrix <- matrix(data=c(1,2,3,3, 4,4), nrow=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix, heights=c(0.3, 0.4, 0.3))

# Export plots
ggsave(g, filename=file.path(plotdir, "Fig1_fisheries_results.png"), 
       width=6.5, height=8, units="in", dpi=600)
  
  
  







# 
# Plot national results
##############################
# 
# # National per capita trends
# cdata_stats <- cdata %>%
#   # Classify countries as experiencing increasing/descreasing per capita supply
#   select(rcp, scenario, country, iso3, year, npeople, meat_kg_person) %>% 
#   filter(year %in% c(2020, 2050, 2100)) %>% 
#   group_by(rcp, scenario, country, iso3) %>% 
#   mutate(trend=ifelse(meat_kg_person > meat_kg_person[year==2020], "increase", "decrease")) %>% 
#   ungroup() %>% 
#   filter(year!=2020) %>%
#   # Tally countries experiencing increasing/decreasing per capita supply
#   group_by(rcp, scenario, year, trend) %>%
#   summarise(ncountries=n(),
#             npeople=sum(npeople)) %>% 
#   ungroup() %>% 
#   # Reduce to just decreasing counts
#   filter(trend=="decrease")
# 
# # Plot data
# g3 <- ggplot(cdata_stats, aes(x=year, y=ncountries, fill=rcp)) +
#   facet_grid(~scenario) +
#   geom_bar(stat="identity", position = "dodge") +
#   labs(y="Number of nations\nwith decreased seafood production per capita") +
#   theme_bw()
# g3
# 
# # Plot data
# g4 <- ggplot(cdata_stats, aes(x=year, y=npeople/1e9, fill=rcp)) +
#   facet_grid(~scenario) +
#   geom_bar(stat="identity", position = "dodge") +   
#   labs(y="Billions of people\nwith decreased seafood production per capita") +
#   theme_bw()
# g4
# 
#   
#   
# 
# 
# 
# 
# 

