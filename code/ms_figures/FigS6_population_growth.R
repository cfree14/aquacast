

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
datadir <- "data/pop_growth/data"

# Read data
data_orig <- readRDS(file.path(datadir, "WB_UN_1960_2100_human_population_by_country.Rds"))


# Plot data
################################################################################

# Build global data
gdata <- data_orig %>% 
  group_by(source, year) %>% 
  summarize(npeople=sum(pop_size_50perc),
            npeople_lo=sum(pop_size_20perc),
            npeople_hi=sum(pop_size_80perc)) %>% 
  ungroup() %>% 
  mutate(source=recode_factor(source, 
                              "World Bank historical"="World Bank historical",
                              "UN WPP projections"="UN WPP projections"))

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

# Plot data
g1 <- ggplot(gdata, aes(x=year, y=npeople/1e9, color=source)) +
  geom_line() +
  geom_ribbon(mapping=aes(x=year, ymin=npeople_lo/1e9, ymax=npeople_hi/1e9), 
              inherit.aes=F, fill="red", alpha=0.3) +
  # Axis
  scale_y_continuous(lim=c(0,NA)) +
  scale_x_continuous(breaks=seq(1960,2100,10)) +
  # Legend
  scale_color_manual(name="", values=c("black", "red")) +
  # Labels
  labs(x="Year", y="Billions of people") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1


# Build national data
ndata <- data_orig %>% 
  filter(year %in% c(2020, 2100)) %>% 
  select(iso3, year, pop_size_50perc) %>% 
  spread(key="year", value="pop_size_50perc") %>% 
  rename(npeople2020='2020', npeople2100='2100') %>% 
  mutate(npeople_diff=npeople2100-npeople2020)

# Add to spatial data
world <- rnaturalearth::ne_countries(scale="small", type="countries", returnclas="sf") %>% 
  mutate(country_use=countrycode(sovereignt, "country.name", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
  left_join(ndata, by=c("iso3_use"="iso3"))

# Plot data
g2 <- ggplot() +
  geom_sf(data=world, mapping=aes(fill=npeople_diff/1e6), lwd=0.2, color="grey30") +
  # Legend
  scale_fill_gradient2(name="Change in population\nfrom 2020 to 2100\n(millions of people)",
                       low = "blue", high = "red", mid="white", midpoint = 0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="bottom")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.4,0.6))

# Export
ggsave(g, filename=file.path(plotdir, "FigS6_population_growth.png"), 
       width=6.5, height=3, units="in", dpi=600)


