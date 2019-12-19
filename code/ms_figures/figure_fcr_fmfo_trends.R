
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "data/species/data"
feeddir <- "data/feed_params/processed"
plotdir <- "figures"

# Read data
load(file.path(datadir, "aquaculture_species_key.Rdata"))
load(file.path(feeddir, "Tacon_Metian_2008_and_2015_fcr_fmfo_data.Rdata"))


# Build data
################################################################################

# Feed groups in analysis
feed_groups <- sort(unique(data$feed_group))

# Merge T&M 2015 Table 1 and T&M 2008 Table 4

# Groups
sort(unique(tm08_t4$group))
sort(unique(tm15_t1$group))

# T&M 2008 Table 4
tm08_t4_format <- tm08_t4 %>% 
  spread(key="ingredient", value="value") %>% 
  rename(fmfo_perc_source=source, fo_perc="Fish oil", fm_perc="Fishmeal") %>% 
  select(group, year, fm_perc, fo_perc, fmfo_perc_source) %>% 
  mutate(group=recode(group, 
                      "Chinese carp species"="Chinese fed carps",
                      "Milkfish (Chanos chanos)"="Milkfish"))

# T&M 2015 Table 1
tm15_t1_format <- tm15_t1 %>% 
  select(group, year, percent_fed, fcr) %>% 
  mutate(group=recode(group, 
                      "Catfishes"="Catfish",
                      "Other freshwater & diadromous fishes"="Freshwater fish"),
         fcr_source="Tacon & Metian 2015") %>% 
  select(group, year, percent_fed, fcr, fcr_source)
  
# Merge data
fdata_wide <- tm15_t1_format %>% 
  full_join(tm08_t4_format) %>% 
  arrange(group, year) %>% 
  mutate(fifo=fcr * ( (fm_perc/100+fo_perc/100) / (0.224 + 0.0485) ) )
  

# Convert to long for plotting
fdata_long <- fdata_wide %>% 
  # Remove columns
  select(-c(fcr_source, fmfo_perc_source)) %>% 
  # Convert wide to long
  gather(key="parameter", value="value", 3:ncol(.)) %>% 
  # Format groups
  mutate(group=recode(group, 
                      "Freshwater fish"="Misc freshwater fish",
                      "Marine fish"="Misc marine fish")) %>% 
  filter(group %in% feed_groups) %>% 
  # Format parameter names
  mutate(parameter_label=recode(parameter, 
                          "fcr"="Feed conversion\nrate (FCR)",
                          "fifo"="Fish In, Fish Out\n(FIFO) ratio",
                          "fm_perc"="Percentage of feed\ncomposed of fishmeal (%)",
                          "fo_perc"="Percentage of feed\ncomposed of fish oil (%)", 
                          "percent_fed"="Percentage of\nproduction fed"),
         parameter_label=factor(parameter_label, levels=c("Percentage of\nproduction fed",
                                              "Fish In, Fish Out\n(FIFO) ratio", 
                                              "Feed conversion\nrate (FCR)",
                                              "Percentage of feed\ncomposed of fishmeal (%)",
                                              "Percentage of feed\ncomposed of fish oil (%)"))) %>% 
  # Format values
  mutate(value_label=ifelse(parameter %in% c("fcr", "fifo"), round(value,2), paste0(round(value,2), "%"))) %>% 
  # Remove NA values
  filter(!is.na(value))

# End point
ts_end <- fdata_long %>% 
  group_by(group, parameter) %>% 
  filter(year==max(year))

# Subset FIFO time series
fifo <- fdata_long %>% 
  filter(parameter=="fifo")


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=5),
                  # axis.title=element_text(size=9),
                  strip.text = element_text(size=5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.title = element_blank(),
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, hjust = 0.5))

# Plot
g <- ggplot(fdata_long, aes(x=year, y=value, color=parameter_label)) +
  # Facetting
  facet_grid(parameter_label ~ group, scales="free") +
  # Trend lines
  geom_line(lwd=0.3) +
  # Add and label end points
  geom_point(data=ts_end, mapping=aes(x=year, y=value), size=0.8) +
  ggrepel::geom_text_repel(data=ts_end, mapping=aes(x=year, y=value, label=value_label), size=2) +
  # Horizontal line in FIFO
  geom_hline(data = fifo, aes(yintercept = 1), linetype="dotted", color="grey70") +
  # Small things
  labs(x="", y="") +
  # xlim(c(1995, 2025)) +
  scale_x_continuous(breaks=seq(1995,2025,5)) +
  theme_bw() + my_theme +
  theme(legend.position = "none")
g  

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_fcr_fmfo_trends.png"), 
       width=6.5, height=5.5, units="in", dpi=600)


