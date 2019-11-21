
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(readxl)
library(tidyverse)

# Directories
indir <- "data/feed_params/raw"
outdir <- "data/feed_params/processed"
plotdir <- "data/feed_params/figures"

# Read data
tm15_t1_orig <- read_excel(file.path(indir, "Tacon_Metian_2015_Table1.xlsx"), na="–")
tm08_t3_orig <- read_excel(file.path(indir, "Tacon_Metian_2008_Table3.xlsx"), na="–")
tm08_t4_orig <- read_excel(file.path(indir, "Tacon_Metian_2008_Table4.xlsx"), na="–")

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Tacon & Metian (2015) Table 1
################################################################################

# Format
tm15_t1 <- tm15_t1_orig %>% 
  select(-sort_orig)

# Inspect
str(tm15_t1)

# Plot
g <- ggplot(tm15_t1, aes(x=year, y=fcr, color=group)) +
  geom_line(alpha=0.8) +
  expand_limits(y=1) +
  labs(x="", y="Feed conversion ratio (FCR)", title="FCR trends from Tacon & Metian (2015)") +
  scale_color_discrete(name="Species group") +
  scale_linetype_discrete(name="FCR estimate") +
  theme_bw()
g

# Tacon & Metian (2008) Table 3
################################################################################

# I never finished formatting this

# Inspect
str(tm08_t3_orig)

# Format
tm08_t3 <- tm08_t3_orig %>% 
  # Break ranges
  # Note: I don't understand the regex but it kind of works
  mutate(fcr_min=str_extract(fcr_range,  "[^–]+") %>% as.numeric(),
         fcr_max=str_extract(fcr_range, "(?<=–).*?(?= )") %>%  as.numeric(),
         fcr_med=str_extract(fcr_range,  "(?<=\\().+?(?=\\))") %>% as.numeric()) %>% 
  # Arrange
  select(sort_orig:fcr_range, fcr_min, fcr_max, fcr_med, everything())


# Tacon & Metian (2008) Table 4
################################################################################

# Inspect
str(tm08_t4_orig)

# Plot
g <- ggplot(tm08_t4_orig, aes(x=year, y=fcr, color=group)) +
  geom_line() +
  expand_limits(y=1) +
  labs(x="", y="Feed conversion ratio (FCR)", title="FCR trends from Tacon & Metian (2008)") +
  scale_color_discrete(name="Species group") +
  theme_bw()
g

# Format data
tm08_t4 <- tm08_t4_orig %>% 
  # Select columns of interest
  select(group, year, fm_perc:fo_perc_iffo) %>% 
  # Format IFFO ranges
  mutate(group=recode(group, 
                      "Miscellaneous freshwater carnivorous fish"="Freshwater fish",
                      "Milkfish"="Milkfish (Chanos chanos)"),
         fm_perc_iffo=recode(fm_perc_iffo, "45–55"="50") %>% as.numeric(),
         fo_perc_iffo=recode(fm_perc_iffo, "10–20"="15") %>% as.numeric()) %>% 
  # Gather for plotting
  gather(key="key", value="value", 3:ncol(.)) %>% 
  # Add source/ingredient columns for plotting
  mutate(source=ifelse(grepl("iffo", key), "IFFO", "Tacon & Metian 2008"),
         ingredient=ifelse(grepl("fo", key), "Fish oil", "Fishmeal")) %>% 
  # Rearrange
  select(group, ingredient, source, year, value) %>% 
  arrange(group, ingredient, source, year) %>% 
  filter(!is.na(value)) %>% 
  # Remove IFFO estimates
  filter(source!="IFFO")

# Plot
g <- ggplot(tm08_t4, aes(x=year, y=value, color=ingredient)) +
  geom_line() +
  facet_wrap(~ group, ncol=4) +
  labs(x="", y="Percent of feed", title="Feed composition trends from Tacon & Metian (2008)") +
  scale_color_discrete(name="Feed ingredient") +
  theme_bw() + my_theme +
  theme(legend.position=c(0.87,0.15),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_feed_comp_trends.png"), 
       width=6.5, height=5.5, units="in", dpi=600)



# Build data
################################################################################

# FCR table
fcr <- tm15_t1_orig %>% 
  filter(year==2020) %>% 
  select(group, fcr) %>% 
  rename(fcr15=fcr) %>% 
  mutate(group=recode(group, 
                      "Catfishes"="Catfish",
                      "Chinese fed carps"="Chinese carp species", 
                      "Other freshwater & diadromous fishes"="Miscellaneous freshwater carnivorous fish"))

# FM/FO table
fmfo <- tm08_t4_orig %>% 
  filter(year==2020) %>% 
  select(group, fcr, fo_perc, fm_perc) %>% 
  rename(fcr08=fcr)

# Merge tables
data <- fcr %>% 
  full_join(fmfo, by="group") %>% 
  mutate(fmfo_perc=fo_perc+fm_perc,
         feed_group=recode(group, 
                              "Chinese carp species"="Chinese carps",
                              "Miscellaneous freshwater carnivorous fish"="Misc freshwater fish",
                              "Marine fish"="Misc marine fish",
                              "Freshwater crustaceans"="Misc freshwater crustaceans"),
         feed_group_long=recode(feed_group, 
                                   "Tilapia"="Tilapia (Oreochromis spp and others)",
                                   "Milkfish"="Milkfish (Chanos chanos)",
                                   "Eel"="Eel (Anguilla spp and others)",
                                   "Catfish"="Catfish (Siluriformes spp)",
                                   "Shrimp"="Shrimp (Penaeus spp and others)")) %>% 
  select(feed_group, feed_group_long, fcr15, fcr08, fm_perc, fo_perc, fmfo_perc)

# Build tuna layer
tuna <- tibble(feed_group="Tuna",
               feed_group_long="Tuna", 
               fcr15=6, # median(c(4.6, 7.4))
               fcr08=NA,
               fm_perc=8, # same as salmon based on Froehlich et al. (2018)
               fo_perc=6) %>% 
  mutate(fmfo_perc=fm_perc+fo_perc)

# Build non-fed layer
nonfed <- tibble(feed_group="Non-fed",
               feed_group_long="Non-fed mariculture species", 
               fcr15=0, # median(c(4.6, 7.4))
               fcr08=0,
               fm_perc=0, # same as salmon based on Froehlich et al. (2018)
               fo_perc=0) %>% 
  mutate(fmfo_perc=fm_perc+fo_perc)

# Combine
data1 <- rbind(data, tuna, nonfed)

# Export
write.csv(data1, file=file.path(outdir, "Tacon_Metian_group_fcrs_and_fmfo_feed_percs.csv"), row.names=F)


# Export all data
################################################################################

# Export
save(tm08_t3, tm08_t4, tm15_t1, file=file.path(outdir, "Tacon_Metian_2008_and_2015_fcr_fmfo_data.Rdata"))




