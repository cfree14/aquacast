
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
plotdir <- "figures"
tabledir <- "tables"

# Read data
data <- read.csv(file.path(tabledir, "Free_etal_mariculture_species_database.csv"), as.is=T)


# Build data
################################################################################

# Reshape for plotting
tdata <- data %>% 
  select(class, species, a_source, b_source, linf_source, k_source, m_source) %>% 
  mutate(m_source=ifelse(m_source=="FishLife", m_source, paste(gsub("-", " ", m_source), "average", sep="-"))) %>% 
  gather(key="parameter", value="source", 3:ncol(.)) %>% 
  mutate(parameter=recode(parameter, 
                          "a_source"="LW a",
                          "b_source"="LW b",
                          "linf_source"= "Linf (cm)",
                          "k_source"="K",
                          "m_source"="M"),
         class=recode(class, 
                      "Finfish"="Finfish (n=122)",
                      "Bivalves"="Bivalves (n=22)"),
         source=factor(source, levels=c("FishLife", "Gentry et al. 2017", "FB species-average", "FB genus-average", "FB family-average")))


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot
g <- ggplot(tdata, aes(x=parameter, fill=source)) +
  geom_bar(position="fill") +
  scale_fill_discrete(name="Source") +
  labs(x="Life history trait", y="Proportion") +
  facet_wrap(~class) +
  theme_bw() + my_theme
g  

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS8_life_history_trait_sources.png"), 
       width=6.5, height=3, units="in", dpi=600)



