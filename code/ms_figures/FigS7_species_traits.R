
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

# Number of orders/families
n_distinct(data$order)
n_distinct(data$family)

# Sample size stats
table(data$class)
nstats <- data %>% 
  group_by(class, isscaap) %>% 
  summarize(n=n()) %>% 
  arrange(class, n) %>% 
  mutate(class_label=recode(class, 
                            "Finfish"="Finfish (n=122)",
                            "Bivalves"="Bivalves (n=22)"),
         class_label=factor(class_label, levels=c("Finfish (n=122)", "Bivalves (n=22)")))
nstats$isscaap_order <- factor(nstats$isscaap, level=nstats$isscaap)


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

# Plot sample size stats
g1 <- ggplot(nstats, aes(x=isscaap_order, y=n, fill=class_label)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="", y="Number of species", tag="A") +
  scale_fill_discrete(name="") +
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.3))
g1

# Linf (cm) density
g2 <- ggplot(data, aes(x=linf_cm, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Asymptotic length (Linf, cm)", y="Density", tag="B") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# K density
g3 <- ggplot(data, aes(x=k, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Growth coefficient (K)", y="Density", tag="C") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g3

# Natural mortality density
g4 <- ggplot(data, aes(x=m, fill=class)) +
  geom_density(alpha=0.8) +
  labs(x="Natural mortality (M)", y="Density", tag="D") +
  theme_bw() + my_theme +
  theme(legend.position="none")
g4

# Assemble plots
g <- grid.arrange(g1, g2, g3, g4, layout_matrix=matrix(c(1,1,1,2,3,4), ncol=3, byrow=T))

# Export plots
ggsave(g, filename=file.path(plotdir, "FigS7_species_growth_params.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



