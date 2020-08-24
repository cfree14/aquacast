
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tabulizer)
library(tidyverse)

# Directories
indir <- "data/bivalve_stocking_densities/references"
outdir <- "data/bivalve_stocking_densities/data"
plotdir <- "data/bivalve_stocking_densities/figures"

# Read AFDW to WW scalars
scalars <- read.csv(file.path(outdir, "Ricciardi_Bourget_1998_afdw2ww_scalars.csv"), as.is=T)

# PDF path
pdf_path <- file.path(getwd(), indir, "Smaal_etal_2019_Chapter23.pdf")

# Extract tables
pages_with_tables <- 25:28
table_list <- extract_tables(pdf_path, pages = pages_with_tables, method="stream")


# Build data
################################################################################

# Format table 1
table1 <- table_list[[1]] %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  # Column names
  setNames(c("system", "code", "country", "species_culture", "species_wild", "species_invasive", "area_sqkm", "depth_m", "volume_mm3")) %>% 
  # Remove first row
  slice(2:nrow(.)) %>% 
  # Format columns
  mutate(area_sqkm= area_sqkm %>% gsub(",", "", .) %>% as.numeric(),
         volume_mm3 = volume_mm3 %>% gsub(",", "", .) %>% as.numeric(),
         depth_m=recode(depth_m, "0-10"="5") %>% as.numeric())

# Format table 2
table2 <- table_list[[2]] %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  # Column names
  setNames(c("system", "code", "chl_mgm3", "phyt_mgC", "pp_gCm2yr", "system_pp", 
             "total_afdw_mt", "cultured_afdw_mt", "wild_afdw_mt", "invasive_afdw_mt", "clearance_rate", "clearange_rate_tot")) %>% 
  # Convert to numeric
  mutate(across(.cols=c(chl_mgm3:clearange_rate_tot), .fns=function(x) x  %>% as.character() %>% gsub(",", "", .) %>% as.numeric())) %>% 
  # Fix some code problems
  mutate(code=recode(code, "ΊΒ"="TB"))

# Format table 3a
table3a <- table_list[[3]] %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  setNames(c("system", "code", "rt_d", "pt_d", "ct_d", "ctrt", "log_ctrt", "ctpt", "log_ctpt", "references")) %>% 
  mutate(system=recode(system, "Chesapeake Bay"="Chesapeake Bay	present"),
         references=recode(references, 
                           "Ferreira et al. (2008) and Sequeira"="Ferreira et al. (2008) and Sequeira et al. (2008)",
                           "Heral et al. (1988), Bacher et al."="Heral et al. (1988), Bacher et al. (1998)",
                           "Tenore et al. (1982) and Filgueira"="Tenore et al. (1982) and Filgueira et al. (2010)",
                           "Pitcher and Calder (1998) and"="Pitcher and Calder (1998) and Stenton-Dozey et al. (2001)")) %>% 
  filter(code!="")

# Format table 3b
table3b <- table_list[[4]] %>% 
  as.data.frame() %>% 
  setNames(c("system", "code", "rt_d", "pt_d", "ct_d", "ctrt", "log_ctrt", "ctpt", "log_ctpt", "references")) %>% 
  mutate(system=recode(system, "South San Fransisco"="South San Fransisco	Bay"),
         system=ifelse(code=="WS94", "Western Wadden Sea 1994", system),
         system=ifelse(code=="WS14", "Western Wadden Sea 2014", system)) %>% 
  filter(code!="")

# Merge table3
table3 <- bind_rows(table3a, table3b) %>% 
  # Convert to numeric
  mutate(across(.cols=c(rt_d:log_ctpt), .fns=function(x) x  %>% as.character() %>% gsub("−", "-", .) %>% as.numeric())) %>% 
  # Fix some code
  mutate(code=recode(code, "OB"="DB", "GEL"="GE"))

# Merge tables
data <- table1 %>% 
  left_join(table2 %>% select(-system), by="code") %>% 
  left_join(table3 %>% select(-system), by="code") %>% 
  # Identify places in the best quadrant
  mutate(impacts=ifelse(log_ctrt>0&log_ctpt>0, "Low", "High")) %>% 
  # Format species 
  mutate(species_culture=recode(species_culture, 
                                "Argopecten purpura tus"="Argopecten purpuratus",
                                "C virginica"="Crassostrea virginica",
                                "C virgvitca"="Crassostrea virginica",
                                "M edulis"="Mytilus edulis",
                                "M galloprovincialis"="Mytilus galloprovincialis",
                                "C gigas/M galloprovincialis"="Crassostrea gigas, Mytilus galloprovincialis",
                                "C gigas/M edulis"="Crassostrea gigas, Mytilus edulis")) %>% 
  # Add AFDW (ash-free dry-weight) to WW (wet-weight) scalar
  left_join(scalars, by=c("species_culture"="species")) %>% 
  mutate(cultured_mt=cultured_afdw_mt * afdw_scalar) %>% 
  # Calculate cultured density
  mutate(cultured_mt_sqkm=cultured_mt/area_sqkm)
  
table(data$species_culture)

# Export data
write.csv(data, file=file.path(outdir, "Smaal_etal_2019_bivalve_carrying_capacity_data.csv"), row.names=F)


# Plot data
################################################################################

# Mean low impact density
data %>% 
  filter(impacts=="Low") %>% 
  pull(cultured_mt_sqkm) %>% 
  mean(na.rm=T)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot data
# CT/RT = clearance ratio; CT/PT = grazing ratio. 
g <- ggplot(data, aes(x=log_ctrt, y=log_ctpt, size=cultured_mt_sqkm, color=impacts)) +
  geom_point() + 
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Labels
  labs(x="log(Clearance ratio)", y="log(Grazing ratio)") +
  # Legends
  scale_color_discrete(name="Ecological impacts") +
  scale_size_continuous("Cultured bivalve\ndensity (mt/sqkm)") +
  # Theme
  theme_bw() + my_theme
g


# Export plot
ggsave(g, filename=file.path(plotdir, "figure_bivavle_carrying_capacity.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
