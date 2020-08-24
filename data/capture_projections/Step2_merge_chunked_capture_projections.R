
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)

# Directories
datadir <- "data/capture_projections/data/chunks_eez"
outputdir <- "data/capture_projections/data"


# Merge data
################################################################################

# Files
files <- list.files(datadir)

# Merge data
data_orig <- purrr::map_df(files, function(x){
  
  # Read files
  df <- readRDS(file.path(datadir, x))
  
})

# Format data
data <- data_orig %>% 
  # Format columns
  mutate(rcp=recode(rcp, 
                    "RCP26"="RCP 2.6", 
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"))

# Calculate statistics and check plots
stats <- data %>% 
  group_by(rcp, scenario, year) %>% 
  summarize(catch_mt=sum(catch_mt),
            profits_usd=sum(profits_usd))


# Plot check
g <- ggplot(stats, aes(x=year, y=catch_mt/1e6, color=rcp, linetype=scenario)) +
  geom_line() + 
  labs(x="Year", y="Catch (millions mt)") +
  scale_color_discrete(name="Emissions scenario") +
  theme_bw()
g

# Export data
saveRDS(data, file=file.path(outputdir, "Free_etal_2020_fish_proj_by_rcp_mgmt_eez_isscaap.Rds"))



