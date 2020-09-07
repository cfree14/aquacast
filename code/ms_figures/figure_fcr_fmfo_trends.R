
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
tabledir <- "tables"

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


# Plot data
################################################################################

# Projection year
proj_years <- c(2030, 2050)

# Subset FIFOs
fifos <- fdata_long %>% 
  filter(parameter=="fifo")

# Plotting parameters
par(mfrow=c(2,4))

# Loop through and fit exponential decline
groups <- sort(unique(fifos$group))
fifo_proj <- data.frame(group=groups, fifo2030=NA, fifo2050=NA)
for(i in 1:length(groups)){
  
  # Subset data
  group_i <- groups[i]
  sdata <- filter(fifos, group==group_i)
  
  # Plot data
  plot(value ~ year, sdata, xlim=c(2000, max(proj_years)), ylim=c(0, 5),
       main=group_i, xlab="", ylab="FIFO")
  abline(h=1)
  
  # Fit model
  lmfit <- lm(log(value) ~ year, sdata)
  a <- exp(coef(lmfit)[1])
  b <- coef(lmfit)[2]
  
  # Plot fit
  curve(a*exp(b*x), from=2000, to=max(proj_years), n=100, add=T)
  
  # Plot and record prediction
  pred <- as.numeric(exp(predict(lmfit, data.frame(year=proj_years))))
  points(x=proj_years, y=pred, pch=16, cex=2)
  fifo_proj$fifo2030[i] <- pred[1]
  fifo_proj$fifo2050[i] <- pred[2]
  
  # Record predictions for GGPLOT graph
  yrs <- 2000:2050
  fifo_preds <- as.numeric(exp(predict(lmfit, data.frame(year=yrs))))
  pred_df <- data.frame(group=group_i, year=yrs, fifo=fifo_preds)
  if(i==1){pred_df1 <- pred_df}else{pred_df1 <- rbind(pred_df, pred_df1)}
  
}


# Build output table
################################################################################

my_theme <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.title.x=element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5))

# Reshape for plotting
fifos_g <- fifos %>%
  select(group, year, value) %>%
  rename(fifo=value)

fifo_proj_g <- fifo_proj %>% 
  gather(key="year", value="fifo", 2:3) %>% 
  mutate(year=recode(year, "fifo2030"="2030", "fifo2050"="2050") %>% as.numeric(),
         fifo_label=round(fifo, digits=3))

# Plot data
g <- ggplot(fifos_g, aes(x=year, y=fifo)) +
  facet_wrap(~group, ncol=4) +
  geom_point() +
  # Add regression lines
  geom_line(data=pred_df1) +
  # Add prediction point
  geom_point(data=fifo_proj_g, col="red") +
  # Add horizontal line
  geom_hline(yintercept = 1, linetype="dotted", color="grey40") +
  ggrepel::geom_text_repel(data=fifo_proj_g, 
                           mapping=aes(label=fifo_label), 
                           color="red", size=2.5) +
  # Little things
  xlim(2000,2050) +
  labs(x="", y='"Fish In, Fish Out"\n(FIFO) ratio') +
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_fifo_projections.png"), 
       width=6.5, height=4, units="in", dpi=600)

# Export data
fifo_preds <- pred_df1
save(fifos_g, fifo_proj_g, fifo_preds, file=file.path(feeddir, "fifo_trends_projections.Rdata"))


# Build output table
################################################################################

# Buildt table
out_table <- ts_end %>% 
  select(group, parameter, value_label) %>% 
  spread(key="parameter", value="value_label") %>% 
  select(fcr, fm_perc, fo_perc, fifo) %>% 
  left_join(fifo_proj)

# Export table
write.csv(out_table, file=file.path(tabledir, "TableS6_fifo_fcr_fmfo_stats.csv"), row.names=F)






