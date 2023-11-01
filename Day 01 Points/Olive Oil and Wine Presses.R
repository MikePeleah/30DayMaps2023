library(sf)
library(dplyr)
library(ggplot2) # tidyverse data visualization package
library(ggpubr)
library(maps)
library(extrafont) # use fonts in charts 

# set working directory
# setwd("V:/30DayMapChallenge/")

# import xkcd font, from https://github.com/ipython/xkcd-font, licensed under a Creative Commons Attribution-NonCommercial 3.0 License.
font_import(pattern="xkcd", prompt = FALSE)

# load world map
world <- map_data("world")

# read data for Olive Oil and Wine Presses 
# The Olive Oil and Wine Presses Database, compiled by Annalisa Marzano and Miko Flohr.
# https://oxrep.classics.ox.ac.uk/databases/olive_oil_and_wine_presses_database/
#
# remove record with wrong names and coordinates, recode country names
presses <- read.csv("oxrep_oliveoilandwinepressesdatabase.csv", stringsAsFactors = FALSE) %>%
  filter(sitename != "", loclong != 0, loclat != 0) %>%
  mutate(country= case_when(country == "Turkey" ~ "Turkiye",
                            country == "Palestinian Territories" ~ "Palestine",
                            .default = country))

# Summarise min and max coordinates 
presses %>% summarise(min_lon = min(loclong, na.rm=TRUE), 
                      max_lon = max(loclong, na.rm=TRUE), 
                      min_lat = min(loclat,  na.rm=TRUE), 
                      max_lat = max(loclat,  na.rm=TRUE))

# Summarise number of sites and presses by modern countries
presses_stat <- presses %>% group_by(country) %>%   
  summarise(n_sites = n(), n_presses = sum(presses)) %>% 
  arrange(desc(n_sites))

# Bar chart number of sites by modern countries
presses_bar <- ggplot(data = presses_stat, aes(y =  reorder(country, n_sites), x = n_sites)) + 
  geom_col(fill = "darkred", width = 0.85) + 
  geom_text(aes(label = n_sites), hjust = -.5, position = "stack", size = 3, color = "darkred", family="xkcd Script") +
  scale_x_continuous(limits = c(0, 70)) + 
  labs(title = "\n") + 
  # xlab ("Modern country") + ylab ("Number of sites") +
  ylab ("Modern country") + xlab ("Number of sites") +
  theme_minimal() + 
  theme(text=element_text(size=14, family="xkcd Script"))

# Map with point size corresponding to number of presses
presses_map <- ggplot() +
  geom_map(data = world, map = world, aes(map_id = region),
    color = "black", fill = "#EEEEEE", size = 0.1) + 
  geom_point(data = presses, aes(x = loclong, y = loclat, size = presses), 
             shape = 21, fill = "darkred", alpha=.66) +
  coord_sf(xlim = c(-10, 45), ylim = c(20, 55), expand = FALSE) + 
  labs(title = "Roman wine and oil presses sites", caption ="Source: The Olive Oil and Wine Presses Database,\ncompiled by Annalisa Marzano and Miko Flohr.\nThe OXREP Project. https://oxrep.classics.ox.ac.uk/databases/", x = NULL, y = NULL, size = "Number of presses") + 
  theme_minimal() + 
  theme(text=element_text(size=16, family="xkcd Script"), plot.caption = element_text(size=10))

# Combine map with bar chart 
presses_plot <- ggarrange(presses_map, presses_bar , widths = c(2, .66),
          ncol = 2, nrow = 1)

# show it 
presses_plot

# export to file 
ggsave(filename = "Day 01 Presses plot.jpg", presses_plot, device = "jpg", width = 10, height = 4, units = "in")

