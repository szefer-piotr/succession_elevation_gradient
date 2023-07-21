# source('code/prepare_data.R')

# RICHNESS # both sp_no and diversity
# BIOMASS  # Biomass
# DENSITY

library(ggplot2)
library(dplyr)

BIOMASS <- BIOMASS %>%
  mutate(elevation = ifelse(site == 'wanang', 200, ifelse(site == "numba", 750, 1900)))

RICHNESS <- RICHNESS %>%
  mutate(elevation = ifelse(site == 'wanang', 200, ifelse(site == "numba", 750, 1900)))

DENSITY <- DENSITY %>%
  mutate(elevation = ifelse(site == 'wanang', 200, ifelse(site == "numba", 750, 1900)))

bio.plot <- ggplot(BIOMASS %>%
                     filter(treatment == "c"), aes(x = elevation, y = tot_bio)) + 
  stat_smooth(lwd = 2) + 
  stat_summary(fun = 'mean',
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'pointrange', lwd = 1.2, cex = 1)+
  theme_bw() + 
  xlab("Elevation") + ylab('Total aboveground biomass')

rich.plot <- ggplot(RICHNESS %>%
                      filter(treatment == "c"), aes(x = elevation, y = sp_no )) + 
  stat_smooth(lwd = 2) + 
  stat_summary(fun = 'mean',
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'pointrange', lwd = 1.2, cex = 1)+
  theme_bw() + 
  xlab("Elevation") + ylab('Total number of woody species')

div.plot <- ggplot(RICHNESS %>%
                     filter(treatment == "c"), aes(x = elevation, y = simpson )) + 
  stat_smooth(lwd = 2) + 
  stat_summary(fun = 'mean',
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'pointrange', lwd = 1.2, cex = 1)+
  theme_bw() + 
  xlab("Elevation") + ylab("Simpson's diersity")

dens.plot <- ggplot(DENSITY %>%
                      filter(treatment == "c"), aes(x = elevation, y = tot.stem.no)) + 
  stat_smooth(lwd = 2) + 
  stat_summary(fun = 'mean',
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'pointrange', lwd = 1.2, cex = 1)+
  theme_bw() + 
  xlab("Elevation") + ylab('Number of woody stems per plot')
