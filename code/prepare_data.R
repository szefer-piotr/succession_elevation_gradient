# Introduction

# Use only one data-set: main.data.w.traits.csv. 
# That file contains data on all woody plants from three studied elevations

library(dplyr)


# Custom made function to create site x species matrix for ordination
source("code/specific_functions.R")

# Read the data
vegetation.data <- read.csv('data/main.data.w.traits.csv')
life.forms <-  read.csv('data/suppl_plant_traits.csv')

# Create vector of woody plant names
woody.plants <- life.forms[life.forms$Life.form == "woody", ]$dsName

# Filter out the non-woody plants
data <- vegetation.data %>%
  filter(unified_names %in% woody.plants,
         treatment %in% c("c","h","i","p","f")) %>%
  mutate(plotid = paste(site, garden, treatment, sep = "_")) # create an original 

# Prepare tables for the LRR plot (Figure 1 in the MS)
DENSITY <- data %>%
  mutate(garden = paste(substr(site,1,1),garden, sep=""))%>%
  group_by(site,garden,treatment)%>%
  summarise(tot.stem.no = sum(no_stems))

BIOMASS <- data %>%
  mutate(garden = paste(substr(site,1,1),garden, sep=""))%>%
  group_by(site,garden,treatment)%>%
  summarise(tot_bio = sum(tot_bio))

RICHNESS <- data %>%
  mutate(garden = paste(substr(site,1,1),garden, sep=""))%>%
  group_by(site, garden, treatment) %>%
  summarise(sp_no = length(unique(unified_names)),
            simpson = vegan::diversity(tot_bio, index = "invsimpson"))

COMMDATA <- contingencyTable2(data,
                              "unified_names", "plotid", "tot_bio")

COMMVARS <- data %>%
  group_by(site,garden, treatment, plotid) %>%
  summarise()
