# Introduction

# Use only one data-set: main.data.w.traits.csv. 
# That file contains data on all woody plants from three studied elevations

library(dplyr)

# Custom made function to create site x species matrix for ordination
source("code/contingencyTable.R")

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

# Check the data dimension
summary(data)
dim(data)
summary(data$tot_bio)

# Prepare data for the LRR plot (Figure 1 in the MS)
# 
DENSITY <- data %>%
  # filter((unified_names %in% wpnames) & !is.na(dbh)) %>%
  group_by(site, garden, treatment, unified_names)


BIOMASS <- data %>%
  group_by(site,garden,treatment)%>%
  summarise(tot_bio = sum(tot_bio))

RICHNESS <- data %>%
  group_by(site, garden, treatment) %>%
  summarise(sp_no = length(unique(unified_names)),
            simpson = vegan::diversity(tot_bio, index = "invsimpson"))

COMMDATA <- contingencyTable2(data,
                              "unified_names", "plotid", "tot_bio")
COMMVARS <- data %>%
  group_by(site,garden, treatment, plotid) %>%
  summarise()
