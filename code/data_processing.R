# rm(list = ls())
library(dplyr)
library(ggplot2)
library(vegan)

# 0. Read data ----

mainData <- read.csv("data/main_data_v2.csv")
trait.dat <- read.csv("data/suppl_plant_traits.csv")
mainDataT <- read.csv("data/main.data.w.traits.csv")

# Wanang has only total stem abundance, no data on stems (no dbh values),
# Since I am not calculating basal area for the rest of the sites but only focus on abundance I can move tot abu to the dbh column
# to build a data summary
mainData[mainData$site == "wanang", ]$dbh <- mainData[mainData$site == "wanang", ]$tot_abu

# Change treatment labels
mainData[mainData$treatment == "FUNGICIDE", ]$treatment <- "f"
mainData[mainData$treatment == "I", ]$treatment <- "i"
mainData[mainData$treatment == "WEEVIL125", ]$treatment <- "h"
mainData[mainData$treatment == "WEEVIL25", ]$treatment <- "lh"
mainData[mainData$treatment == "CONTROL", ]$treatment <- "c"
mainData[mainData$treatment == "PREDATOR", ]$treatment <- "p"
mainData[mainData$treatment == "INSECTICIDE", ]$treatment <- "i"

# Convert tot_bio into numeric values (when decimal points are being separated with commas)
mainData$tot_bio <- gsub(",",".",mainData$tot_bio)
mainData$tot_bio <- as.numeric(mainData$tot_bio)

mainDataT$tot_bio <- gsub(",",".",mainDataT$tot_bio)
mainDataT$tot_bio <- as.numeric(mainDataT$tot_bio)

# Dbh is sometimes written as "no stems", zero.
mainData[mainData$dbh == "no stems",] # NA is dedicated for plants that are not woody.
mainData$dbh <- as.numeric(mainData$dbh)


# 1. Total biomass data ----
getBioData <- function (mainData){
  
  # produces data-set for with raw total biomass for the total plant biomass 
  # for individual plots and treatments along elevation gradients.
  
  # Creates a lookup character vector (primary key).
  lookup <- paste(mainData$plot,
                  mainData$treatment,
                  mainData$garden,
                  mainData$unified_names,
                  mainData$site)
  
  # Exclude duplicated primary keys.
  dupl <- !duplicated(lookup)
  
  # How many duplicated rows were there?
  print(length(dupl))
  
  # Get species data without duplicates
  biodata <- mainData[!duplicated(lookup),c("unified_names","plot","treatment","garden",
                                            "tot_bio",
                                            "site")]
  
  # Unify the treatments
  # biodata[biodata$treatment == "FUNGICIDE", ]$treatment <- "f"
  # biodata[biodata$treatment == "I", ]$treatment <- "i"
  # biodata[biodata$treatment == "WEEVIL125", ]$treatment <- "h"
  # biodata[biodata$treatment == "WEEVIL25", ]$treatment <- "lh"
  # biodata[biodata$treatment == "CONTROL", ]$treatment <- "c"
  # biodata[biodata$treatment == "PREDATOR", ]$treatment <- "p"
  # biodata[biodata$treatment == "INSECTICIDE", ]$treatment <- "i"
   
  # Correct garden codes
  biodata[biodata$site == "numba", ]$garden <- paste("n", biodata[biodata$site == "numba", ]$garden, 
                                                     sep = "")
  biodata[biodata$site == "yawan", ]$garden <- paste("y", biodata[biodata$site == "yawan", ]$garden,
                                                     sep = "")
  biodata$garden <- tolower(biodata$garden)
  
  # Re-factor with increasing elevation
  biodata$site <- factor(biodata$site, levels = c("wanang", "numba", "yawan"))
  
  # unique(biodata$garden)
  
  # Summarised bio data. Sum of biomasses in individual plots
  # bdat <- biodata %>%
  #   group_by(site,treatment, garden) %>%
  #   summarise(bio = sum(tot_bio))
  
  # Check where values seem problematic.
  # ggplot(bdat, aes(x = site, y=bio))+
  #   geom_jitter(alpha = 0.15,width = 0.1, size = 2)+
  #   stat_summary()+
  #   geom_line()+
  #   facet_wrap(~treatment, scales = "free")
  
  return(biodata)
  
}

# elev.dat <- BIODATA
# trait.dat
# 2. Combine trait data with main data-set ----
combineTraits <- function (elev.dat, trait.dat){
  
  # Get rid of the 'X' column
  elev.dat <- elev.dat[, !(names(elev.dat) %in% ("X"))]
  
  # Assign species names to each row
  rownames(trait.dat) <- trait.dat$dsName
  
  # Combine main data with life form data
  elev.dat$life.form <- trait.dat[elev.dat$unified_names, "Life.form"]
  return(elev.dat)
  
}

# 3. Density data ----
wpnames <- trait.dat[trait.dat$Life.form == "woody", ]$dsName
wpnames <- wpnames[!is.na(wpnames)]

DENSITY <- mainData %>%
  filter((unified_names %in% wpnames) & !is.na(dbh)) %>%
  group_by(site, garden, treatment, plot, unified_names) %>%
  summarise(stem_no = n())

new.gard.id <- tolower(gsub("W","", DENSITY[DENSITY$site == "wanang", ]$garden))

DENSITY[DENSITY$site == "wanang", ]$garden <- new.gard.id

DENSITY_SUM <- DENSITY %>%
  mutate(garden = paste(substr(site,1,1),garden, sep=""))%>%
  group_by(site,garden,treatment)%>%
  summarise(tot.stem.no = sum(stem_no))

# 4. Biomass ----
BIODATA <- getBioData(mainData) # This dataset has an updated woody plant category
BIODATA <- combineTraits(BIODATA, trait.dat) # add traits: life forms

### WOODY ONLY #### ----
# Switch the woody plant data only
# Make a copy of the original biodata dataset
biodata.orig <- BIODATA
unique(BIODATA$life.form)

# Filter woody plants
BIODATA <- BIODATA %>%
  filter(life.form == "woody")

BIOMASS_SUM <- BIODATA %>%
  group_by(site,garden,treatment)%>%
  summarise(tot.bio = sum(tot_bio))

# 5. Richness ----
RICHNESS <- BIODATA %>%
  group_by(site, garden, treatment, plot) %>%
  summarise(sp_no = length(unique(unified_names)),
            simpson = diversity(tot_bio, index = "invsimpson"))
# inv.simpson counts the effective number of species


# Tests
# RICHNESS[c(1,80,130), ] 
# testset <- BIODATA %>%
#   filter((garden == "ng8" & treatment == "ch"))

# 6. Data for ordination ----
source("code/contingencyTable.R")
COMMBIO <- BIODATA %>%
  mutate(plotid = paste(garden, treatment, 
                        sep = "_"))

COMMDATA <- contingencyTable2(COMMBIO,
                              "unified_names", "plotid", "tot_bio")
COMMVARS <- COMMBIO %>%
  group_by(site,garden, treatment, plotid) %>%
  summarise()

COMMVARS <- as.data.frame(COMMVARS)
rownames(COMMVARS) <- COMMVARS$plotid

# 7. Biomass ----

BIOMASS <- BIODATA %>%
  group_by(site,garden,treatment) %>%
  summarise(biomass = sum(tot_bio))

# Test wether the filtering was ok

# These are the names withouth the life form assigned to them
# biodata.orig[which(is.na(biodata.orig$life.form)), "unified_names"]

# biodata.orig %>%
  # filter(garden == "ng3")

# BIODATA %>%
#   filter(garden == "ng3")
