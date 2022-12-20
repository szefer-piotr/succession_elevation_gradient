# 1. Read main data-set ----
source("code/prepare_data.R")

BIOMASS.LR <- getLogRatio(BIOMASS, 
                           effect = "tot.bio")

DENSITY.LR <- getLogRatio(DENSITY, 
                          effect = "tot.stem.no")
                          
DIVERSITY.LR <- getLogRatio(RICHNESS, 
                          effect = "simpson")

RICHNESS.LR <- getLogRatio(RICHNESS, 
                            effect = "sp_no")

# 2. Perform statistical set and output labels for the figure