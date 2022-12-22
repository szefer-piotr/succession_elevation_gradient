# 1. Read main data-set ----
source("code/prepare_data.R")

library(emmeans)
library(lsmeans)
library(nlme)
library(car)

# library(ggpubr)
# library(multcompView)


BIOMASS.LR <- getLogRatio(BIOMASS, 
                           effect = "tot.bio")

DENSITY.LR <- getLogRatio(DENSITY, 
                          effect = "tot.stem.no")
                          
DIVERSITY.LR <- getLogRatio(RICHNESS, 
                          effect = "simpson")

RICHNESS.LR <- getLogRatio(RICHNESS, 
                            effect = "sp_no")

# 2. Perform statistical set and output labels for the figure

# Biomasss
f.test <- runTest(BIOMASS.LR, treatment_code = "f", "biomass")
i.test <- runTest(BIOMASS.LR, treatment_code = "i", "biomass")
p.test <- runTest(BIOMASS.LR, treatment_code = "p", "biomass")
h.test <- runTest(BIOMASS.LR, treatment_code = "h", "biomass")
biotest_labels <- c(f.test[[1]], i.test[[1]], p.test[[1]], h.test[[1]])
biotest_table <- rbind(f.test[[2]], i.test[[2]], p.test[[2]], h.test[[2]])

# Diversity
f.test <- runTest(DIVERSITY.LR, treatment_code = "f", "diversity")
i.test <- runTest(DIVERSITY.LR, treatment_code = "i", "diversity")
p.test <- runTest(DIVERSITY.LR, treatment_code = "p", "diversity")
h.test <- runTest(DIVERSITY.LR, treatment_code = "h", "diversity")
divtest_labels <- c(f.test[[1]], i.test[[1]], p.test[[1]], h.test[[1]])
divtest_table <- rbind(f.test[[2]], i.test[[2]], p.test[[2]], h.test[[2]])

# Richness
f.test <- runTest(RICHNESS.LR, treatment_code = "f", "richness")
i.test <- runTest(RICHNESS.LR, treatment_code = "i", "richness")
p.test <- runTest(RICHNESS.LR, treatment_code = "p", "richness")
h.test <- runTest(RICHNESS.LR, treatment_code = "h", "richness")
richtest_labels <- c(f.test[[1]], i.test[[1]], p.test[[1]], h.test[[1]])
richtest_table <- rbind(f.test[[2]], i.test[[2]], p.test[[2]], h.test[[2]])

# Density
f.test <- runTest(DENSITY.LR, treatment_code = "f", 'density') 
i.test <- runTest(DENSITY.LR, treatment_code = "i", 'density')
p.test <- runTest(DENSITY.LR, treatment_code = "p", 'density') # and now it is here !!! unequal variances
h.test <- runTest(DENSITY.LR, treatment_code = "h", 'density')
denstest_labels <- c(f.test[[1]], i.test[[1]], p.test[[1]], h.test[[1]])
denstest_table <- rbind(f.test[[2]], i.test[[2]], p.test[[2]], h.test[[2]])

lrr_panel_labs <- c(biotest_labels,
                    divtest_labels,
                    richtest_labels,
                    denstest_labels)

lrr_test_df <- rbind(biotest_table,
                       divtest_table,
                       richtest_table,
                       denstest_table)
