# rm(list = ls())
library(dplyr)

# 1. Read main data-set ----
# elev.dat <- read.csv("data/elevation_vegetation_data.csv")
source("code/data_processing.R")

 # 2. Filter data ----

# 2.1 All plants withouth Musa ----
# ed.nomusa <- BIODATA %>%
#   filter(!grepl("musa", 
#                unified_names, 
#                ignore.case = T))

# 2.1 Only woody plants ----
# ed.woody <- BIODATA %>%
#   filter(life.form == "woody")

# 3. For each garden calculate log ratios ----

# Makes a copy of the original dataset
# dataset <- ed.nomusa

# Calcualtions of wood
# dataset <- ed.woody

# dataset$treatment <- as.factor(dataset$treatment)

# gdat <- dataset %>%
#   group_by(site, garden, treatment) %>%
#   summarise(bio = sum(tot_bio))

# Treatments used for the altitudinal comparison
# low herbivory and chlorkill are not included as the former was 
# used only in lowest elevation (Wanang) and the latter in NUmba and Yawan

# alt.comp.treat <- c("p","i","f","h","ch")

# lrdata <- data.frame()

# for (gard in unique(dataset$garden)){
#   
#   print(gard)
#   
#   # Dataset only for control plot
#   based <- gdat %>%
#     filter(garden == gard, treatment == "c")
#   
#   # Dataset for each treatment
#   gdatf <- gdat %>%
#     filter(garden == gard, treatment %in% alt.comp.treat) %>% #
#     mutate(cval = based$bio) %>%
#     
#     # log ratio is Factor(present)/Factor(absent)
#     # Thus in the case of excluding factors (i.e. insecticide, exclosure)
#     # and fungicide) we would divide control value over the treatment value.
#     # In case of herbivore addition we would divide treatment 
#     # (addition of herbivores) over the control.
#     
#     mutate(lratio = ifelse(treatment %in% c("h"), 
#                            log(bio/cval), 
#                            log(cval/bio)))
# 
#   lrdata <- rbind(lrdata, gdatf)
#   
#   }

# Change the order of site factors in the lrdata
# lrdata$site <- factor(lrdata$site, levels = c("wanang", "numba","yawan"))

# 4. Density log ratio extraction ----

# There is only few stems with dbh > 1 in Wanang, whereas in Numba these numbers
# Are really high!!!

# ggplot(DENSITY_SUM, aes(x = site, y = tot.stem.no))+
#   geom_jitter(width = 0.1)+
#   facet_wrap(~treatment)

# To hash-out
# dataset <- RICHNESS
# effect <- "simpson"
# alt.comp.treat = c("p","i","f","h","ch")
# gard <- "ng1"


# getLogRatio function ----
getLogRatio <- function (dataset,
                         effect,
                         alt.comp.treat = c("p","i","f","h","ch")){
  
  lrdata <- data.frame()
  
  dset <- dataset
  
  names(dset)[grepl(effect, names(dset))] <- "value"
  
  for (gard in unique(dset$garden)){
    
    print(gard)
    
    # Dataset only for control plot
    based <- dset %>%
      filter(garden == gard, treatment == "c")
    
    # Dataset for each treatment
    gdatf <- dset %>%
      filter(garden == gard & treatment %in% alt.comp.treat) %>% #
      mutate(cval = as.numeric(based$value)) %>%
      
      # log ratio is Factor(present)/Factor(absent)
      # Thus in the case of excluding factors (i.e. insecticide, exclosure)
      # and fungicide) we would divide control value over the treatment value.
      # In case of herbivore addition we would divide treatment 
      # (addition of herbivores) over the control.
      
      mutate(lratio = ifelse(treatment %in% c("h"), 
                             log(value/cval), 
                             log(cval/value)))
    
    lrdata <- rbind(lrdata, gdatf)
  }
  
  # Change the order of site factors in the lrdata
  lrdata$site <- factor(lrdata$site, levels = c("wanang", "numba","yawan")) 
  
  return(lrdata)
  
}

DENSITY_SUM <- DENSITY %>%
  mutate(garden = paste(substr(site,1,1),garden, sep=""))%>%
  group_by(site,garden,treatment)%>%
  summarise(tot.stem.no = sum(stem_no))

BIOMASS.LR <- getLogRatio(BIOMASS_SUM, 
                           effect = "tot.bio")

DENSITY.LR <- getLogRatio(DENSITY_SUM, 
                          effect = "tot.stem.no")
                          
DIVERSITY.LR <- getLogRatio(RICHNESS, 
                          effect = "simpson")

RICHNESS.LR <- getLogRatio(RICHNESS, 
                            effect = "sp_no")
