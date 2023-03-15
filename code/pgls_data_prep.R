# 0. Data preparation (move to a separate script) ----
library(dplyr)
library(lme4)
library(lmerTest)
library(MASS)
library(ggplot2)
library(EnvStats)
library(ape)

## 0.1 Read the data with traits ----

### Biomass data
data <- read.csv("data/main.data.w.traits.csv")     
### Type data (woody/non-woody)
trait.dat <- read.csv("data/suppl_plant_traits.csv")
### Phylogenetic tree
myTree <- read.tree('data/phylogeny/V_Phylomaker_tree.txt')

### Convert labels of the phylogenetic tree to lower case
myTree$tip.label <- tolower(myTree$tip.label)

# Randomly resolve polytomies
myTree.rand.poly <- multi2di(myTree)

### Convert biomass data into numerical values (number format from European to US)
data$tot_bio <- gsub(",", ".", data$tot_bio) # substitute commas to dots
data$n.tot_bio <- as.numeric(data$tot_bio) # woody-non woody data

## 0.2 Filter only woody plants and try to clean up the SLA values ----
woody.names <- trait.dat[trait.dat$Life.form == "woody",]$dsName
data.woody <- data %>%
  filter(unified_names %in% woody.names)

### Check the area lost histograms
# ggplot(data.woody, aes(log(sla.d.m2kg1), colour = site))+
#   geom_histogram()

## 0.3 Change inf values to NAs ----
data.woody[is.infinite(data.woody$sla.d.m2kg1), ]$sla.d.m2kg1 <- NA


replaceWithMedian <- function(data.woody, name = "piper_recessum", threshold = 100){
  
  # This function replaces missing values for a given species with
  # a SLA median for that species.
  # It returns data frame with the changes.
  
  # Subset the data
  piper.dat <- data.woody %>% filter(unified_names == name)
  print("step 1")
  
  # Change extremes and NA's into a meadian value
  new.sla.val <- median(piper.dat$sla.d.m2kg1, na.rm = T)
  
  if(length(piper.dat$sla.d.m2kg1) == 1){
    print("single entry - remove")
    data.woody <- data.woody[-(data.woody$unified_names == name), ]
    return(data.woody)
    break
  }
  
  piper.dat.no.na <- piper.dat[!is.na(piper.dat$sla.d.m2kg1),]
  print("step 2")
  
  # Replace extremes with the median
  piper.dat.no.na[piper.dat.no.na$sla.d.m2kg1 > threshold, ]$sla.d.m2kg1 <- new.sla.val
  print("step 3")
  
  # Add these to the main data
  data.woody[data.woody$X %in% piper.dat.no.na$X, 
  ]$sla.d.m2kg1 <- piper.dat.no.na$sla.d.m2kg1
  
  return(data.woody)
  
}

## 0.4 Excluding extreme values ----

### Remove everything above and below 2 sd
sla.data <- data.woody$sla.d.m2kg1[!is.na(data.woody$sla.d.m2kg1)]

sla.parms <- enorm(log(sla.data))
fit.curve <- rnorm(10000, 
                   mean = sla.parms$parameters[1], 
                   sd = sla.parms$parameters[2])

sd.range <- 2
upper.lim <- exp(sla.parms$parameters[1] + sd.range*sla.parms$parameters[2])
lower.lim <- exp(sla.parms$parameters[1] - sd.range*sla.parms$parameters[2])

# Final dataset with only values within a range of [-2, 2] stand deviations.
data.woody.brut <- data.woody %>%
  filter(sla.d.m2kg1 < upper.lim & sla.d.m2kg1 > lower.lim)


## 0.5 More detailed data - clearing ----
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 1. piper aduncum at y g5 i 
data.woody <- replaceWithMedian(data.woody, name = "piper_aduncum")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 2. piper recessum y g9 p
data.woody <- replaceWithMedian(data.woody, name = "piper_recessum")
data.woody[data.woody$X == 4769,]
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 3. trema_orientale at y, g9 p
data.woody <- replaceWithMedian(data.woody, name = "trema_orientale")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 4. tournefortia_sp1 at y g5 c
data.woody <- replaceWithMedian(data.woody, name = "tournefortia_sp1")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 5. numba     g5         p trichospermum_pleiostigma
data.woody <- replaceWithMedian(data.woody, name = "trichospermum_pleiostigma")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 6. yawan     g9         h pipturus_argenteus
data.woody <- replaceWithMedian(data.woody, name = "pipturus_argenteus")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 7. yawan     g1         c debregeasia_longifolia 
data.woody <- replaceWithMedian(data.woody, name = "debregeasia_longifolia")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 8. numba     g5         p solanum_torvum 
data.woody <- replaceWithMedian(data.woody, name = "solanum_torvum")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 9. yawan     g6         i nothocnide_sp1
data.woody <- replaceWithMedian(data.woody, name = "nothocnide_sp1")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 10.yawan     g4         p cypholophus_friesianus
data.woody <- replaceWithMedian(data.woody, name = "cypholophus_friesianus")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 11. yawan     g9         h ficus_pungens
data.woody <- replaceWithMedian(data.woody, name = "ficus_pungens")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 12. yawan     g5         c ficus_dammaropsis
data.woody <- replaceWithMedian(data.woody, name = "ficus_dammaropsis")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 13. numba     g3         f carica_papaya 
data.woody <- replaceWithMedian(data.woody, name = "carica_papaya")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 14 yawan     g6         p  piper_melula
data.woody <- replaceWithMedian(data.woody, name = "piper_melula")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 15. yawan     g2         h saurauia_poolei
data.woody <- replaceWithMedian(data.woody, name = "saurauia_poolei")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 16. yawan     g4         c piper_subulatum
data.woody <- replaceWithMedian(data.woody, name = "piper_subulatum")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 18 numba     g1         f ficus_variegata
data.woody <- replaceWithMedian(data.woody, name = "ficus_variegata")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 19 yawan     g5         c gastonia_spectabilis 
data.woody <- replaceWithMedian(data.woody, name = "gastonia_spectabilis")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 20 numba     g2         p meliosma_pinnata
data.woody <- replaceWithMedian(data.woody, name = "meliosma_pinnata")
data.woody[which(data.woody$sla.d.m2kg1 == max(data.woody$sla.d.m2kg1, na.rm = T)),]
# 21 numba     g5         i oreocnide_rubescens
data.woody <- replaceWithMedian(data.woody, name = "oreocnide_rubescens")
