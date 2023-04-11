#### ------------------------------------------------------
# 1. Subset a data-set to only control and treatment plots
subDat <- dwbRichF %>%
  filter(treatment %in% subsetVars)

lgsData <- data.frame()

for (st in unique(subDat$site)){
  for (gard in unique(subDat$garden)){
    
    stGardDat <- subDat %>%
      filter(site == st,
             garden == gard)
    
    # Some quality control
    if(dim(stGardDat)[1] == 0){
      next
    }
    if(length(unique(stGardDat$treatment)) < 2){
      next
    }
    
    specInCtr <- unique(stGardDat[stGardDat$treatment == subsetVars[1], ]$unified_names)
    specInTrt <- unique(stGardDat[stGardDat$treatment == subsetVars[2], ]$unified_names)
    
    spLost <- specInCtr[!(specInCtr %in% specInTrt)]
    spGained <- specInTrt[!(specInTrt %in% specInCtr)]
    
    stGardDat$pg.cat <- ifelse(stGardDat$unified_names %in% spLost, "lost",
                               ifelse(stGardDat$unified_names %in% spGained, "gained", "stayed"))
    
    
    lgsData <- rbind(lgsData, stGardDat)
    
    
  }
}

### --------------------------------------------------------
# 2. Get the delta data ready for th e given subset vars (comparison)

# Create data-set for a given treatment
deltaData <- data.frame()

# Change total bio variable into numeric
lgsData$tot_bio <- as.numeric(lgsData$tot_bio)

# Loop through each species in each garden and site and calculate their deltaBio
for (st in unique(lgsData$site)){
  
  for (gard in unique(lgsData$garden)){
    
    stGardDat <- lgsData %>%
      filter(site == st,
             garden == gard)
    
    
    if (dim(stGardDat)[1] == 0){
      # If subset is empty skip to another one
      next
    }
    
    if (length(unique(stGardDat$treatment)) < 2){
      # If there is species in only one treatment skip to another one
      next
    }
    
    specInCtr <- unique(stGardDat[stGardDat$treatment == subsetVars[1], ]$unified_names)
    specInTrt <- unique(stGardDat[stGardDat$treatment == subsetVars[2], ]$unified_names)
    specInBoth <- specInCtr[specInCtr %in% specInTrt]
    
    cDat <- stGardDat %>%
      filter(unified_names %in% specInBoth,
             treatment == subsetVars[1]) %>%
      arrange(unified_names)
    tDat <- stGardDat %>%
      filter(unified_names %in% specInBoth,
             treatment == subsetVars[2]) %>%
      arrange(unified_names)
    
    # Calculate LRR for biomass and SLA change
    
    cDat$comparison <- subsetVars[2]
    
    if (subsetVars[2] == "h"){
      cDat$lrrtb <- log(tDat$tot_bio/cDat$tot_bio)
      cDat$lrrsla <- log(tDat$sla.d.m2kg1/cDat$sla.d.m2kg1)
    } else{
      cDat$lrrtb <- log(cDat$tot_bio/tDat$tot_bio)
      cDat$lrrsla <- log(cDat$sla.d.m2kg1/tDat$sla.d.m2kg1)
    }
    
    deltaData <- rbind(deltaData, cDat)
    
  }
}

####--------------------------------------------------
# 3. Log transform estimated leaf area
deltaData <- deltaData %>%
  mutate(lest.leaf.area = log(est.leaf.area),
         fsite = factor(site, 
                        levels = c("wanang", "numba","yawan")),
         perc.herb = c(scale(perc.herb)))%>%
  filter(!is.infinite(lest.leaf.area))

# DeltaBiomass vs SLA at each site
slaPlot <- ggplot(deltaData, aes(x = lsla.d, y = lrrtb, color = site)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

# There might be some effect of site.
# DeltaBiomass vs SLA at each site
wcPlot <- ggplot(deltaData, aes(x = water.cont, y = lrrtb, color = site)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))
# Seems like the effect is not very strong either

herbPlot <- ggplot(deltaData, aes(x = perc.herb, y = lrrtb, color = site)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  scale_x_continuous(trans = 'log2')
