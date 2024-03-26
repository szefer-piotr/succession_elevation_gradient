rm(list = ls())

# Start with Wanang C and I
# source("code/data_processing.R")
source("code/prepare_data.R")
COMMDATA <- t(COMMDATA)

# This script performs RC analysis on the experimental elevation plots.
# For each plot comparison within the same treatment I constrained the
# plant data to species and biomass observed within the SITE (not garden, not only gardern).
# This included all treatments to obtain the most reliable species list for
# random sampling.

# Declare functions ----
### Function to subset the main plant data-set
getPlantMat <- function (COMMDATA, site) {
  # Takes full dataset and creates a subset for a given garden and site
  c1 <- grepl(site, rownames(COMMDATA)) # site constraint
  # c2 <- grepl(garden, rownames(COMMDATA)) # garden constraint
  return(COMMDATA[c1, ])
}


siteCode="wanang_g1_c"

### Assemble function
assemble <- function (siteCode,
                      plant_mat, # sampling community (either within garden/site/treatment)
                      accuracy = 0.001){
  
  # Samples random community with constraints defined by the given site code:
  # number of species and biomass randomized is equal to the observed values.
  
  # Calculate characteristics of that site
  empiricalSpNo <- sum(plant_mat[siteCode, ]>0)
  empiricalBio  <- round(sum(plant_mat[siteCode, ]), 
                         digits = -log(accuracy)/log(10))
  spec_patterns <- colSums(plant_mat > 0)
  bio_patterns <- round(colSums(plant_mat), 
                        digits = -log(accuracy)/log(10))
  bio_patterns[bio_patterns == 0] <- accuracy 
  
  # How to add variability to these probabilities?
  spec_prob <- spec_patterns/sum(spec_patterns)
  
  # Sample given number of species
  S1 <- sample(names(spec_prob),
               empiricalSpNo,
               replace=FALSE,
               prob = spec_prob)
  
  # Probabilities for the biomass conditioned on the species sampled
  bio_prob <- bio_patterns[S1]/sum(bio_patterns[S1])
    
  
  # Initiate community with minimal biomass for the S1 sampled species
  # This assures that all sampled species get to the community because their 
  # probability of increment might be so small that they never get there.
  randCommunity <- setNames(rep(accuracy, length(S1)),
                            S1[order(S1)])

  # I know in advance how many steps needs to be taken to increase bio to the 
  # empirical value
  stepsNo <- (empiricalBio - length(S1)*accuracy)/accuracy
  
  # Assembling the plot. Create a list of species that will experience biomass
  # increments at each of the stepsNo step
  
  incrementLineup <- sample(x = S1, size = stepsNo, 
                            replace = TRUE, 
                            prob = bio_prob)
  
  # print("Done")
  
  incrementCommunity <- table(incrementLineup) * accuracy
  
  finalRandCommunity <- randCommunity + as.numeric(incrementCommunity[names(randCommunity)])
  finalRandCommunity[is.na(finalRandCommunity)] <- accuracy
  
  return(finalRandCommunity)
  
}

### Site pairs generation ----
compareSites <- function (s1, s2, plant_mat, accuracy = 0.001) {
  
  # Function that generates all site code combinations for comparisons.
  # It only comapres sites within garden and within treatment.
  
  ng3cRand <- assemble(s1,
                       plant_mat,
                       accuracy)
  ng2cRand <- assemble(s2,
                       plant_mat,
                       accuracy)
  
  empBC <- vegdist(plant_mat[c(s1, s2), ])
  
  speciesFromRandomizedCommunity <- unique(names(c(ng3cRand, ng2cRand)))
  randomizedCommunityDF <- data.frame(species = speciesFromRandomizedCommunity,
                                     site1 = as.numeric(ng3cRand[speciesFromRandomizedCommunity]),
                                     site2 = as.numeric(ng2cRand[speciesFromRandomizedCommunity]))
  randomizedCommunityDF[is.na(randomizedCommunityDF)] <- 0 
  
  randBC <- vegdist(t(randomizedCommunityDF[,-1]))
  
  return(randBC < empBC)
  
  # return(paste("Empirical value: ", empBC, " vs randomized vale: " , randBC))
  
}

### Data comparison generation fro a given treatment pair
compDatGeneration <- function(treat1, treat2){
  site = "n"
  n.all.pairs <- rbind(data.frame(site1 = paste(site, "g1_", treat1, sep = ""),
                                  site2 = paste(site, "g", 2:9, "_", treat1, sep = ""),
                                  counter = 0,
                                  treatment = treat1,
                                  site = site,
                                  comp.id = 2:9),
                       data.frame(site1 = paste(site, "g1_", treat2, sep = ""),
                                  site2 = paste(site, "g", 2:9, "_", treat2, sep = ""),
                                  counter = 0,
                                  treatment = treat2,
                                  site = site,
                                  comp.id = 2:9))
  site = "y"
  y.all.pairs <- rbind(data.frame(site1 = paste(site, "g1_", treat1, sep = ""),
                                  site2 = paste(site, "g", 2:9, "_", treat1, sep = ""),
                                  counter = 0,
                                  treatment = treat1,
                                  site = site,
                                  comp.id = 2:9),
                       data.frame(site1 = paste(site, "g1_", treat2, sep = ""),
                                  site2 = paste(site, "g", 2:9, "_", treat2, sep = ""),
                                  counter = 0,
                                  treatment = treat2,
                                  site = site,
                                  comp.id = 2:9))
  
  site = "w"
  w.all.pairs <- rbind(data.frame(site1 = paste(site, "g1_", treat1, sep = ""),
                                  site2 = paste(site, "g", 2:6, "_", treat1, sep = ""),
                                  counter = 0,
                                  treatment = treat1,
                                  site = site,
                                  comp.id = 2:6),
                       data.frame(site1 = paste(site, "g1_", treat2, sep = ""),
                                  site2 = paste(site, "g", 2:6, "_", treat2, sep = ""),
                                  counter = 0,
                                  treatment = treat2,
                                  site = site,
                                  comp.id = 2:6))
  all.pairs <- rbind(w.all.pairs, 
                     n.all.pairs,
                     y.all.pairs) %>%
    mutate(comp = paste(toupper(treat1), "vs", toupper(treat2)))
  return(all.pairs)
}

# Randomization ----

### 1. All treatment pairs for all sites ----
ci.pairs <- compDatGeneration("c","i")
ch.pairs <- compDatGeneration("c","h")
cp.pairs <- compDatGeneration("c","p")
cf.pairs <- compDatGeneration("c","f")
all.pairs <- rbind(ci.pairs,ch.pairs,cp.pairs,cf.pairs) 

### 2. Set up randomization parameters ----
iterations = 9999
accuracy = 0.001

### 3. Run ----

# Progress BAR
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = iterations*dim(all.pairs)[1], # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")
row = 1
for(row in 1:dim(all.pairs)[1]){
  
  site1 <- all.pairs[row, 1]
  site2 <- all.pairs[row, 2]
  
  # Get plant_mat data for a given site
  plant_mat <- getPlantMat(COMMDATA, substr(site1, 1,1))
  
  # Initiate a counter that tracks the number of times when the BC of randomized
  # community was smaller or equal to the observed BC
  counter <- 0
  
  for (iter in 1:iterations){
    
    setTxtProgressBar(pb, ((row - 1)*iterations) + iter)
    
    if(compareSites(site1,
                    site2, 
                    plant_mat = plant_mat, 
                    accuracy = accuracy)){
      counter <- counter + 1
    }
    
  }
  
  all.pairs[row, ]$counter <- counter
  
}

# Calculate the RC value ----
# 0.1 biomass accuracy, 9999 iterations
all.pairs_max_accuracy <- all.pairs %>%
  mutate(RC = (counter/iterations - 0.5)*2,
         fsite = factor(as.factor(site), levels = c("w","n","y")))

# write.csv(all.pairs_min_accuracy, "data/raup_crick/all.pairs_min_accuracy.csv")

# write.csv(all.pairs_med_accuracy, "data/raup_crick/all.pairs_med_accuracy.csv")

# write.csv(all.pairs_max_accuracy, "data/raup_crick/all.pairs_max_accuracy.csv")

all.pairs_max_accuracy <- read.csv("data/raup_crick/all.pairs_max_accuracy.csv")
all.pairs_max_accuracy$fsite <- factor(all.pairs_max_accuracy$fsite, 
                                       levels = c("w","n","y"))
# Plot results ----

# ggplot(all.pairs_min_accuracy, aes(x = treatment, y = RC))+
#   geom_jitter(width = 0.05, size = 2.5)+
#   ylim(c(-1,1))+
#   # stat_summary(fun.y = mean, na.rm = T,
#   #              geom = "point", size = 4)+
#   # stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = comp.id),
#                geom = "line", lty = 3)+
#   theme_bw()+
#   ggtitle(paste("Accuracy = 0.1"))+
#   geom_hline(yintercept = 0, lty = 2)+
#   facet_grid(vars(fsite), vars(comp), scales = "free")

# ggplot(all.pairs_med_accuracy, aes(x = treatment, y = RC))+
#   geom_jitter(width = 0.05, size = 2.5)+
#   ylim(c(-1,1))+
#   # stat_summary(fun.y = mean, na.rm = T,
#   #              geom = "point", size = 4)+
#   # stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = comp.id),
#                geom = "line", lty = 3)+
#   theme_bw()+
#   ggtitle(paste("Accuracy = 0.01"))+
#   geom_hline(yintercept = 0, lty = 2)+
#   facet_grid(vars(fsite), vars(comp), scales = "free")

# ggplot(all.pairs_max_accuracy, aes(x = treatment, y = RC))+
#   geom_jitter(width = 0.05, size = 2.5)+
#   ylim(c(-1.25,1.25))+
#   # stat_summary(fun.y = mean, na.rm = T,
#   #              geom = "point", size = 4)+
#   # stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = comp.id),
#                geom = "line", lty = 3)+
#   theme_bw()+
#   ggtitle(paste("Accuracy = 0.001"))+
#   geom_hline(yintercept = 0, lty = 2)+
#   geom_hline(yintercept = 1, lty = 1)+
#   geom_hline(yintercept = -1, lty = 1)+
#   facet_grid(vars(fsite), vars(comp), scales = "free")

# Statistical tests ----
test.df <- data.frame()
for (st in unique(all.pairs_max_accuracy$fsite)){
  for (cp in unique(all.pairs_max_accuracy$comp)){
    
    print(paste(st, cp))
    
    subdat <- all.pairs_max_accuracy %>%
      filter(fsite == st & comp == cp)
    
    mod <- nlme::lme(RC ~ treatment,
                     random = ~1|comp.id,
                     data = subdat)

    # print(t.test(RC ~ treatment, data = subdat, paired = TRUE))
    print(summary(mod))
    
    test.df <- rbind(test.df, data.frame(fsite = st,
                                         comp = cp,
                                         group1 = tolower(substr(cp,1,1)),
                                         group2 = tolower(substr(cp,6,6)),
                                         test.res = summary(mod)$tTable[2,5]))
    
    print(summary(mod))
    
  }
}

test.df <- test.df %>%
  mutate(label = ifelse(test.res < 0.05, "*"," "))

# library(ggsignif)

ggplot(all.pairs_max_accuracy, aes(x = treatment, y = RC))+
  # geom_boxplot()+
  ylim(c(-1.25,1.5))+
  stat_summary(fun.y = mean, na.rm = T,
               geom = "point", size = 4)+
  stat_summary(fun.data = "mean_cl_boot")+
  theme_bw()+
  ggtitle(paste("Accuracy = 0.001"))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_hline(yintercept = 1, lty = 3)+
  geom_hline(yintercept = -1, lty = 3)+
  facet_grid(fsite ~ comp, scales = "free")+
  ggpubr::stat_pvalue_manual(data = test.df[test.df$label == "*",], label = "label",
                     y.position = 1.25, 
                     label.size = 6)




ggplot(all.pairs_max_accuracy, aes(x = treatment, y = RC))+
  # geom_boxplot()+
  ylim(c(-1.25,1.5))+
  stat_summary(fun.y = mean, na.rm = T,
               geom = "point", size = 4)+
  stat_summary(fun.data = "mean_cl_boot")+
  theme_bw()+
  ggtitle(paste("Accuracy = 0.001"))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_hline(yintercept = 1, lty = 3)+
  geom_hline(yintercept = -1, lty = 3)+
  facet_grid(fsite ~ comp, scales = "free")+
  ggpubr::stat_pvalue_manual(data = test.df[test.df$label == "*",], label = "label",
                             y.position = 1.25, 
                             label.size = 6)
