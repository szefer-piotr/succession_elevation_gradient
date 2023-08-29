library(caTools)
library(C50)
library(randomForest)

# Add richness
richdat <- data %>%
  group_by(site, garden, treatment) %>%
  summarise(richness = n()) %>%
  mutate(sgt = paste(site,garden,treatment, sep = "_"))

data.woody.brut <- data.woody.brut %>%
  mutate(sgt = paste(site,garden,treatment, sep = "_"))

dwbRich <- merge(data.woody.brut, richdat, by = "sgt")

# Filter and add estimated leaf area
dwbRichF <- dwbRich %>%
  mutate(est.leaf.area = num_leaf_weight * sla.w.m2kg1,
         lsla.d = log(sla.d.m2kg1),
         ltb = log(as.numeric(tot_bio)),
         labu = log(as.numeric(no_stems+1))) %>%
  dplyr::select(site.x, garden.x, treatment.x, 
                unified_names, tot_bio, no_stems, 
                perc.herb, water.cont, est.leaf.area, sla.d.m2kg1,
                richness, ltb, lsla.d, labu) %>%
  filter(water.cont > 0)

names(dwbRichF) <- c("site", "garden", "treatment", "unified_names","tot_bio",
                     "no_stems", "perc.herb", "water.cont", "est.leaf.area",
                     "sla.d.m2kg1", "richness", "ltb","lsla.d","labu")


# Random Forest
# Loading package


rfAnalysis <- function(lgsData) {
  
  # rfData <- lgsData %>%
  #   mutate(pg.cat = as.factor(pg.cat),
  #          lest.leaf.area = log(est.leaf.area),
  #          lstem = log(no_stems)) %>%
  #   dplyr::select(pg.cat, lstem, perc.herb, water.cont,
  #                 lest.leaf.area, richness, ltb, lsla.d, treatment)
  
  rfData <- lgsData %>%
    mutate(pg.cat = as.factor(pg.cat),
           lest.leaf.area = log(est.leaf.area),
           lstem = log(no_stems)) %>%
    dplyr::select(pg.cat, site, lstem, perc.herb, water.cont,
                  lest.leaf.area, richness, ltb, lsla.d, treatment)
  
  # Try standardized, non-correlated variables
  # rfDataRF <- rfData %>%
  #   filter(!is.infinite(lest.leaf.area)) %>%  
  #   dplyr::select(pg.cat, perc.herb, water.cont, richness, ltb, lsla.d)
  # rfDataRF[,-1] <- scale(rfDataRF[,-1])
 
  cont_preds <- c("perc.herb", "water.cont", "richness", "ltb", "lsla.d")
  
  rfDataRF <- rfData %>%
    filter(!is.infinite(lest.leaf.area)) %>%  
    dplyr::select(pg.cat, site, perc.herb, water.cont, richness, ltb, lsla.d)
  rfDataRF[,cont_preds] <- scale(rfDataRF[,cont_preds])
  
  
  # Splitting data in train and test data
  split <- caTools::sample.split(rfDataRF, SplitRatio = 0.7)
  train <- subset(rfDataRF, split == "TRUE")
  test <- subset(rfDataRF, split == "FALSE")
  
  classifier_RF = randomForest(pg.cat~ ., data = train,
                               ntree = 500, proximity= TRUE)
  
  # Predicting the Test set results
  y_pred = predict(classifier_RF, newdata = test[-1])
  
  # Confusion Matrix
  confusion_mtx = table(test[, 1], y_pred)
  
  # Regression C50
  C50Fit <- C5.0(pg.cat ~., data=train,
                 control = C5.0Control(minCases = 5))
  C50pred <- predict(object=C50Fit, 
                     newdata=test, 
                     type="class")
  
  return(list(rf_confusion_matrix = confusion_mtx,
              classifier_for_varImpPlot = classifier_RF,
              reg_tree_table = table(C50pred, test$pg.cat),
              reg_fit_for_plot = C50Fit,
              train_data = train,
              validation_data = test))
  
  
}


# Run all random forests
subsetVars <- c("c", "p")
source('code/get_data_ready_for_the_sla_analyses.R')
prfres <- rfAnalysis(lgsData)

subsetVars <- c("c", "i")
source('code/get_data_ready_for_the_sla_analyses.R')
irfres <- rfAnalysis(lgsData)

subsetVars <- c("c", "f")
source('code/get_data_ready_for_the_sla_analyses.R')
frfres <- rfAnalysis(lgsData)

subsetVars <- c("c", "h")
source('code/get_data_ready_for_the_sla_analyses.R')
hrfres <- rfAnalysis(lgsData)

# plot(getTree(prfres$classifier_for_varImpPlot, 3, labelVar=TRUE))
