# Define functions specific for the data that is used for this study

contingencyTable2 <- function(dataset, ROW, COL, VALUE, rm.null=TRUE){
  
  # Takes a data frame and creates a matrix with specified
  # row labels and column labels populated with a sum of specified VALUE
  # Used to summarise 
  
  # Get rid of the empty factors
  dataset[, colnames(dataset) == ROW] <- as.character(dataset[, colnames(dataset) == ROW])
  dataset[, colnames(dataset) == COL] <- as.character(dataset[, colnames(dataset) == COL])
  # Make a table, get rid of the empty rows and columns
  plants <- table(dataset[, colnames(dataset) == ROW], dataset[, colnames(dataset) == COL])
  if(rm.null){
    plants <- plants[rowSums(plants) != 0, colSums(plants) != 0]
  }
  
  # See where to insert values
  allSpecCodes <- colnames(plants)
  allPlotCodes <- rownames(plants)
  entries <- which(plants != 0, arr.ind = TRUE)
  # Loop through the entries and insert values
  for (entry in 1:dim(entries)[1]){
    plot <- entries[entry,1]
    plant <- entries[entry,2]
    specCode <- allSpecCodes[plant]
    plotCode <- allPlotCodes[plot]
    res <- dataset[dataset[,ROW] == plotCode & dataset[,COL] == specCode,VALUE]
    plants[plot,plant] <- sum(res, na.rm = TRUE)
  }
  plants[is.na(plants)] <- 0
  
  # Change the table to a matrix or data.frame
  mat_a <- matrix(0, nrow = dim(plants)[1], ncol = dim(plants)[2])
  colnames(mat_a) <- colnames(plants)
  rownames(mat_a) <- rownames(plants)
  for (row in 1:dim(plants)[1]){
    for (col in 1:dim(plants)[2])
      mat_a[row,col] <- plants[row,col]
  }
  
  #return(plants)
  return(mat_a)
}

getLogRatio <- function (dataset,
                         effect,
                         alt.comp.treat = c("p","i","f","h")){
  
  # What this function does?...
  
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

runTest <- function(lrdata, treatment_code = "f"){
  # Specific wrapper around testing procedure
  
  # Filter data
  trtdata  <- lrdata %>%
    filter(treatment == treatment_code)
  
  ltest.p <- leveneTest(lratio ~ site, data =  trtdata)
  btest.p <- bartlett.test(lratio ~ site, data =  trtdata)
  print(ltest.p)
  print(btest.p)
  
  # If the varianvces are unequal then perform GLS
  if(ltest.p$`Pr(>F)`[1] < 0.05){
    
    trtlm <- gls(lratio ~ 0 + site, data = trtdata,
                 weights = varIdent(form = ~1|site))
    summary(trtlm)
    
    print("Model runned")
    
    # Tukey post-hoc
    emm <- emmeans::emmeans(object = trtlm, 
                            specs = "site", 
                            adjust = "wald")
    
    print("Post hoc runned")
    
    zero.dif <- ifelse(summary(trtlm)$tTable[,4] > 0.05, "","*")
    print("zero.dif assigned")
    
    mod_means <- multcomp::cld(object = emm,
                               Letters = letters)$.group
    
    paste(mod_means,zero.dif, sep = "") -> res
    
  }else{
    trtlm <- lm(lratio ~ 0 + site, data = trtdata)
    
    # Tukey post-hoc
    emm <- emmeans(object = trtlm, 
                   specs = "site", 
                   adjust = "wald")
    
    zero.dif <- ifelse(summary(trtlm)$coefficients[,4] > 0.05, "","*")
    mod_means <- multcomp::cld(object = emm,
                               Letters = letters)$.group
    
    paste(mod_means,zero.dif, sep = "") -> res
  }
  
  # If all letter are the same then return empty vector
  if(length(unique(res)) == 1){
    res <- vector("character", length = 3)
  } 
  
  return(res)
  
}
