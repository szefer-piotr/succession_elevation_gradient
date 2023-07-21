# Predictions and visualizations!

predictAndVisualize <- function (model, 
                                 deltaData, 
                                 traitnm = "fsite", 
                                 n=100,
                                 titlestring = "",
                                 ylabstring = "",
                                 xlabstring = ""
) {
  
  other_traits <- names(deltaData)[!(names(deltaData) %in% c(traitnm, 
                                                             "fsite",
                                                             "site",
                                                             "garden", 
                                                             "treatment", 
                                                             "unified_names",
                                                             "pg.cat",
                                                             "comparison"))]
  
  # Create a value gradient for the focal variable.
  # focal.column = seq(from = min(deltaData[, traitnm], na.rm = T),
  #                    to = max(deltaData[, traitnm], na.rm = T), 
  #                    length = n)
  
  if(traitnm != "fsite"){
    focal.column.w = seq(from = min(deltaData[deltaData$site == "wanang", 
                                              traitnm], na.rm = T),
                         to = max(deltaData[deltaData$site == "wanang", 
                                            traitnm], na.rm = T), 
                         length = n)
    focal.column.n = seq(from = min(deltaData[deltaData$site == "numba", traitnm], na.rm = T),
                         to = max(deltaData[deltaData$site == "numba", 
                                            traitnm], na.rm = T), 
                         length = n)
    focal.column.y = seq(from = min(deltaData[deltaData$site == "yawan", traitnm], na.rm = T),
                         to = max(deltaData[deltaData$site == "yawan", 
                                            traitnm], na.rm = T), 
                         length = n)
    
    # Assume average values for the rest.
    other.vals.w <- t(apply(deltaData[deltaData$site == "wanang", 
                                      other_traits], 2, mean, na.rm = T))
    other.vals.n <- t(apply(deltaData[deltaData$site == "numba", 
                                      other_traits], 2, mean, na.rm = T))
    other.vals.y <- t(apply(deltaData[deltaData$site == "yawan", 
                                      other_traits], 2, mean, na.rm = T))
    
    # Assemble the full data frame with sites.
    preddata.w <- cbind(focal.column.w,
                        do.call("rbind", replicate(n, other.vals.w, simplify = FALSE)))
    preddata.n <- cbind(focal.column.n,
                        do.call("rbind", replicate(n, other.vals.n, simplify = FALSE)))
    preddata.y <- cbind(focal.column.y,
                        do.call("rbind", replicate(n, other.vals.y, simplify = FALSE)))
    traitnm -> colnames(preddata.w)[1] -> colnames(preddata.n)[1] -> colnames(preddata.y)[1]
    preddata <- as.data.frame(rbind(preddata.w, preddata.n, preddata.y))
    # preddata$site <- factor(rep(c("wanang", "numba", "yawan"), each = n),
    #                         levels = c("wanang","numba","yawan"))
    preddata$fsite <- factor(c(rep(c("wanang", "numba", "yawan"), each = n)), 
                             levels = c("wanang", "numba", "yawan"))
    
    # Predict values for the newly created data.
    predvals <- predictSE.gls(model, newdata = preddata)
    preddata$predvals <- predvals$fit
    preddata$predse <- predvals$se.fit
    
    varPlot <- ggplot(preddata, aes_string(x = traitnm, y = 'predvals', group = 'fsite')) +
      geom_line(aes(color = fsite), lwd = 1) + 
      geom_ribbon(aes(ymin = predvals - predse,
                      ymax = predvals + predse),
                  alpha = 0.1) +
      geom_point(data = deltaData, 
                 aes_string(x = traitnm, y = 'lrrtb', color = 'fsite'),
                 alpha = 0.5,
                 pch = 16,
                 size = 2) +
      ggtitle(titlestring) + 
      theme_bw() +  
      ylab(ylabstring) + 
      xlab(xlabstring)
    
    
  } else {
    preddata <- deltaData
    predvals <- predictSE.gls(model, newdata = deltaData)
    preddata$predvals <- predvals$fit
    preddata$predse <- predvals$se.fit
    
    varPlot <- ggplot(preddata, aes_string(x = traitnm, y = 'predvals', group = 'fsite')) +
      geom_jitter(data = deltaData, 
                  aes_string(x = traitnm, y = 'lrrtb', color = 'fsite'),
                  alpha = 0.5,
                  pch = 16,
                  size = 2) +
      stat_summary(geom = "pointrange")+
      ggtitle(titlestring) + 
      theme_bw() +  
      ylab(ylabstring) + 
      xlab(xlabstring)
    
  }
  
  return(varPlot)
  
}