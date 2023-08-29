# Calculate position of species and site along a given treatment vector.

traitPredsCommComp <- function (data = data.woody.brut,
                                ordination = ord.list,
                                phylogeny = myTree.rand.poly,
                                treatment = "f", st = "wanang") {
  
  surfData <- data %>%
    filter(site == st) %>%
    group_by(unified_names) %>%
    summarise(lsla.d = log(mean(sla.d, na.rm=T)),
              water.cont = mean(water.cont, na.rm = T),
              herbivory = mean(perc.herb, na.rm = T))
  
  # Add row names to sla data
  surfDataDf <- as.data.frame(surfData)
  rownames(surfDataDf) <- surfDataDf$unified_names
  
  # Get the treatment vector.
  trtVcts <- ordination[[substr(st, 1, 1)]]$CCA$biplot[treatment, c(1,2)]
  
  # Get species vector.
  all.specs <- ordination[[substr(st, 1, 1)]]$CCA$v[, c(1,2)]
  usednms <- surfDataDf$unified_names[surfDataDf$unified_names %in% rownames(all.specs)]
  specVcts <- all.specs[usednms,]
  
  # Projection of species onto treatment vector.
  VdotU <- ((specVcts %*% trtVcts))
  uNormSq <- (trtVcts %*% trtVcts)
  projUV <- (VdotU/as.numeric(uNormSq)) %*% t(as.matrix(trtVcts))
  
  # Get norm for each projection vector.
  positions <- apply(projUV, 1, function(x) sqrt(x %*% x))
  
  # Add positions to the data frame.
  modDat <- data.frame(value = c(surfDataDf[usednms, ]$lsla.d,
                                 surfDataDf[usednms, ]$water.cont,
                                 surfDataDf[usednms, ]$herbivory),
                       label = rep(c("SLA", "Water Content", "Herbivory"), 
                                   each = length(positions)),
                       pos = positions[usednms])
  
  testDat <- data.frame(sla = surfDataDf[usednms, ]$lsla.d,
                        water = surfDataDf[usednms, ]$water.cont,
                        herb = surfDataDf[usednms, ]$herbivory,
                        pos = positions[usednms],
                        unified_names = usednms)
  
  
  
  # print(ggplot(modDat, aes(x = pos, y = value)) +
  #   geom_point()+
  #   stat_smooth(method = "lm") + 
  #   facet_wrap(~label, scales = 'free'))
  
  # Phylogeny ready
  testDatTree <- testDat %>%
    filter(unified_names %in% phylogeny$tip.label)
  
  # Tests + phylogeny!!!!
  glsmod1 <- gls(pos ~ sla + water + herb,
                 data = testDatTree,
                 method = "ML",
                 correlation = corMartins(2.718282,
                                          phy = phylogeny,
                                          form = ~unified_names,
                                          fixed = T),
                 na.action = na.omit)
  
  # glsmod2 <- gls(pos ~ sla +       + herb, 
  #                data = testDatTree,
  #                method = "ML",
  #                correlation = corMartins(2.718282,
  #                      phy = myTree.rand.poly,
  #                      form = ~unified_names,
  #                      fixed = T),
  #                na.action = na.omit)
  # 
  # glsmod3 <- gls(pos ~ sla + water       ,
  #                data = testDatTree,
  #                method = "ML",
  #                correlation = corMartins(2.718282,
  #                      phy = myTree.rand.poly,
  #                      form = ~unified_names,
  #                      fixed = T),
  #                na.action = na.omit)
  # 
  # glsmod4 <- gls(pos ~               herb,
  #                data = testDatTree,
  #                method = "ML",
  #                correlation = corMartins(2.718282,
  #                      phy = myTree.rand.poly,
  #                      form = ~unified_names,
  #                      fixed = T),
  #                na.action = na.omit)
  # 
  # glsmod5 <- gls(pos ~       water       ,
  #                data = testDatTree,
  #                method = "ML",
  #                correlation = corMartins(2.718282,
  #                      phy = myTree.rand.poly,
  #                      form = ~unified_names,
  #                      fixed = T),
  #                na.action = na.omit)
  # glsmod6 <- gls(pos ~ sla               ,
  #                data = testDatTree,
  #                method = "ML",
  #                correlation = corMartins(2.718282,
  #                      phy = myTree.rand.poly,
  #                      form = ~unified_names,
  #                      fixed = T),
  #                na.action = na.omit)
  
  # Summary 
  
  return(list(model = glsmod1, 
              title = toupper(paste(st, treatment))))
  
  # Prediction of the full model
  # effx1 <- effect("sla", glsmod1, partial.residuals=T)
  # effx2 <- effect("water", glsmod1, partial.residuals=T)
  # effx3 <- effect("herb", glsmod1, partial.residuals=T)
  
  # plot(effx1)
  # plot(effx2)
  # plot(effx3)
  
}