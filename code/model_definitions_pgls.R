moddata <- deltaData %>% filter(unified_names %in% myTree.rand.poly$tip.label)

modelDefinitions <-  function () {
  
  # Runs all of model definitions
  
  # NO TRAITS
  no.traits <- gls(
    lrrtb ~ richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  # FULL MODEL
  full.model <- gls(
    lrrtb ~ lsla.d*fsite+ I(lsla.d^2)*fsite+water.cont*fsite+I(water.cont^2)*fsite+perc.herb*fsite+I(perc.herb^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  # FULL MODEL
  full.model.noint <- gls(
    lrrtb ~ lsla.d + I(lsla.d^2) + water.cont + I(water.cont^2) + perc.herb + I(perc.herb^2) + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  all.linear <- gls(
    lrrtb ~ lsla.d*fsite + water.cont*fsite + perc.herb*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  all.linear.noint <- gls(
    lrrtb ~ lsla.d+water.cont+perc.herb+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  # Individual traits-------------------------------------------------
  
  ## SLA
  just.sla.linear.noint <- gls(
    lrrtb ~ lsla.d + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.sla.linear <- gls(
    lrrtb ~ lsla.d*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.sla.quadratic.noint <- gls(
    lrrtb ~ lsla.d + I(lsla.d^2) + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.sla.quadratic <- gls(
    lrrtb ~ lsla.d*fsite + I(lsla.d^2)*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  ## WATER
  just.water.linear.noint <- gls(
    lrrtb ~ water.cont + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.water.linear <- gls(
    lrrtb ~ water.cont*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.water.quadratic.noint <- gls(
    lrrtb ~ water.cont + I(water.cont^2) + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.water.quadratic <- gls(
    lrrtb ~ water.cont*fsite + I(water.cont^2)*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  ## HERBIVORY
  just.herb.linear.noint <- gls(
    lrrtb ~ perc.herb + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.herb.linear <- gls(
    lrrtb ~ perc.herb*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.herb.quadratic.noint <- gls(
    lrrtb ~ perc.herb + I(perc.herb^2) + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  just.herb.quadratic <- gls(
    lrrtb ~ perc.herb*fsite + I(perc.herb^2)*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  
  
  # COMBINATIONS -----------------------------------------------------------------
  # Interaction
  # quadratic SLA, HERB -------------------
  q.sla.q.herb <- gls(
    lrrtb ~ lsla.d*fsite+I(lsla.d^2)*fsite+perc.herb*fsite+I(perc.herb^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  # quadratic SLA, linear HERB
  q.sla.l.herb <- gls(
    lrrtb ~ lsla.d*fsite+ I(lsla.d^2)*fsite+perc.herb*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  # linear SLA, quadratic HERB
  l.sla.q.herb <- gls(
    lrrtb ~ lsla.d*fsite+perc.herb*fsite+I(perc.herb^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",
    correlation = corMartins(2.718282,
                             phy = myTree.rand.poly,
                             form = ~unified_names,
                             fixed = T),
    na.action = na.omit
  )
  # linear SLA, linear HERB
  l.sla.l.herb <- gls(
    lrrtb ~ lsla.d*fsite+perc.herb*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                             
                                           phy = myTree.rand.poly,                              
                                           form = ~unified_names,                              
                                           fixed = T),     
    na.action = na.omit
  )
  
  # No interaction
  # quadratic SLA, HERB 
  q.sla.q.herb.noint <- gls(
    lrrtb ~ lsla.d+I(lsla.d^2)+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                              
                                           phy = myTree.rand.poly,
                                           form = ~unified_names,   
                                           fixed = T),     
    na.action = na.omit
  )
  # quadratic SLA, linear HERB
  q.sla.l.herb.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+perc.herb+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                              
                                           phy = myTree.rand.poly,  
                                           form = ~unified_names,   
                                           fixed = T), 
    na.action = na.omit
  )
  # linear SLA, quadratic HERB
  l.sla.q.herb.noint <- gls(
    lrrtb ~ lsla.d+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,               
                                           phy = myTree.rand.poly, 
                                           form = ~unified_names,  
                                           fixed = T),   
    na.action = na.omit
  )
  # linear SLA, linear HERB
  l.sla.l.herb.noint <- gls(
    lrrtb ~ lsla.d+perc.herb+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                      
                                           phy = myTree.rand.poly,   
                                           form = ~unified_names,   
                                           fixed = T),    
    na.action = na.omit
  )
  
  # Interaction
  # quadratic HERB, WATER----------------------
  q.herb.q.water <- gls(
    lrrtb ~ water.cont*fsite+I(water.cont^2)*fsite+perc.herb*fsite+I(perc.herb^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,           
                                           phy = myTree.rand.poly,    
                                           form = ~unified_names,   
                                           fixed = T),   
    na.action = na.omit
  )
  # quadratic HERB, linear WATER
  q.herb.l.water <- gls(
    lrrtb ~ water.cont*fsite+perc.herb*fsite+I(perc.herb^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                    
                                           phy = myTree.rand.poly,      
                                           form = ~unified_names,     
                                           fixed = T),    
    na.action = na.omit
  )
  # linear HERB, quadratic WATER
  l.herb.q.water <- gls(
    lrrtb ~ water.cont*fsite+I(water.cont^2)*fsite+perc.herb*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                    
                                           phy = myTree.rand.poly,      
                                           form = ~unified_names,     
                                           fixed = T),   
    na.action = na.omit
  )
  # linear HERB, linear WATER
  l.herb.l.water <- gls(
    lrrtb ~ water.cont*fsite+perc.herb*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                    
                                           phy = myTree.rand.poly,      
                                           form = ~unified_names,      
                                           fixed = T),   
    na.action = na.omit
  )
  
  # NO Interaction
  # quadratic HERB, WATER
  q.herb.q.water.noint <- gls(
    lrrtb ~ water.cont+I(water.cont^2)+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,             
                                           phy = myTree.rand.poly,  
                                           form = ~unified_names,  
                                           fixed = T),   
    na.action = na.omit
  )
  # quadratic HERB, linear WATER
  q.herb.l.water.noint <- gls(
    lrrtb ~ water.cont+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,               
                                           phy = myTree.rand.poly, 
                                           form = ~unified_names,  
                                           fixed = T),    
    na.action = na.omit
  )
  # linear HERB, quadratic WATER
  l.herb.q.water.noint <- gls(
    lrrtb ~ water.cont+I(water.cont^2)+perc.herb+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,            
                                           phy = myTree.rand.poly,  
                                           form = ~unified_names, 
                                           fixed = T),  
    na.action = na.omit
  )
  # linear HERB, linear WATER
  l.herb.l.water.noint <- gls(
    lrrtb ~ water.cont+perc.herb+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                  
                                           phy = myTree.rand.poly,   
                                           form = ~unified_names,    
                                           fixed = T),   
    na.action = na.omit
  )
  
  
  
  # quadratic SLA, WATER-----------------------------
  # Interaction
  q.sla.q.water <- gls(
    lrrtb ~ lsla.d*fsite+ I(lsla.d^2)*fsite+water.cont*fsite+I(water.cont^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,              
                                           phy = myTree.rand.poly, 
                                           form = ~unified_names,  
                                           fixed = T),    
    na.action = na.omit
  )
  # quadratic SLA, linear WATER
  q.sla.l.water <- gls(
    lrrtb ~ lsla.d*fsite+ I(lsla.d^2)*fsite+water.cont*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                    
                                           phy = myTree.rand.poly,    
                                           form = ~unified_names,   
                                           fixed = T),  
    na.action = na.omit
  )
  # linear SLA, quadratic WATER
  l.sla.q.water <- gls(
    lrrtb ~ lsla.d*fsite+water.cont*fsite+I(water.cont^2)*fsite+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                 
                                           phy = myTree.rand.poly,
                                           form = ~unified_names, 
                                           fixed = T),   
    na.action = na.omit
  )
  # linear SLA, linear WATER
  l.sla.l.water <- gls(
    lrrtb ~ lsla.d*fsite + water.cont*fsite + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                    
                                           phy = myTree.rand.poly,  
                                           form = ~unified_names,    
                                           fixed = T),   
    na.action = na.omit
  )
  
  # NO Interaction
  q.sla.q.water.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+water.cont+I(water.cont^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                   
                                           phy = myTree.rand.poly,     
                                           form = ~unified_names,      
                                           fixed = T),   
    na.action = na.omit
  )
  # quadratic SLA, linear WATER
  q.sla.l.water.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+water.cont+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                     
                                           phy = myTree.rand.poly,  
                                           form = ~unified_names,   
                                           fixed = T),    
    na.action = na.omit
  )
  # linear SLA, quadratic WATER
  l.sla.q.water.noint <- gls(
    lrrtb ~ lsla.d+water.cont+I(water.cont^2)+richness+labu+lrrsla+fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,              
                                           phy = myTree.rand.poly, 
                                           form = ~unified_names, 
                                           fixed = T),  
    na.action = na.omit
  )
  # linear SLA, linear WATER
  l.sla.l.water.noint <- gls(
    lrrtb ~ lsla.d + water.cont + richness + labu + lrrsla + fsite,
    data = moddata,
    method = "ML",correlation = corMartins(2.718282,                  
                                           phy = myTree.rand.poly, 
                                           form = ~unified_names,  
                                           fixed = T),   
    na.action = na.omit
  )
  
  
  return(list(no.traits = no.traits,
              
              # Full vs null
              full.model = full.model,
              full.model.noint = full.model.noint,
              all.linear = all.linear,
              all.linear.noint = all.linear.noint,
              
              # Individual traits
              just.sla.linear.noint = just.sla.linear.noint,
              just.sla.linear = just.sla.linear,
              just.sla.quadratic = just.sla.quadratic,
              just.sla.quadratic.noint = just.sla.quadratic.noint,
              
              just.water.linear.noint = just.water.linear.noint,
              just.water.linear = just.water.linear,
              just.water.quadratic = just.water.quadratic,
              just.water.quadratic.noint = just.water.quadratic.noint,
              
              just.herb.linear.noint =just.herb.linear.noint,
              just.herb.linear = just.herb.linear,
              just.herb.quadratic = just.herb.quadratic,
              just.herb.quadratic.noint = just.herb.quadratic.noint,
              
              # Combinations
              q.sla.q.herb = q.sla.q.herb,
              q.sla.l.herb = q.sla.l.herb,
              l.sla.q.herb = l.sla.q.herb,
              l.sla.l.herb = l.sla.l.herb,
              q.sla.q.herb.noint = q.sla.q.herb.noint,
              q.sla.l.herb.noint = q.sla.l.herb.noint,
              l.sla.q.herb.noint = l.sla.q.herb.noint,
              l.sla.l.herb.noint = l.sla.l.herb.noint,
              
              q.sla.q.water = q.sla.q.water,
              q.sla.l.water = q.sla.l.water,
              l.sla.q.water = l.sla.q.water,
              l.sla.l.water = l.sla.l.water,
              q.sla.q.water.noint = q.sla.q.water.noint,
              q.sla.l.water.noint = q.sla.l.water.noint,
              l.sla.q.water.noint = l.sla.q.water.noint,
              l.sla.l.water.noint = l.sla.l.water.noint,
              
              q.herb.q.water = q.herb.q.water,
              q.herb.l.water = q.herb.l.water,
              l.herb.q.water = l.herb.q.water,
              l.herb.l.water = l.herb.l.water,
              q.herb.q.water.noint = q.herb.q.water.noint,
              q.herb.l.water.noint = q.herb.l.water.noint,
              l.herb.q.water.noint = l.herb.q.water.noint,
              l.herb.l.water.noint = l.herb.l.water.noint
              
              
  ))
  
}

attach(modelDefinitions())