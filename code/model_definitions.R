moddata <- deltaData %>% filter(unified_names %in% myTree.rand.poly$tip.label)

modelDefinitions <-  function () {
  
  # Runs all of model definitions
  
  # NO TRAITS
  no.traits <- gls(
    lrrtb ~ richness + labu + lrrsla + site,
    data = moddata,
    method = "ML",
    # correlation = corMartins(2.718282, 
    #                          phy = myTree.rand.poly,
    #                          form = ~unified_names,
    #                          fixed = T),
    # na.action = na.omit
  )
  
  # FULL MODEL
  full.model <- gls(
    lrrtb ~ lsla.d*site+ I(lsla.d^2)*site+water.cont*site+I(water.cont^2)*site+perc.herb*site+I(perc.herb^2)*site+richness+labu+lrrsla+site,
    data = ),
    method = "ML"
  )
  
  # FULL MODEL
  full.model.noint <- gls(
    lrrtb ~ lsla.d + I(lsla.d^2) + water.cont + I(water.cont^2) + perc.herb + I(perc.herb^2) + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  all.linear <- gls(
    lrrtb ~ lsla.d*site + water.cont*site + perc.herb*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  all.linear.noint <- gls(
    lrrtb ~ lsla.d+water.cont+perc.herb+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  
  # Individual traits-------------------------------------------------
  
  ## SLA
  just.sla.linear.noint <- gls(
    lrrtb ~ lsla.d + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.sla.linear <- gls(
    lrrtb ~ lsla.d*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.sla.quadratic.noint <- gls(
    lrrtb ~ lsla.d + I(lsla.d^2) + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.sla.quadratic <- gls(
    lrrtb ~ lsla.d*site + I(lsla.d^2)*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  ## WATER
  just.water.linear.noint <- gls(
    lrrtb ~ water.cont + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.water.linear <- gls(
    lrrtb ~ water.cont*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.water.quadratic.noint <- gls(
    lrrtb ~ water.cont + I(water.cont^2) + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.water.quadratic <- gls(
    lrrtb ~ water.cont*site + I(water.cont^2)*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  ## HERBIVORY
  just.herb.linear.noint <- gls(
    lrrtb ~ perc.herb + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.herb.linear <- gls(
    lrrtb ~ perc.herb*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.herb.quadratic.noint <- gls(
    lrrtb ~ perc.herb + I(perc.herb^2) + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  just.herb.quadratic <- gls(
    lrrtb ~ perc.herb*site + I(perc.herb^2)*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  
  # COMBINATIONS -----------------------------------------------------------------
  # Interaction
  # quadratic SLA, HERB -------------------
  q.sla.q.herb <- gls(
    lrrtb ~ lsla.d*site+I(lsla.d^2)*site+perc.herb*site+I(perc.herb^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic SLA, linear HERB
  q.sla.l.herb <- gls(
    lrrtb ~ lsla.d*site+ I(lsla.d^2)*site+perc.herb*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, quadratic HERB
  l.sla.q.herb <- gls(
    lrrtb ~ lsla.d*site+perc.herb*site+I(perc.herb^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, linear HERB
  l.sla.l.herb <- gls(
    lrrtb ~ lsla.d*site+perc.herb*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  
  # No interaction
  # quadratic SLA, HERB 
  q.sla.q.herb.noint <- gls(
    lrrtb ~ lsla.d+I(lsla.d^2)+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic SLA, linear HERB
  q.sla.l.herb.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+perc.herb+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, quadratic HERB
  l.sla.q.herb.noint <- gls(
    lrrtb ~ lsla.d+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, linear HERB
  l.sla.l.herb.noint <- gls(
    lrrtb ~ lsla.d+perc.herb+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  
  # Interaction
  # quadratic HERB, WATER----------------------
  q.herb.q.water <- gls(
    lrrtb ~ water.cont*site+I(water.cont^2)*site+perc.herb*site+I(perc.herb^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic HERB, linear WATER
  q.herb.l.water <- gls(
    lrrtb ~ water.cont*site+perc.herb*site+I(perc.herb^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear HERB, quadratic WATER
  l.herb.q.water <- gls(
    lrrtb ~ water.cont*site+I(water.cont^2)*site+perc.herb*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear HERB, linear WATER
  l.herb.l.water <- gls(
    lrrtb ~ water.cont*site+perc.herb*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  
  # NO Interaction
  # quadratic HERB, WATER
  q.herb.q.water.noint <- gls(
    lrrtb ~ water.cont+I(water.cont^2)+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic HERB, linear WATER
  q.herb.l.water.noint <- gls(
    lrrtb ~ water.cont+perc.herb+I(perc.herb^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear HERB, quadratic WATER
  l.herb.q.water.noint <- gls(
    lrrtb ~ water.cont+I(water.cont^2)+perc.herb+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear HERB, linear WATER
  l.herb.l.water.noint <- gls(
    lrrtb ~ water.cont+perc.herb+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  
  
  
  # quadratic SLA, WATER-----------------------------
  # Interaction
  q.sla.q.water <- gls(
    lrrtb ~ lsla.d*site+ I(lsla.d^2)*site+water.cont*site+I(water.cont^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic SLA, linear WATER
  q.sla.l.water <- gls(
    lrrtb ~ lsla.d*site+ I(lsla.d^2)*site+water.cont*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, quadratic WATER
  l.sla.q.water <- gls(
    lrrtb ~ lsla.d*site+water.cont*site+I(water.cont^2)*site+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, linear WATER
  l.sla.l.water <- gls(
    lrrtb ~ lsla.d*site + water.cont*site + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
  )
  
  # NO Interaction
  q.sla.q.water.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+water.cont+I(water.cont^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # quadratic SLA, linear WATER
  q.sla.l.water.noint <- gls(
    lrrtb ~ lsla.d+ I(lsla.d^2)+water.cont+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, quadratic WATER
  l.sla.q.water.noint <- gls(
    lrrtb ~ lsla.d+water.cont+I(water.cont^2)+richness+labu+lrrsla+site,
    data = moddata,
    method = "ML"
  )
  # linear SLA, linear WATER
  l.sla.l.water.noint <- gls(
    lrrtb ~ lsla.d + water.cont + richness + labu + lrrsla + site,
    data = moddata,
    method = "ML"
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