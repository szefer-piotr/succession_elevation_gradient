source('code/log_ratio_extraction.R')

# RICHNESS.LR
# BIOMASS.LR
# DIVERSITY.LR
# DENSITY.LR

RichLRR = RICHNESS.LR$lratio
ConRicVal = RICHNESS.LR$cval
DivLRR = DIVERSITY.LR$lratio
ConDivVal = DIVERSITY.LR$cval
DenLRR = DENSITY.LR$lratio
ConDenVal = DENSITY.LR$cval

LRR_DEP_TEST_DATA <- BIOMASS.LR %>%
  rename(Site = 'site',
         Garden = 'garden',
         Treatment = 'treatment',
         TrtBioVal = 'value',
         ConBioVal = 'cval',
         BioLRR = 'lratio')

# LRR_DEP_TEST_DATA %>%
#   dplyr::mutate(RichLRR = RICHNESS.LR$lratio,
#                 ConRicVal = RICHNESS.LR$cval,
#                 DivLRR = DIVERSITY.LR$lratio,
#                 ConDivVal = DIVERSITY.LR$cval,
#                 DenLRR = DENSITY.LR$lratio,
#                 ConDenVal = DENSITY.LR$cval)

LRR_DEP_TEST_DATA$RichLRR = RichLRR
LRR_DEP_TEST_DATA$ConRicVal = ConRicVal
LRR_DEP_TEST_DATA$DivLRR = DivLRR
LRR_DEP_TEST_DATA$ConDivVal = ConDivVal
LRR_DEP_TEST_DATA$DenLRR = DenLRR
LRR_DEP_TEST_DATA$ConDenVal = ConDenVal
LRR_DEP_TEST_DATA = LRR_DEP_TEST_DATA %>%
  mutate(SiteTreatment = paste(Site, Treatment, sep = " "))

library(ggplot2)

responses <- c("BioLRR","RichLRR","DivLRR","DenLRR")
predictors <- c("ConBioVal","ConRicVal","ConDivVal","ConDenVal")

# responses <- c("BioLRR")
# predictors <- c("ConBioVal","ConRicVal")

plot_list <- list()
test_list <- list()
for (resp in responses){
  for (pred in predictors) {
    
    print(paste(resp, pred))
  
    pp <- ggplot(LRR_DEP_TEST_DATA %>%
                   filter(Treatment == "i"), aes_string(x=pred, y=resp, 
                                        color = "Site")) + 
      geom_point() + 
      geom_smooth(method = "lm")+
      coord_trans(x = 'log')
      
    plot_list[[paste(resp, pred)]] <- pp
    
    # Test each 
    
    
    
  }
}


# Herbivory
hplot_list <- list()

for (resp in responses){
  for (pred in predictors) {
    
    print(paste(resp, pred))
    
    pp <- ggplot(LRR_DEP_TEST_DATA %>%
                   filter(Treatment == "h"), aes_string(x=pred, y=resp, 
                                                        color = "Site")) + 
      geom_point() + 
      geom_smooth(method = "lm")+
      coord_trans(x = 'log')
    
    hplot_list[[paste(resp, pred)]] <- pp
    
    # Test each 
    
    
    
  }
}


# Predators
pplot_list <- list()

for (resp in responses){
  for (pred in predictors) {
    
    print(paste(resp, pred))
    
    pp <- ggplot(LRR_DEP_TEST_DATA %>%
                   filter(Treatment == "p"), aes_string(x=pred, y=resp, 
                                                        color = "Site")) + 
      geom_point() + 
      geom_smooth(method = "lm")+
      coord_trans(x = 'log')
    
    pplot_list[[paste(resp, pred)]] <- pp
    
    # Test each 
    
    
    
  }
}

ggpubr::ggarrange(plotlist = plot_list)
ggpubr::ggarrange(plotlist = hplot_list)
ggpubr::ggarrange(plotlist = pplot_list)

# Test
LRR_DEP_TEST_DATA

# Are LRRs correlated, i.e., are effects of richness correalted with effects on biomass?
ggplot(LRR_DEP_TEST_DATA, aes(y = BioLRR, 
                              x = RichLRR, 
                              color= Treatment)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~Site)

# Yes, they are. Significantly?
lrrm1 <- lm(RichLRR ~ BioLRR * Site + Treatment, 
            data = LRR_DEP_TEST_DATA)
lrrm2 <- lm(RichLRR ~ BioLRR + Treatment, 
            data = LRR_DEP_TEST_DATA)

anova(lrrm1, lrrm2)

summary(lrrm1)
# Yes, they are, irrespective of site.

# Accross all treatments
# bp1 <- ggplot(LRR_DEP_TEST_DATA, aes(x=BioLRR, y = log(ConRicVal), color = Treatment)) + 
#   geom_point() + 
#   geom_smooth()
# 
# bp2 <- ggplot(LRR_DEP_TEST_DATA, aes(x=BioLRR, y =log(ConDivVal), color = Treatment)) + 
#   geom_point()
# 
# bp3 <- ggplot(LRR_DEP_TEST_DATA, aes(x=BioLRR, y=log(ConDenVal), color = Treatment)) + 
#   geom_point()

# Correlation between LRRs ---- 
# library(corrplot)
# cor.dat <- as.matrix(LRR_DEP_TEST_DATA[, c("BioLRR","RichLRR","DivLRR","DenLRR")])
# M <- cor(cor.dat)
# M.test <- cor.mtest(cor.dat)
# corrplot::corrplot(M, p.mat = M.test$p)

## Biomass is correlated to density. Three groups of models: ----
# 1. BIOLRR ~ biomass_C + richenss_C
# 2. RICLRR ~ biomass_C + richenss_C
# 3. DIVLRR ~ biomass_C + richenss_C
# Additional herbivory removed because it is inversely related to the 

# bp4 <- ggplot(LRR_DEP_TEST_DATA%>% 
#                 filter(Treatment != "h"), 
#               aes(y=BioLRR, x=log(ConBioVal), color = Site)) + 
#   geom_point()+ 
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
#   facet_wrap(~ Treatment)

# Herbivory addition needs to be removed because it is inversely proportional to 

bioLRR.bio <- ggplot(LRR_DEP_TEST_DATA %>% 
                      filter(Treatment != "h"), aes(y=BioLRR, x=log(ConBioVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

bioLRR.ric <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=BioLRR, x=log(ConRicVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

ricLRR.bio <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=RichLRR, x=log(ConBioVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

ricLRR.ric <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=RichLRR, x=log(ConRicVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

divLRR.bio <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=DivLRR, x=log(ConBioVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

divLRR.ric <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=DivLRR, x=log(ConRicVal), color = Site)) + 
  geom_point()+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = F) +
  facet_wrap(~ Treatment) + 
  theme_bw()

# ggpubr::ggarrange(bioLRR.bio,
#                   bioLRR.ric,
#                   ricLRR.bio,
#                   ricLRR.ric,
#                   divLRR.bio,
#                   divLRR.ric)

# Make some transformed columns
LRR_DEP_TEST_DATA$lcbv <- log(LRR_DEP_TEST_DATA$ConBioVal)
LRR_DEP_TEST_DATA$lcrv <- log(LRR_DEP_TEST_DATA$ConRicVal)

## BIOLRR test ---- 
biolrr.bio.mod0 <- lm(BioLRR ~ lcbv, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
biolrr.bio.mod1 <- lm(BioLRR ~ lcbv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
biolrr.bio.mod2 <- lm(BioLRR ~ lcbv * Treatment * Site, 
                     data = LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"))

anova(biolrr.bio.mod0, biolrr.bio.mod1)
anova(biolrr.bio.mod1, biolrr.bio.mod2)
# Simpler model is better
summary(biolrr.bio.mod1)

bioLRR.bio <- ggplot(LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"), aes(y=BioLRR, x=log(ConBioVal))) + 
  geom_point(aes(color = Site))+ 
  geom_smooth(method = "lm", formula = y ~ x,
              se = T) +
  facet_wrap(~ Treatment) + 
  theme_bw()

################################################
# Biomass at control site determines the LRR ~ 42%

# Richness explaining LRR in biomass
biolrr.ric.mod0 <- lm(BioLRR ~ lcrv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
biolrr.ric.mod1 <- lm(BioLRR ~ lcrv * Treatment * Site, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))

anova(biolrr.ric.mod0, biolrr.ric.mod1) # Simpler model is better
summary(biolrr.ric.mod0)

################################################
# Richness does not explain the LRR in biomass


# RICLRR test
riclrr.bio.mod0 <- lm(RichLRR ~ lcbv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
riclrr.bio.mod1 <- lm(RichLRR ~ lcbv * Treatment * Site, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))

anova(riclrr.bio.mod0, riclrr.bio.mod1) # More complicated is better
summary(riclrr.bio.mod1)

###########################################
# Biomass does not explain lrr in richness

riclrr.ric.mod0 <- lm(RichLRR ~ lcrv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
riclrr.ric.mod1 <- lm(RichLRR ~ lcrv * Treatment * Site, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))

anova(riclrr.ric.mod0, riclrr.ric.mod1) # More complicated is better
summary(riclrr.ric.mod1)

################################################
# Richness does not explain lrr in richness

# DIVLRR test
divlrr.bio.mod0 <- lm(DivLRR ~ lcbv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
divlrr.bio.mod1 <- lm(DivLRR ~ lcbv * Treatment * Site, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))

anova(divlrr.bio.mod0, divlrr.bio.mod1) # More complicated is better
summary(divlrr.bio.mod1)

##############################################
# biomass does not explain LRR in diversity

divlrr.ric.mod0 <- lm(DivLRR ~ lcrv * Treatment, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))
divlrr.ric.mod1 <- lm(DivLRR ~ lcrv * Treatment * Site, 
                      data = LRR_DEP_TEST_DATA %>% 
                        filter(Treatment != "h"))

anova(divlrr.ric.mod0, divlrr.ric.mod1) # Simpler is better
summary(divlrr.ric.mod0)

##############################################
# richness does not explain LRR in diversity


#############################################################################
###########   BIOMASS
#############################################################################
# More complex models with two descriptors ---
# biolrr.mod0 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Treatment*Site, 
#                   data = LRR_DEP_TEST_DATA %>% 
#                     filter(Treatment != "h"))
# 
# # Alternative models
# biolrr.mod1 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod2 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod3 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod4 <- lm(BioLRR ~ lcbv * Treatment*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod5 <- lm(BioLRR ~ lcbv * Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod6 <- lm(BioLRR ~ lcbv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod7 <- lm(BioLRR ~ 1,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod8 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv*Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod9 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod10 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod11 <- lm(BioLRR ~ lcrv * Treatment*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod12 <- lm(BioLRR ~ lcrv * Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod13 <- lm(BioLRR ~ lcrv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# biolrr.mod14 <- lm(BioLRR ~ 1,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# # Model withouth treatments to comparison
# biolrr.modNoTrt <- lm(BioLRR ~ lcbv * Site + lcrv * Site, 
#                   data = LRR_DEP_TEST_DATA %>% 
#                     filter(Treatment != "h"))
# 
# 
# AIC(biolrr.mod0,
#     biolrr.mod1,
#     biolrr.mod2,
#     biolrr.mod3,
#     biolrr.mod4,
#     biolrr.mod5,
#     biolrr.mod6,
#     biolrr.mod7,
#     biolrr.mod8,
#     biolrr.mod9,
#     biolrr.mod10,
#     biolrr.mod11,
#     biolrr.mod12,
#     biolrr.mod13,
#     biolrr.mod14)
# 
# # anova(biolrr.mod5, biolrr.mod0) # This model is almost better
# 
# library(sjPlot)
# 
# 
# p1 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Treatment", "Site"),
#                  show.data = T)
# p1.1 <- plot_model(biolrr.mod0, type="pred", terms = c("lcrv", "Treatment", "Site"),
#                    show.data = T)
# p2 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Site"))
# p3 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Treatment"))


#############################################################################
###########   Diversity  
#############################################################################

# # More complicated models for diversity ----
# divlrr.mod0 <- lm(DivLRR ~ lcbv * Treatment*Site + lcrv*Treatment*Site, 
#                   data = LRR_DEP_TEST_DATA %>% 
#                     filter(Treatment != "h"))
# 
# # Alternative models
# divlrr.mod1 <- lm(DivLRR ~ lcbv * Treatment*Site + lcrv*Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod2 <- lm(DivLRR ~ lcbv * Treatment*Site + lcrv*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod3 <- lm(DivLRR ~ lcbv * Treatment*Site + lcrv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod4 <- lm(DivLRR ~ lcbv * Treatment*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod5 <- lm(DivLRR ~ lcbv * Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod6 <- lm(DivLRR ~ lcbv,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod7 <- lm(DivLRR ~ 1,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod8 <- lm(DivLRR ~ lcrv * Treatment*Site + lcbv*Treatment,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod9 <- lm(DivLRR ~ lcrv * Treatment*Site + lcbv*Site,
#                   data = LRR_DEP_TEST_DATA %>%
#                     filter(Treatment != "h"))
# 
# divlrr.mod10 <- lm(DivLRR ~ lcrv * Treatment*Site + lcbv,
#                    data = LRR_DEP_TEST_DATA %>%
#                      filter(Treatment != "h"))
# 
# divlrr.mod11 <- lm(DivLRR ~ lcrv * Treatment*Site,
#                    data = LRR_DEP_TEST_DATA %>%
#                      filter(Treatment != "h"))
# 
# divlrr.mod12 <- lm(DivLRR ~ lcrv * Treatment,
#                    data = LRR_DEP_TEST_DATA %>%
#                      filter(Treatment != "h"))
# 
# divlrr.mod13 <- lm(DivLRR ~ lcrv,
#                    data = LRR_DEP_TEST_DATA %>%
#                      filter(Treatment != "h"))
# 
# divlrr.mod14 <- lm(DivLRR ~ 1,
#                    data = LRR_DEP_TEST_DATA %>%
#                      filter(Treatment != "h"))
# 
# # Model withouth treatments to comparison
# divlrr.modNoTrt <- lm(DivLRR ~ lcbv * Site + lcrv * Site, 
#                       data = LRR_DEP_TEST_DATA %>% 
#                         filter(Treatment != "h"))


# AIC(divlrr.mod0,
#     divlrr.mod1,
#     divlrr.mod2,
#     divlrr.mod3,
#     divlrr.mod4,
#     divlrr.mod5,
#     divlrr.mod6,
#     divlrr.mod7,
#     divlrr.mod8,
#     divlrr.mod9,
#     divlrr.mod10,
#     divlrr.mod11,
#     divlrr.mod12,
#     divlrr.mod13,
#     divlrr.mod14)

# anova(biolrr.mod5, biolrr.mod0) # This model is almost better

library(sjPlot)



########################################################
#######
###

# NONLINEAR TRENDS

###
#######
########################################################

########################################################
###########   BIOMASS
########################################################

# More complex models with two descriptors ----
biolrr.nl.mod0 <- lm(BioLRR ~ lcbv * Treatment*Site + 
                    I(lcbv^2) * Treatment*Site + 
                    I(lcrv^2) * Treatment*Site + 
                    lcrv*Treatment*Site, 
                  data = LRR_DEP_TEST_DATA %>% 
                    filter(Treatment != "h"))
library(MASS)
final.biolrr.nl.mod <- stepAIC(biolrr.nl.mod0, scope = BioLRR ~ 1)
summary(final.biolrr.nl.mod)


########################################################
###########   RICHNESS
########################################################

# More complex models with two descriptors ----
riclrr.nl.mod0 <- lm(RichLRR ~ lcbv * Treatment*Site + 
                       I(lcbv^2) * Treatment*Site + 
                       I(lcrv^2) * Treatment*Site + 
                       lcrv*Treatment*Site, 
                     data = LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"))
final.riclrr.nl.mod <- stepAIC(riclrr.nl.mod0, scope = RichLRR ~ 1)
summary(final.riclrr.nl.mod)

########################################################
###########   DENSITY
########################################################

# More complex models with two descriptors ----
denlrr.nl.mod0 <- lm(DenLRR ~ lcbv * Treatment*Site + 
                       I(lcbv^2) * Treatment*Site + 
                       I(lcrv^2) * Treatment*Site + 
                       lcrv*Treatment*Site, 
                     data = LRR_DEP_TEST_DATA %>% 
                       filter(Treatment != "h"))
final.denlrr.nl.mod <- stepAIC(denlrr.nl.mod0, scope = DenLRR ~ 1)
summary(final.denlrr.nl.mod)

#############################################################################
###########   Diversity  
#############################################################################


# More complex models with two descriptors ---
divlrr.nl.mod0 <- lm(DivLRR ~ lcbv * Treatment*Site +
                       I(lcbv^2) * Treatment*Site +
                       I(lcrv^2) * Treatment*Site +
                       lcrv*Treatment*Site,
                     data = LRR_DEP_TEST_DATA %>%
                       filter(Treatment != "h"))
final.divlrr.nl.mod <- stepAIC(divlrr.nl.mod0, scope = DivLRR ~ 1)
summary(final.divlrr.nl.mod)


#############################################################################
 
#############################################################################

# data <- as.data.frame(LRR_DEP_TEST_DATA[LRR_DEP_TEST_DATA$Treatment != "h",
#                                         'DivLRR'])


# Compare models with and without the Treatment variable ---
nt.biolrr.nl.mod <- update(final.biolrr.nl.mod, 
                           . ~ lcbv*Site + lcrv*Site + I(lcbv^2)*Site + I(lcrv^2)*Site)

nt.divlrr.nl.mod <- update(final.divlrr.nl.mod, 
                           . ~ lcbv*Site + lcrv*Site + I(lcbv^2)*Site + I(lcrv^2)*Site)

############################################################################
# Diversity Lrr predictions and plot ----
############################################################################
model <- nt.divlrr.nl.mod
pred.val <- 'lcbv'
data <- LRR_DEP_TEST_DATA %>% 
  filter(Treatment != "h")

sites = c("wanang","numba","yawan")
treatments = c("i", "f","p")
grain = 1000











#####################################################################
# Predict Diversity plot
# Build a data frame with varying pred.var variable and average value for the rest
pvv <- data[,pred.val] # pred.va. values
predictor <- seq(length = grain, from = min(pvv), to = max(pvv))
  
# Make a df for predictions.
newdata <- expand.grid(lcbv = pvv$lcbv, 
                       Site = sites, 
                       Treatment = treatments, 
                       lcrv = mean(data$lcrv, na.rm=T))

newdata$lcrv <- ifelse(newdata$Site == "wanang", 
                       mean(data[data$Site == "wanang", ]$lcrv, na.rm=T),
                       ifelse(newdata$Site == "numba", 
                              mean(data[data$Site == "numba", ]$lcrv, na.rm=T),
                              mean(data[data$Site == "yawan", ]$lcrv, na.rm=T)))  

pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Richness = newdata$lcrv)
  
DivPredPlot <- ggplot(pred.df, aes(x = lcbv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcbv, y = RichLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[Woody plant biomass]') +
  ylab("LRR of diversity (Simpson's index)") + ylim(c(-3,3))










####################################################################
# Biomass LRR pediction and plot ----
model <- final.biolrr.nl.mod
pred.val <- 'lcbv'
data <- LRR_DEP_TEST_DATA %>% 
  filter(Treatment != "h")

sites = c("wanang","numba","yawan")
treatments = c("i", "f","p")
grain = 1000

require(dplyr)

# Build a data frame with varying pred.var variable and average value for the rest
pvv <- data[,pred.val] # pred.va. values
predictor <- seq(length = grain, from = min(pvv), to = max(pvv))

# Make a df for predictions.
newdata <- expand.grid(lcbv = pvv$lcbv, 
                       Site = sites, 
                       Treatment = treatments)

newdata$lcrv <- ifelse(newdata$Site == "wanang", 
                       mean(data[data$Site == "wanang", ]$lcrv, na.rm=T),
                       ifelse(newdata$Site == "numba", 
                              mean(data[data$Site == "numba", ]$lcrv, na.rm=T),
                              mean(data[data$Site == "yawan", ]$lcrv, na.rm=T)))

# Predict
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Richness = newdata$lcrv)

BioPredPlot <- ggplot(pred.df, aes(x = lcbv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcbv, y = BioLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[Woody plant biomass]') +
  ylab('LRR of biomass') + ylim(c(-3,3))








####################################################################
# Richness LRR pediction and plot ----
model <- final.riclrr.nl.mod
pred.val <- 'lcbv'
data <- LRR_DEP_TEST_DATA %>% 
  filter(Treatment != "h")

sites = c("wanang","numba","yawan")
treatments = c("i", "f","p")
grain = 1000

require(dplyr)

# Build a data frame with varying pred.var variable and average value for the rest
pvv <- data[,pred.val] # pred.va. values
predictor <- seq(length = grain, from = min(pvv), to = max(pvv))

# Make a df for predictions.
newdata <- expand.grid(lcbv = pvv$lcbv, 
                       Site = sites, 
                       Treatment = treatments)

newdata$lcrv <- ifelse(newdata$Site == "wanang", 
                       mean(data[data$Site == "wanang", ]$lcrv, na.rm=T),
                       ifelse(newdata$Site == "numba", 
                              mean(data[data$Site == "numba", ]$lcrv, na.rm=T),
                              mean(data[data$Site == "yawan", ]$lcrv, na.rm=T)))

# Predict
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Richness = newdata$lcrv)

RicPredPlot <- ggplot(pred.df, aes(x = lcbv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcbv, y = RichLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[Woody plant biomass]') +
  ylab('LRR of species richness') + ylim(c(-3,3))


####################################################################
# Density LRR prediction and plot ----
model <- final.denlrr.nl.mod
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Richness = newdata$lcrv)

DenPredPlot <- ggplot(pred.df, aes(x = lcbv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcbv, y = DenLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[Woody plant biomass]') +
  ylab('LRR of density') + ylim(c(-3,3))

########################################################################
########################################################################
### PREDICTIONS FOR LCRV

# New data
pred.val <- 'lcrv'
pvv <- data[,pred.val] # pred.va. values
predictor <- seq(length = grain, from = min(pvv), to = max(pvv))

# Make a df for predictions.
newdata <- expand.grid(lcrv = pvv$lcrv,
Site = sites,
Treatment = treatments)

newdata$lcbv <- ifelse(newdata$Site == "wanang",
mean(data[data$Site == "wanang", ]$lcbv, na.rm=T),
ifelse(newdata$Site == "numba",
mean(data[data$Site == "numba", ]$lcbv, na.rm=T),
mean(data[data$Site == "yawan", ]$lcbv, na.rm=T)))

####################################################################
# Biomass LRR ----
model <- final.biolrr.nl.mod
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Biomass = newdata$lcbv)

BioPredPlotR <- ggplot(pred.df, aes(x = lcrv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcrv, y = BioLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[No. of woody plant species]') +
  ylab('LRR of biomass') + ylim(c(-3,3))


# Richness LRR ----
model <- final.riclrr.nl.mod
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Biomass = newdata$lcbv)

RicPredPlotR <- ggplot(pred.df, aes(x = lcrv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcrv, y = RichLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[No. of woody plant species]') +
  ylab('LRR of species richness') + ylim(c(-3,3))

# Div LRR ----
model <- final.divlrr.nl.mod
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Biomass = newdata$lcbv)

DivPredPlotR <- ggplot(pred.df, aes(x = lcrv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcrv, y = DivLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[No. of woody plant species]') +
  ylab('LRR of diversity') + ylim(c(-3,3))

# Biomass LRR ----
model <- final.denlrr.nl.mod
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Biomass = newdata$lcbv)

DenPredPlotR <- ggplot(pred.df, aes(x = lcrv, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = lcrv, y = DenLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Treatment) +
  xlab('Log[No. of woody plant species]') +
  ylab('LRR of density') + ylim(c(-3,3))

















# Repeat the same analyses for individual sites: Wanang, Numba Yawan ----

# Numba should have the highest R2, but withouth the treatment

library(caret)

# ### Wanang -----
w.bio.nl <- lm(BioLRR ~ lcbv * Treatment +
                       I(lcbv^2) * Treatment +
                       I(lcrv^2) * Treatment +
                       lcrv*Treatment,
                     data = LRR_DEP_TEST_DATA %>%
                       filter(Treatment != "h",
                              Site == "wanang"))
final.w.bio.nl <- stepAIC(w.bio.nl, scope = BioLRR ~ 1)
summary(final.w.bio.nl)

# w.ric.nl <- lm(RichLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "wanang"))
# final.w.ric.nl <- stepAIC(w.ric.nl, scope = RichLRR ~ 1)
# summary(final.w.ric.nl)
# 
# w.den.nl <- lm(DenLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "wanang"))
# final.w.den.nl <- stepAIC(w.den.nl, scope = DenLRR ~ 1)
# summary(final.w.den.nl)
# 
# w.div.nl <- lm(DivLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "wanang"))
# final.w.div.nl <- stepAIC(w.div.nl, scope = DivLRR ~ 1)
# summary(final.w.div.nl)
# 
# # Numba ----
# n.bio.nl <- lm(BioLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "numba"))
# final.n.bio.nl <- stepAIC(n.bio.nl, scope = BioLRR ~ 1)
# summary(final.n.bio.nl)
# 
# 
# n.ric.nl <- lm(RichLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "numba"))
# final.n.ric.nl <- stepAIC(n.ric.nl, scope = RichLRR ~ 1)
# summary(final.n.ric.nl)
# 
# n.den.nl <- lm(DenLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "numba"))
# final.n.den.nl <- stepAIC(n.den.nl, scope = DenLRR ~ 1)
# summary(final.n.den.nl)
# 
# n.div.nl <- lm(DivLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "numba"))
# final.n.div.nl <- stepAIC(n.div.nl, scope = DivLRR ~ 1)
# summary(final.n.div.nl)
# 
# # Yawan ----
# y.bio.nl <- lm(BioLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "yawan"))
# final.y.bio.nl <- stepAIC(y.bio.nl, scope = BioLRR ~ 1)
# summary(final.y.bio.nl)
# 
# 
# y.ric.nl <- lm(RichLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "yawan"))
# final.y.ric.nl <- stepAIC(y.ric.nl, scope = RichLRR ~ 1)
# summary(final.y.ric.nl)
# 
# y.den.nl <- lm(DenLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "yawan"))
# final.y.den.nl <- stepAIC(y.den.nl, scope = DenLRR ~ 1)
# summary(final.y.den.nl)
# 
# y.div.nl <- lm(DivLRR ~ lcbv * Treatment + 
#                  I(lcbv^2) * Treatment + 
#                  I(lcrv^2) * Treatment + 
#                  lcrv*Treatment, 
#                data = LRR_DEP_TEST_DATA %>% 
#                  filter(Treatment != "h", 
#                         Site == "yawan"))
# final.y.div.nl <- stepAIC(y.div.nl, scope = DivLRR ~ 1)
# summary(final.y.div.nl)


### Wanang -----
w.bio.nl <- lm(BioLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "wanang"))
final.w.bio.nl <- stepAIC(w.bio.nl, scope = BioLRR ~ 1)
summary(final.w.bio.nl)


w.ric.nl <- lm(RichLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "wanang"))
final.w.ric.nl <- stepAIC(w.ric.nl, scope = RichLRR ~ 1)
summary(final.w.ric.nl)

w.den.nl <- lm(DenLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "wanang"))
final.w.den.nl <- stepAIC(w.den.nl, scope = DenLRR ~ 1)
summary(final.w.den.nl)

w.div.nl <- lm(DivLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "wanang"))
final.w.div.nl <- stepAIC(w.div.nl, scope = DivLRR ~ 1)
summary(final.w.div.nl)

# Numba ----
n.bio.nl <- lm(BioLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "numba"))
final.n.bio.nl <- stepAIC(n.bio.nl, scope = BioLRR ~ 1)
summary(final.n.bio.nl)


n.ric.nl <- lm(RichLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "numba"))
final.n.ric.nl <- stepAIC(n.ric.nl, scope = RichLRR ~ 1)
summary(final.n.ric.nl)

n.den.nl <- lm(DenLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "numba"))
final.n.den.nl <- stepAIC(n.den.nl, scope = DenLRR ~ 1)
summary(final.n.den.nl)

n.div.nl <- lm(DivLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "numba"))
final.n.div.nl <- stepAIC(n.div.nl, scope = DivLRR ~ 1)
summary(final.n.div.nl)

# Yawan ----
y.bio.nl <- lm(BioLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "yawan"))
final.y.bio.nl <- stepAIC(y.bio.nl, scope = BioLRR ~ 1)
summary(final.y.bio.nl)


y.ric.nl <- lm(RichLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "yawan"))
final.y.ric.nl <- stepAIC(y.ric.nl, scope = RichLRR ~ 1)
summary(final.y.ric.nl)

y.den.nl <- lm(DenLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "yawan"))
final.y.den.nl <- stepAIC(y.den.nl, scope = DenLRR ~ 1)
summary(final.y.den.nl)

y.div.nl <- lm(DivLRR ~ lcbv + 
                 I(lcbv^2) + 
                 I(lcrv^2) + 
                 lcrv, 
               data = LRR_DEP_TEST_DATA %>% 
                 filter(Treatment != "h", 
                        Site == "yawan"))
final.y.div.nl <- stepAIC(y.div.nl, scope = DivLRR ~ 1)
summary(final.y.div.nl)



# r2data <- data.frame(
#   Site = rep(c("Wanang", "Numba","Yawan"), each = 4),
#   Descriptor = rep(c("Biomass LRR",
#                      "Richness LRR",
#                      "Density LRR",
#                      "Diversity LRR"), 3),
#   AdjustedR2 = c(summary(final.w.bio.nl)$adj.r.squared,
#                  summary(final.w.ric.nl)$adj.r.squared,
#                  summary(final.w.den.nl)$adj.r.squared,
#                  summary(final.w.div.nl)$adj.r.squared,
#                  summary(final.n.bio.nl)$adj.r.squared,
#                  summary(final.n.ric.nl)$adj.r.squared,
#                  summary(final.n.den.nl)$adj.r.squared,
#                  summary(final.n.div.nl)$adj.r.squared,
#                  summary(final.y.bio.nl)$adj.r.squared,
#                  summary(final.y.ric.nl)$adj.r.squared,
#                  summary(final.y.den.nl)$adj.r.squared,
#                  summary(final.y.div.nl)$adj.r.squared)
# )

r2data <- data.frame(
  Site = rep(c("Wanang", "Numba","Yawan"), each = 4),
  Descriptor = rep(c("Biomass LRR",
                     "Richness LRR",
                     "Density LRR",
                     "Diversity LRR"), 3),
  R2 = c(summary(w.bio.nl)$r.squared,
         summary(w.ric.nl)$r.squared,
         summary(w.den.nl)$r.squared,
         summary(w.div.nl)$r.squared,
         summary(n.bio.nl)$r.squared,
         summary(n.ric.nl)$r.squared,
         summary(n.den.nl)$r.squared,
         summary(n.div.nl)$r.squared,
         summary(y.bio.nl)$r.squared,
         summary(y.ric.nl)$r.squared,
         summary(y.den.nl)$r.squared,
         summary(y.div.nl)$r.squared)
)

r2data$Site <- factor(r2data$Site, 
                         levels = c("Wanang", "Numba","Yawan"))

# Do all this for sites and each treatment

r2data <- data.frame()

for (desc in c("BioLRR", "RichLRR", "DivLRR", "DenLRR")) {
  
  for (site in c("wanang","numba","yawan")) {
    
    for (treat in c("i","f","p")) {
      
      data <- LRR_DEP_TEST_DATA %>% 
        filter(Treatment == treat, 
               Site == site)
      
      form <- formula(paste(desc, "~ lcbv+I(lcbv^2)+I(lcrv^2)+lcrv"))
      
      mod <- lm(form, data = data)
      
      val <- summary(mod)$adj.r.square
        
      r2row <- data.frame(Descriptors = desc,
                          Site = site,
                          Treatement = treat,
                          AdjR2 = val)
      
      r2data <- rbind(r2data, r2row)
      
    }
    
  }
  
}

r2data$Site <- factor(r2data$Site, 
                      levels = c("wanang", "numba","yawan"))




# BIOMASS vs SPECIES RICHNESS ----
# rb.cor.dat <- RICHNESS
# rb.cor.dat$bio = BIOMASS$tot_bio
# 
# ggplot(rb.cor.dat, aes(x=sp_no, y=bio)) +
#   geom_point(aes(colour = treatment)) +
#   scale_x_continuous(trans='log2') +
#   scale_y_continuous(trans='log2') + 
#   stat_smooth(method = "lm")
# 
# # control no relation
# summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
#              filter(treatment == "c")))
# # Insecticide - YES
# summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
#              filter(treatment == "i")))
# # Fungicide
# summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
#              filter(treatment == "f")))
# # Herbivore YES
# summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
#              filter(treatment == "h")))
# # Predator YES
# summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
#              filter(treatment == "p")))

