source('code/log_ratio_extraction.R')

RICHNESS.LR
BIOMASS.LR
DIVERSITY.LR
DENSITY.LR

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
library(corrplot)
cor.dat <- as.matrix(LRR_DEP_TEST_DATA[, c("BioLRR","RichLRR","DivLRR","DenLRR")])
M <- cor(cor.dat)
M.test <- cor.mtest(cor.dat)
corrplot::corrplot(M, p.mat = M.test$p)

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

ggpubr::ggarrange(bioLRR.bio,
                  bioLRR.ric,
                  ricLRR.bio,
                  ricLRR.ric,
                  divLRR.bio,
                  divLRR.ric)

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

# More complex models with two desscriptors
biolrr.mod0 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Treatment*Site, 
                  data = LRR_DEP_TEST_DATA %>% 
                    filter(Treatment != "h"))

# Alternative models
biolrr.mod1 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Treatment,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod2 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv*Site,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod3 <- lm(BioLRR ~ lcbv * Treatment*Site + lcrv,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod4 <- lm(BioLRR ~ lcbv * Treatment*Site,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod5 <- lm(BioLRR ~ lcbv * Treatment,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod6 <- lm(BioLRR ~ lcbv,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod7 <- lm(BioLRR ~ 1,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod8 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv*Treatment,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod9 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv*Site,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod10 <- lm(BioLRR ~ lcrv * Treatment*Site + lcbv,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod11 <- lm(BioLRR ~ lcrv * Treatment*Site,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod12 <- lm(BioLRR ~ lcrv * Treatment,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod13 <- lm(BioLRR ~ lcrv,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

biolrr.mod14 <- lm(BioLRR ~ 1,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))

# Model withouth treatments to comparison
biolrr.modNoTrt <- lm(BioLRR ~ lcbv * Site + lcrv * Site, 
                  data = LRR_DEP_TEST_DATA %>% 
                    filter(Treatment != "h"))


AIC(biolrr.mod0,
    biolrr.mod1,
    biolrr.mod2,
    biolrr.mod3,
    biolrr.mod4,
    biolrr.mod5,
    biolrr.mod6,
    biolrr.mod7,
    biolrr.mod8,
    biolrr.mod9,
    biolrr.mod10,
    biolrr.mod11,
    biolrr.mod12,
    biolrr.mod13,
    biolrr.mod14)

# anova(biolrr.mod5, biolrr.mod0) # This model is almost better

library(sjPlot)


p1 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Treatment", "Site"),
                 show.data = T)
p1.1 <- plot_model(biolrr.mod0, type="pred", terms = c("lcrv", "Treatment", "Site"),
                   show.data = T)
p2 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Site"))
p3 <- plot_model(biolrr.mod0, type="pred", terms = c("lcbv", "Treatment"))




# BIOMASS vs SPECIES RICHNESS ----
rb.cor.dat <- RICHNESS
rb.cor.dat$bio = BIOMASS$tot_bio

ggplot(rb.cor.dat, aes(x=sp_no, y=bio)) +
  geom_point(aes(colour = treatment)) +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') + 
  stat_smooth(method = "lm")

# control no relation
summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
             filter(treatment == "c")))
# Insecticide - YES
summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
             filter(treatment == "i")))
# Fungicide
summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
             filter(treatment == "f")))
# Herbivore YES
summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
             filter(treatment == "h")))
# Predator YES
summary(lm(log(bio) ~ log(sp_no), data = rb.cor.dat %>% 
             filter(treatment == "p")))



# Tests
