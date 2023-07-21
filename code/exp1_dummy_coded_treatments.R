source('code/log_ratio_extraction.R')

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

# Prepare model data - h exluded
moddata <- LRR_DEP_TEST_DATA %>% 
  filter(Treatment != "h") %>%
  mutate(Insects = ifelse(Treatment == "i", 1, 0),
         Fungi = ifelse(Treatment == "f", 1, 0),
         Predators = ifelse(Treatment == "p", 1, 0),
         LogBioC = log(ConBioVal),
         LogRicC = log(ConRicVal))

# Dummy code the treatments
moded

biolrr.nl.mod0 <- lm(BioLRR ~
                       LogBioC*Insects*Site +
                       LogBioC*Predators*Site + 
                       I(LogBioC^2)*Insects*Site + 
                       I(LogBioC^2)*Predators*Site + 
                       LogRicC*Insects*Site +
                       LogRicC*Predators*Site + 
                       I(LogRicC^2)*Insects*Site + 
                       I(LogRicC^2)*Predators*Site, 
                     data = moddata)

divlrr.nl.mod0 <- lm(DivLRR ~
                       LogBioC*Insects*Site +
                       LogBioC*Predators*Site +
                       I(LogBioC^2)*Insects*Site + 
                       I(LogBioC^2)*Predators*Site + 
                       LogRicC*Insects*Site +
                       LogRicC*Predators*Site + 
                       I(LogRicC^2)*Insects*Site + 
                       I(LogRicC^2)*Predators*Site, 
                     data = moddata)

library(MASS)
select.bio.mod <- stepAIC(biolrr.nl.mod0, scope = BioLRR ~ 1)
select.div.mod <- stepAIC(divlrr.nl.mod0, scope = DivLRR ~ 1)

# Predictions and plots ----
model <- select.bio.mod
pred.val <- 'LogBioC'
data <- moddata

sites = c("wanang","numba","yawan")
treatments = c("Insects", "Fungi","Predators")
grain = 100

# Build a data frame with varying pred.var variable and average value for the rest
pvv <- data[,pred.val] # pred.va. values
predictor <- seq(length = grain, from = min(pvv), to = max(pvv))

# Make a df for predictions
newdata <- expand.grid(LogBioC = pvv$LogBioC, 
                       Site = sites, 
                       Predators = c(1,0),
                       Fungi = c(1,0),
                       Insects = c(1,0))

newdata$LogRicC <- ifelse(newdata$Site == "wanang", 
                       mean(data[data$Site == "wanang", ]$LogRicC, na.rm=T),
                       ifelse(newdata$Site == "numba", 
                              mean(data[data$Site == "numba", ]$LogRicC, na.rm=T),
                              mean(data[data$Site == "yawan", ]$LogRicC, na.rm=T)))  


# Predict
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Predators = newdata$Predators,
                      Insects = newdata$Insects,
                      Fungi = newdata$Fungi,
                      Richness = newdata$LogRicC)

library(ggplot2)
ggplot(pred.df, aes(x = LogBioC, y = Predicted, group = Site))+
  geom_line(aes(color = Site), lwd = 1) + 
  geom_ribbon(aes(ymin = Predicted - SE,
                  ymax = Predicted + SE),
              alpha = 0.1,
              fill = 'grey50', colour = NA)+
  theme_bw() + 
  geom_point(data = data, aes(x = LogBioC, y = RichLRR, 
                              color = Site, alpha = 0.5)) +
  facet_grid(~Predators) + 
  xlab('Log[No. of woody plant species]') +
  ylab("LRR of diversity (Simpson's index)")



