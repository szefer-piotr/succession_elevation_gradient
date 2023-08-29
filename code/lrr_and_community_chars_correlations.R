# source('code/log_ratio_extraction.R')

# It needst to produce:
# final.biolrr.nl.mod, final.riclrr.nl.mod,final.divlrr.nl.mod, final.denlrr.nl.mod
# BioPredPlot, RicPredPlot, DivPredPlot, DenPredPlot,
# BioPredPlotR, RicPredPlotR, DivPredPlotR, DenPredPlotR
# r2data

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

LRR_DEP_TEST_DATA$RichLRR = RichLRR
LRR_DEP_TEST_DATA$ConRicVal = ConRicVal
LRR_DEP_TEST_DATA$DivLRR = DivLRR
LRR_DEP_TEST_DATA$ConDivVal = ConDivVal
LRR_DEP_TEST_DATA$DenLRR = DenLRR
LRR_DEP_TEST_DATA$ConDenVal = ConDenVal
LRR_DEP_TEST_DATA = LRR_DEP_TEST_DATA %>%
  mutate(SiteTreatment = paste(Site, Treatment, sep = " "))

library(ggplot2)
library(stringr)
library(sjPlot)
library(MASS)
library(mgcv)
library(tidyr)
library(caret)
library(MASS)

responses <- c("BioLRR","RichLRR","DivLRR","DenLRR")
predictors <- c("ConBioVal","ConRicVal","ConDivVal","ConDenVal")

# Calculate different bottom-up indicators: sum of all species within a garden
# And average biomass across all plots.
new_bu_vars <- as.data.frame(t(COMMDATA))
pivot_names <- colnames(new_bu_vars)
new_bu_vars$siteID <- rownames(new_bu_vars)

bu_vars <- pivot_longer(data = new_bu_vars,
             cols = all_of(pivot_names),
             values_to = "biomass",
             names_to = 'species')

bu_vars$site <- sapply(strsplit(bu_vars$siteID, '_'), '[', 1)
bu_vars$garden <- sapply(strsplit(bu_vars$siteID, '_'), '[', 2)
bu_vars$treatment <- sapply(strsplit(bu_vars$siteID, '_'), '[', 3)
bu_vars <- bu_vars%>%
  filter(biomass != 0)
bu_vars_aggr <- bu_vars %>% 
  group_by(site, garden) %>%
  summarise(sp_rich = length(unique(species)),
            mean_bio = sum(biomass)/length(unique(treatment))) %>%
  mutate(id = paste(site, garden, sep="_"))
  
# testbv <- bu_vars %>%
#   filter(site == "numba",
#          garden == "g1")
# sum(testbv$biomass)/length(unique(testbv$treatment))

LRR_DEP_TEST_DATA <- LRR_DEP_TEST_DATA %>% 
  mutate(id = paste(Site, sub(".","",Garden), sep = "_"))

LRR_DEP_TEST_DATA <- left_join(LRR_DEP_TEST_DATA, bu_vars_aggr, by = "id")

LRR_DEP_TEST_DATA$lcbv <- log(LRR_DEP_TEST_DATA$mean_bio)
LRR_DEP_TEST_DATA$lcrv <- log(LRR_DEP_TEST_DATA$sp_rich)

LRR_DEP_TEST_DATA$fSite <- factor(LRR_DEP_TEST_DATA$Site)
LRR_DEP_TEST_DATA$fTreatment <- factor(LRR_DEP_TEST_DATA$Treatment)

########################################################
###########   BIOMASS
########################################################
# More complex models with two descriptors ----
biolrr.nl.mod0 <- lm(BioLRR ~ lcbv * Treatment * Site +
                    I(lcbv^2) * Treatment*Site +
                    I(lcrv^2) * Treatment*Site +
                    lcrv*Treatment*Site,
                  data = LRR_DEP_TEST_DATA %>%
                    filter(Treatment != "h"))
final.biolrr.nl.mod <- stepAIC(biolrr.nl.mod0, scope = BioLRR ~ 1, trace = 0)
# summary(final.biolrr.nl.mod)

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
final.riclrr.nl.mod <- stepAIC(riclrr.nl.mod0, scope = RichLRR ~ 1, trace = 0)
# summary(final.riclrr.nl.mod)

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
final.denlrr.nl.mod <- stepAIC(denlrr.nl.mod0, scope = DenLRR ~ 1, trace = 0)
# summary(final.denlrr.nl.mod)

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
final.divlrr.nl.mod <- stepAIC(divlrr.nl.mod0, scope = DivLRR ~ 1, trace = 0)
# summary(final.divlrr.nl.mod)


#############################################################################
 
# Zamknij to w funkcji
mod.data = LRR_DEP_TEST_DATA %>%
  filter(Treatment != "h")

modCompAIC <- function(desc = "BioLRR"){
  
  full.form <- formula(paste(desc, "~ lcbv * Treatment * Site +
                       I(lcbv^2) * Treatment*Site +
                       I(lcrv^2) * Treatment*Site +
                       lcrv*Treatment*Site"))
  no.trt.form <- formula(paste(desc, "~ lcbv * Site +
                           I(lcbv^2) * Site +
                           I(lcrv^2) * Site +
                           lcrv * Site"))
  no.site.form <-  formula(paste(desc, "~ lcbv * Treatment +
                             I(lcbv^2) * Treatment +
                             I(lcrv^2) * Treatment +
                             lcrv*Treatment"))
  no.trt.site.form <- formula(paste(desc, "~ lcbv + 
                           I(lcbv^2) + 
                           I(lcrv^2) + 
                           lcrv"))
  trt.only.form <- formula(paste(desc, "~ Treatment"))
  site.only.form <- formula(paste(desc, "~ Site"))
  trt.site.form <- formula(paste(desc, "~ Treatment * Site"))
  
  full.mod         <- lm(full.form,                  data=mod.data)
  no.trt.mod       <- lm(no.trt.form,                data=mod.data)
  no.site.mod      <- lm(no.site.form,               data=mod.data)
  no.trt.site.mod  <- lm(no.trt.site.form,           data=mod.data)
  trt.only.mod     <- lm(trt.only.form,              data=mod.data)
  site.only.mod    <- lm(site.only.form,             data=mod.data)
  trt.site.mod     <- lm(trt.site.form,              data=mod.data)
  
  result_df <- data.frame(Formula = c(paste(full.form)[3],
                                      paste(no.trt.form)[3],
                                      paste(no.site.form)[3],
                                      paste(no.trt.site.form)[3],
                                      paste(trt.only.form)[3],
                                      paste(site.only.form)[3],
                                      paste(trt.site.form)[3]),
                          AIC = AIC(full.mod,
                                    no.trt.mod,
                                    no.site.mod,
                                    no.trt.site.mod,
                                    trt.only.mod,
                                    site.only.mod,
                                    trt.site.mod),
                          Rsquared = c(summary(full.mod)$r.squared,
                                       summary(no.trt.mod)$r.squared,
                                       summary(no.site.mod)$r.squared,
                                       summary(no.trt.site.mod)$r.squared,
                                       summary(trt.only.mod)$r.squared,
                                       summary(site.only.mod)$r.squared,
                                       summary(trt.site.mod)$r.squared),
                          AdjRsquared = c(summary(full.mod)$adj.r.squared,
                                          summary(no.trt.mod)$adj.r.squared,
                                          summary(no.site.mod)$adj.r.squared,
                                          summary(no.trt.site.mod)$adj.r.squared,
                                          summary(trt.only.mod)$adj.r.squared,
                                          summary(site.only.mod)$adj.r.squared,
                                          summary(trt.site.mod)$adj.r.squared))
  
  return(result_df)
  
}

biolrr.comp <- modCompAIC(desc = "BioLRR")
riclrr.comp <- modCompAIC(desc = "RichLRR")
divlrr.comp <- modCompAIC(desc = "DivLRR")
denlrr.comp <- modCompAIC(desc = "DenLRR")


############################################################################
# Diversity Lrr predictions and plot ----
############################################################################
model <- final.divlrr.nl.mod
pred.val <- 'lcbv'
data <- LRR_DEP_TEST_DATA %>%
  filter(Treatment != "h")

sites = c("wanang","numba","yawan")
treatments = c("i", "f","p")
grain = 1000

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


# newdata$lcrv <- ifelse(newdata$Site == "wanang",
#                        mean(bu_vars_aggr[bu_vars_aggr$Site == "wanang", ]$lcrv, na.rm=T),
#                        ifelse(newdata$Site == "numba",
#                               mean(bu_vars_aggr[bu_vars_aggr$Site == "numba", ]$lcrv, na.rm=T),
#                               mean(bu_vars_aggr[bu_vars_aggr$Site == "yawan", ]$lcrv, na.rm=T)))

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
  ylab("LRR of diversity (Simpson's index)") #+ ylim(c(-10,3))

####################################################################
# Biomass LRR pediction and plot ----
model <- final.biolrr.nl.mod
# model <- biolrr.nl.mod0.gam

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

# newdata <- expand.grid(lcbv = pvv$lcbv,
#                        fSite = sites,
#                        fTreatment = treatments)
# 
# 
# newdata$lcrv <- ifelse(newdata$fSite == "wanang",
#                        mean(data[data$fSite == "wanang", ]$lcrv, na.rm=T),
#                        ifelse(newdata$fSite == "numba",
#                               mean(data[data$fSite == "numba", ]$lcrv, na.rm=T),
#                               mean(data[data$fSite == "yawan", ]$lcrv, na.rm=T)))

# Predict
pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
                      SE = pred.values$se.fit,
                      Site = newdata$Site,
                      Treatment = newdata$Treatment,
                      Richness = newdata$lcrv)

# pred.values <- predict(model, newdata = newdata, se.fit = TRUE)
# pred.df <- data.frame(pred.vals = pvv, Predicted = pred.values$fit,
#                       SE = pred.values$se.fit,
#                       Site = newdata$fSite,
#                       Treatment = newdata$fTreatment,
#                       Richness = newdata$lcrv)


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
  ylab('LRR of biomass') #+ ylim(c(-10,3))

# BioPredPlot <- ggplot(pred.df, aes(x = lcbv, y = Predicted, group = Site))+
#   geom_line(aes(color = Site), lwd = 1) +
#   geom_ribbon(aes(ymin = Predicted - SE,
#                   ymax = Predicted + SE),
#               alpha = 0.1,
#               fill = 'grey50', colour = NA)+
#   theme_bw() +
#   geom_point(data = data, aes(x = lcbv, y = BioLRR,
#                               color = Site, alpha = 0.5)) +
#   facet_grid(~Treatment) +
#   xlab('Log[Woody plant biomass]') +
#   ylab('LRR of biomass')



####################################################################
# Richness LRR pediction and plot ----
model <- final.riclrr.nl.mod

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
  ylab('LRR of species richness') ##+ ylim(c(-10,3))


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
  ylab('LRR of density') #+ ylim(c(-10,3))

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

###################################################################
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
  ylab('LRR of biomass') #+ ylim(c(-10,3))


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
  ylab('LRR of species richness') #+ ylim(c(-10,3))

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
  ylab('LRR of diversity') #+ ylim(c(-10,3))

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
  ylab('LRR of density') #+ ylim(c(-10,3))


# Do all this for sites and each treatment
library(mgcv)
r2data <- data.frame()

for (desc in c("BioLRR", "RichLRR", "DivLRR", "DenLRR")) {
  
  for (st in c("wanang","numba","yawan")) {
    
    for (treat in c("i","f","p")) {
      
      data <- LRR_DEP_TEST_DATA %>% 
        filter(Treatment == treat,
               Site == st)
      
      #LM
      form <- formula(paste(desc, "~ lcbv+I(lcbv^2)+lcrv+I(lcrv^2)"))
      mod <- lm(form, data = data)
      val <- summary(mod)$adj.r.square
      
      # print(form)
      # print(summary(mod))

      # GAM
      # form <- formula(paste(desc, "~ s(lcbv, k = 3) + s(lcrv, k = 3)"))
      # mod <- gam(form, data = data)
      # val <- summary(mod)$r.sq
        
      r2row <- data.frame(Descriptors = desc,
                          Site = st,
                          Treatement = treat,
                          AdjR2 = val)
      
      r2data <- rbind(r2data, r2row)
      
    }
    
  }
  
}

r2data$Site <- factor(r2data$Site, 
                      levels = c("wanang", "numba","yawan"))

#Visualizations
# Wanang, insects, biomass

# datanpd <- LRR_DEP_TEST_DATA %>% 
#   filter(Treatment == 'p',
#          Site == 'numba')
# 
# form <- formula(paste('DivLRR', "~ lcbv+I(lcbv^2)+lcrv+I(lcrv^2)"))
# mod <- lm(form, data = datawib)
# summary(mod)
# 
# # Predicted values and a 3d plot
# library(rgl)
# plot3d(x = datanpd$lcrv,
#        y = datanpd$lcbv,
#        z = datanpd$DivLRR, type= "s",
#        radius = .1)
# scatter3d(x = datanpd$lcrv,
#           y = datanpd$lcbv,
#           z = datanpd$DivLRR, 
#           grid = FALSE, fit = "smooth")
