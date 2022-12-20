# This file is obsolete. Use only for the test

# This may be used to produce labels for the log-ratio comparison figure.

# rm(list = ls())
# source("code/log_ratio_extraction.R")

library(ggplot2)
library(emmeans)
library(ggpubr)
library(nlme)
library(car)
library(multcompView)


# Statistical test

# 1) Compare relative strengths of individual factors at each site

# 1.1) On raw biomass data glmm with garden as a random factor (in different script!)

# 2) strength changing with elevation
lrdata <- BIOMASS.LR
treatment_code <- "f"



# runTest <- function(lrdata, treatment_code = "f"){
#   # Specific wrapper around testing procedure
#   
#   # Filter data
#   trtdata  <- lrdata %>%
#     filter(treatment == treatment_code)
#   
#   # Calculate model
#   # No intercept model
#   trtlmw <- lm(lratio ~ 1, data = trtdata %>% filter(site == "wanang"))
#   trtlmn <- lm(lratio ~ 1, data = trtdata %>% filter(site == "numba"))
#   trtlmy <- lm(lratio ~ 1, data = trtdata %>% filter(site == "yawan"))
#   
#   # Tukey post-hoc
#   # emm <- emmeans(trtlm, "site", correction = "tukey")
#   
#   return(c())
#   
# }

# 2.1) Tests for individual treatments

# Biomasss
f.test <- runTest(BIOMASS.LR, treatment_code = "f")
# ch.test <- runTest(BIOMASS.LR, treatment_code = "ch")
i.test <- runTest(BIOMASS.LR, treatment_code = "i")
p.test <- runTest(BIOMASS.LR, treatment_code = "p")
h.test <- runTest(BIOMASS.LR, treatment_code = "h")



biotest_labels <- c(f.test, i.test, p.test, h.test)
# biotest_labels <- c("","","","a*","b*","a","a*","a" ,"","","","a*","b","b")

# Diversity
f.test <- runTest(DIVERSITY.LR, treatment_code = "f")
# ch.test <- runTest(DIVERSITY.LR, treatment_code = "ch")
i.test <- runTest(DIVERSITY.LR, treatment_code = "i")
p.test <- runTest(DIVERSITY.LR, treatment_code = "p")
h.test <- runTest(DIVERSITY.LR, treatment_code = "h")


divtest_labels <- c(f.test, i.test, p.test, h.test)
# c("","","","","","a","a","a*" ,"a","a","a*","","","")

# Richness
f.test <- runTest(RICHNESS.LR, treatment_code = "f")
# ch.test <- runTest(RICHNESS.LR, treatment_code = "ch")
i.test <- runTest(RICHNESS.LR, treatment_code = "i")
p.test <- runTest(RICHNESS.LR, treatment_code = "p")
h.test <- runTest(RICHNESS.LR, treatment_code = "h")

richtest_labels <- c(f.test, i.test, p.test, h.test)


# Density
# f.test <- runTest(DENSITY.LR, treatment_code = "f") # unequal variances
f.test <- c(" a*", " a",  " a" )
# ch.test <- runTest(DENSITY.LR, treatment_code = "ch")
i.test <- runTest(DENSITY.LR, treatment_code = "i")
p.test <- runTest(DENSITY.LR, treatment_code = "p")
h.test <- runTest(DENSITY.LR, treatment_code = "h")

denstest_labels <- c(f.test, i.test, p.test, h.test)

lrr_panel_labs <- c(biotest_labels,
                    divtest_labels,
                    richtest_labels,
                    denstest_labels)

# Figures

# Change the labels on top of each box
# lrdata$treatment <- as.character(lrdata$treatment)
# lrdata$treatment <- as.factor(lrdata$treatment)
# lrdata$treatment <- factor(lrdata$treatment,
# labels = c("Below-ground herbivory",
#            "Fungicide",
#            "Increased herbivory",
#            "Insecticide",
#            "Bird and ants exclusion"))

# Log effect ratios of total biomass (excluding Musa sp.)
# 
# fig1lab <- c("a*",
#              "b",
#              "a",
#              "a",
#              "a",
#              "a*",
#              "b",
#              "b",
#              "a",
#              "a*",
#              "a",
#              "a",
#              "a",
#              "a")






# fig1dat <- lrdata[lrdata$treatment != "Below-ground herbivory", ]
# fig1dat <- lrdata













# tiff(filename='figures/fig1.tif',
#      height=5600,
#      width=5200,
#      units='px',
#      res=800,compression='lzw')

# ggplot(fig1dat, aes(y = lratio, x = site)) +
#   geom_point(position = position_jitter(width = 0.1),
#              alpha = 0.1, size = 2)+
#   stat_summary(fun.y = mean, na.rm = T,
#                geom = "point", size = 4)+
#   stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = treatment),
#                geom = "line")+
# 
#   geom_text(data = fig1dat %>%
#               group_by(treatment, site) %>%
#               summarise(mlr = mean(lratio)),
#             aes(x = site, y = mlr + 1, group = treatment,
#                 label = fig1lab),
#             size = 5)+
# 
#   geom_hline(yintercept = 0, lty = 2)+
# 
#   facet_wrap(~treatment) +
# 
#   ylab("Logarithm of an Effect Ratio")+
#   xlab("Site/Elevation") +
#   theme_bw()

# dev.off()

# write.table(fig1dat, 'supplementary_files/bio_lrr.txt')

# Comparison of insect effects above-, below-ground and generalists


# tiff(filename='figures/fig2.tif',
#      height=5600/2,
#      width=5200,
#      units='px',
#      res=800,compression='lzw')

# fig2dat <- lrdata[lrdata$treatment %in% c("Below-ground herbivory",
#                                           "Insecticide",
#                                           "Increased herbivory"), ]

# fig2lab <- c("a*", #IH 
#              "a",   #I
#              "a*",
#              "b",  #IH
#              "a*",   #I
#              "b*",
#              "b",  #IH
#              "a")   #I
             
# ggplot(fig2dat, aes(y = lratio, x = site)) + 
#   geom_point(position = position_jitter(width = 0.1),
#              alpha = 0.1, size = 2)+
#   stat_summary(fun.y = mean, na.rm = T,
#                geom = "point", size = 4)+
#   stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = treatment),
#                geom = "line")+
#   
#   geom_text(data = fig2dat %>% 
#               group_by(site, treatment) %>%
#               summarise(mlr = mean(lratio)), 
#             aes(x = site, y = mlr + 0.7, group = treatment, 
#                 label = fig2lab), 
#             size = 5)+ 
#   
#   geom_hline(yintercept = 0, lty = 2)+
#   facet_wrap(~treatment) +
#   ylab("Logarithm of an Effect Ratio")+
#   xlab("Site/Elevation") + 
#   theme_bw()
  

# dev.off()


# Relative strength of the effects within the elevation ----

# On biomass ----
# ggplot(fig1dat, aes(y = lratio, x = treatment)) +
#   geom_point(position = position_jitter(width = 0.1),
#              alpha = 0.1, size = 2)+
#   stat_summary(fun.y = mean, na.rm = T,
#                geom = "point", size = 4)+
#   stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = site),
#                geom = "line")+
#   
#   # geom_text(data = fig1dat %>%
#   #             group_by(treatment, site) %>%
#   #             summarise(mlr = mean(lratio)),
#   #           aes(x = site, y = mlr + 1, group = treatment,
#   #               label = fig1lab),
#   #           size = 5)+
#   # 
#   geom_hline(yintercept = 0, lty = 2)+
#   
#   facet_wrap(~site) +
#   
#   ylab("Logarithm of an Effect Ratio on total biomass")+
#   xlab("Site/Elevation") +
#   theme_bw()

# Tests

# for(loc in unique(fig1dat$site)){
#   print(loc)
#   
#   test.d <- fig1dat %>%
#     filter(site == loc)
#   
#   model <- lm(lratio ~ 0 + treatment, data = test.d)
#   summary(model)
#   
#   print(emmeans(model, pairwise ~ treatment))
#   
# }

# On density ----
# treat.to.test <- c("f",
#                    "h",
#                    "i",
#                    "p")
# DENSITY.LR.sel <- DENSITY.LR %>%
#   filter(treatment %in% treat.to.test)
#   
# ggplot(DENSITY.LR.sel, aes(y = lratio, x = treatment)) +
#   geom_point(position = position_jitter(width = 0.1),
#              alpha = 0.1, size = 2)+
#   stat_summary(fun.y = mean, na.rm = T,
#                geom = "point", size = 4)+
#   stat_summary(fun.data = "mean_cl_boot")+
#   stat_summary(fun.y = mean, aes(group = site),
#                geom = "line")+
#   
#   # geom_text(data = fig1dat %>%
#   #             group_by(treatment, site) %>%
#   #             summarise(mlr = mean(lratio)),
#   #           aes(x = site, y = mlr + 1, group = treatment,
#   #               label = fig1lab),
#   #           size = 5)+
#   # 
#   geom_hline(yintercept = 0, lty = 2)+
#   
#   facet_wrap(~site) +
#   
#   ylab("Logarithm of an Effect Ratio on total biomass")+
#   xlab("Site/Elevation") +
#   theme_bw()

# Tests

# for(loc in unique(DENSITY.LR.sel$site)){
#   print(loc)
#   
#   test.d <- DENSITY.LR.sel %>%
#     filter(site == loc)
#   
#   model <- lm(lratio ~ 0 + treatment, data = test.d)
#   summary(model)
#   
#   print(emmeans(model, pairwise ~ treatment))
#   
# }



# Lrr correlation

# lrrcor <- fig1dat %>%
#   group_by(site, garden) %>%
#   summarise()
# lrrcor$fvals = fig1dat[fig1dat$treatment == "Fungicide", ]$lratio
# lrrcor$hvals = fig1dat[fig1dat$treatment == "Increased herbivory", ]$lratio
# lrrcor$ivals = fig1dat[fig1dat$treatment == "Insecticide", ]$lratio
# lrrcor$pvals = fig1dat[fig1dat$treatment == "Bird and ants exclusion", ]$lratio
# 
# pairs(lrrcor[lrrcor$site == "wanang", c(3:6)])
# 
# ggplot(lrrcor, aes(x = fvals, y = ivals))+
#   geom_point()+
#   geom_smooth(method="lm")+
#   theme_bw()+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = 0)+
#   facet_wrap(~site)
