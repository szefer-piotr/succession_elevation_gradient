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

library(ggplot2)
ggplot(LRR_DEP_TEST_DATA %>%
         filter(Treatment == "i"), aes(BioLRR, ConRicVal)) + 
  geom_point()

ggplot(LRR_DEP_TEST_DATA %>%
         filter(Treatment == "i"), aes(BioLRR, ConDivVal)) + 
  geom_point()

ggplot(LRR_DEP_TEST_DATA %>%
         filter(Treatment == "i"), aes(BioLRR, ConDenVal)) + 
  geom_point()

ggplot(LRR_DEP_TEST_DATA %>%
         filter(Treatment == "i"), aes(BioLRR, log(ConBioVal))) + 
  geom_point()

# Tests