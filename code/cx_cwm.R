# SLA values of individual plant species for each treatment.

# I am using the data.woody.brut data-set from the pgls_data_prep script. It has
# filtered woody palnt names, cleaned sla.d values and limited sla range.

# Define CWM function
cwmFun <- function(sla.d.m2kg1, tot_bio){
  # summarize function: CWM for sites
  return(mean(sla.d.m2kg1*(tot_bio/sum(tot_bio, na.rm = T)), na.rm = T))
}

# siteSLA <- data.woody.brut %>%
#   filter(unified_names %in% trait.dat[trait.dat$Life.form == "woody", ]$dsName) %>%
#   mutate(tot_bio = ifelse(is.infinite(tot_bio), NA, tot_bio),
#          sla.d.m2kg1 = ifelse(is.infinite(sla.d.m2kg1), NA, sla.d.m2kg1)) %>%
#   group_by(site, garden, treatment) %>%
#   summarise(cwm.sla = cwmFun(sla.d.m2kg1, n.tot_bio),
#             sp.rich = n())

siteSLA <- data.woody.brut %>%
  filter(treatment %in% c('i','c','p','h', 'f')) %>%
  mutate(tot_bio = ifelse(is.infinite(tot_bio), NA, tot_bio),
         sla.d.m2kg1 = ifelse(is.infinite(sla.d.m2kg1), NA, sla.d.m2kg1),
         water.cont = ifelse(is.infinite(water.cont), NA, water.cont)) %>%
  group_by(site, garden, treatment) %>%
  summarise(cwm.sla = cwmFun(sla.d.m2kg1, n.tot_bio),
            cwm.wc = cwmFun(water.cont, n.tot_bio),
            sp.rich = n(),
            total.bio = sum(n.tot_bio))

siteSLAdf <- as.data.frame(siteSLA)

rownames(siteSLAdf) <- paste(substr(siteSLA$site,1,1), 
                             siteSLA$garden, 
                             "_", 
                             siteSLA$treatment, sep = "")

# Plot of CWM vs species richness
require(scales)


p1.wc <- ggplot(siteSLAdf, aes(x = sp.rich, y = cwm.wc))+
  geom_point(aes(size = total.bio, color = treatment)) + 
  stat_smooth(method = "lm")+
  scale_x_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()



p1 <- ggplot(siteSLAdf, aes(x = sp.rich, y = cwm.sla))+
  geom_point(aes(size = total.bio, color = treatment)) + 
  stat_smooth(method = "lm")+
  scale_x_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()

p2 <- ggplot(siteSLAdf, aes(x = treatment, y = cwm.sla, fill = site))+
  geom_boxplot()+
  scale_y_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()

p3 <- ggplot(siteSLAdf, aes(x = site, y = cwm.sla, fill = treatment))+
  geom_boxplot()+
  scale_y_continuous(trans = 'log2',
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()








# 
# 
# p1.wc <- ggplot(siteSLAdf, aes(x = sp.rich, y = cwm.wc))+
#   geom_point(aes(size = total.bio, color = treatment)) + 
#   stat_smooth()
# 
# 
# p1 <- ggplot(siteSLAdf, aes(x = sp.rich, y = cwm.sla))+
#   geom_point(aes(size = total.bio, color = treatment)) + 
#   stat_smooth()
# 
# ggpubr::ggarrange(p1,p1.wc)
# p2 <- ggplot(siteSLAdf, aes(x = treatment, y = cwm.sla, fill = site))+
#   geom_boxplot()+
#   scale_y_continuous(trans = 'log2',
#                      breaks = trans_breaks("log2", function(x) 2^x),
#                      labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()
# 
# p3 <- ggplot(siteSLAdf, aes(x = site, y = cwm.sla, fill = treatment))+
#   geom_boxplot()+
#   scale_y_continuous(trans = 'log2',
#                      breaks = trans_breaks("log2", function(x) 2^x),
#                      labels = trans_format("log2", math_format(2^.x))) +  theme_minimal()