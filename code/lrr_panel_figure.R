library(ggplot2)
library(GGally)
# library(lme4)
# library(lmerTest)
# library(ggpmisc)
# library(tibble)


source('code/log_ratio_extraction.R')

# 0. Data preparation ----

# Data comes from the log_ratio_extraction.R file

# bio, dens, div, rich
panel_data <- data.frame()

dats <- list(biomass = BIOMASS.LR, 
             denssity = DENSITY.LR, 
             diversity = DIVERSITY.LR, 
             richness = RICHNESS.LR)

# Stack the data and rename descriptor names

for (file in names(dats)){
  print(file)
  dat <- dats[[file]]
  dat$descriptor <- ifelse(grepl("bio", file), "Biomass",
                           ifelse(grepl("dens", file), "Woody plant density",
                                  ifelse(grepl("div", file),"Diversity", "Richness")))
  dat <- dat[, c("site", "garden", "treatment", "lratio","descriptor")]
  panel_data <- rbind(dat, panel_data)
}

# Remove chlorkill treatment

pd <- panel_data %>% 
  filter(treatment != "ch")

# Make a renamed factor variable for the treatments

pd$ftreatment <- pd$treatment

# Fix treatment names
pd$ftreatment[pd$ftreatment == "i"] <- "Insects"
pd$ftreatment[pd$ftreatment == "p"] <- "Birds, bats and ants"
pd$ftreatment[pd$ftreatment == "f"] <- "Fungi"
pd$ftreatment[pd$ftreatment == "h"] <- "Increased herbivory"

pd$ftreatment <- factor(pd$ftreatment,
                           levels = c("Fungi",
                                      "Insects",
                                      "Birds, bats and ants",
                                      "Increased herbivory"
                                      ))

# Add elevation variable
pd$elev <- ifelse(pd$site == "wanang", 200, 
                  ifelse(pd$site == "numba", 750, 1900))

pd$elev2 <- pd$elev^2

# Set the transparency value
aph <- 0.5

# Create a factor variable for sites
pd$fsite <- factor(pd$site, labels = c("Wanang",
                                      "Numba",
                                      "Yawan"))

# 1. Figure1: The panel ----

# tiff(filename='figures/lrr_panel_woody.tif',
#      height=5600,
#      width=5200,
#      units='px',
#      res=800,compression='lzw')

ggplot(pd, aes(x = fsite, y = lratio))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 2)+
  
  # Group means
  stat_summary(fun = mean, na.rm = T,
               geom = "point", size = 2,
               alpha = aph)+
  
  stat_summary(fun.data = "mean_cl_boot",
               alpha = aph) + 
  
  # Annotations
  geom_text(data = pd %>%
              group_by(ftreatment, fsite) %>%
              summarise(mlr = mean(lratio)),
            aes(x = fsite, y = mlr + 1.5, group = ftreatment,
                label = lrr_panel_labs),
            size = 3)+
  
  theme_bw()+
  theme(strip.text = element_text(size =5.5),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  geom_hline(yintercept = 0, lty = 2)+
  ylab("Log response ratio")+xlab("")+
  facet_grid(descriptor ~ ftreatment)

# dev.off()

# 2. LRR correlations ----

ftreats <- unique(pd$ftreatment)

fdat <- pd %>%
  filter(ftreatment == ftreats[1])%>%
  arrange(descriptor, site, garden)

hdat <- pd %>%
  filter(ftreatment == ftreats[2])%>%
  arrange(descriptor, site, garden)

idat <- pd %>%
  filter(ftreatment == ftreats[3])%>%
  arrange(descriptor, site, garden)

pdat <- pd %>%
  filter(ftreatment == ftreats[4])%>%
  arrange(descriptor, site, garden)

chdat <- pd %>%
  filter(ftreatment == ftreats[5])%>%
  arrange(descriptor, site, garden)

# Create one dataset
pdpairs <- pd %>%
  group_by(descriptor, site, garden) %>%
  summarise() # Summarise based on what??????

# Add columns with LRRs
pdpairs$f <- fdat$lratio
pdpairs$h <- hdat$lratio
pdpairs$i <- idat$lratio
pdpairs$p <- pdat$lratio

# Is this necessary?
# source("code/data_processing.R")

# mainData <- read.csv("data/main_data_v2.csv")

biolrr <- pdpairs %>% filter(descriptor == "Biomass")

biocont <- BIOMASS  %>% filter(treatment == "c") %>%
  arrange(as.character(site), garden)

# lowerfun2 <- function(data,mapping){
#   ggplot(data = data, mapping = mapping)+
#     geom_point(alpha = 0.9, size=2)+
#     geom_smooth(method = "lm", alpha = 0.1)
# }  

# cordat <- as.data.frame(cbind(biocont, biolrr))

# cordat$lbiomass <- log(cordat$biomass)

#   ggpairs(cordat[,c("lbiomass", "f","i","p","h")], 
#         aes(colour = cordat$site...1, shape = cordat$site...1), 
#         lower = list(continuous = wrap(lowerfun2)))+
#   theme_bw() + ggtitle("Woody plant density")

# lrr correlation data for a panel plot----
cpdata <- data.frame(descriptor = rep(pdpairs$descriptor,2),
                     site = rep(pdpairs$site,2),
                     garden = rep(pdpairs$garden),
                     x = c(pdpairs$h,
                           pdpairs$p),
                     y = rep(pdpairs$i, 2),
                     response = rep(c("Elevated herbivory effect",
                                      "Top predators effect"), 
                                    each = length(pdpairs$descriptor))) 

# pdp <- data.frame(desc = pdpairs$descriptor,
#                   site = pdpairs$site,
#                   gard = pdpairs$garden,
#                   ins.lrr = pdpairs$i,
#                   herb.lrr = pdpairs$h,
#                   pred.lrr = pdpairs$p)
                  

# pdp %>% select(-desc, -site, -gard) %>% ggpairs(aes(color = pdp$site))

lrr.cor.test.data <- c()

for (resp in unique(cpdata$response))   {
  for (desc in unique(cpdata$descriptor)){
    for (loc in unique(cpdata$site))     {
      
      print(paste(resp, desc,loc))
      
      lrrtd <- cpdata %>%
        filter(response == resp,
               descriptor == desc,
               site == loc)
      
      # np.test.p <- cor.test(lrrtd$x, lrrtd$y, method = c('pearson'))$p.value
      # Test for noramlity
      print(shapiro.test(lrrtd$x))
      print(shapiro.test(lrrtd$y))
      
      lm.test.p <- summary(lm(lrrtd$y~lrrtd$x, data = lrrtd))$coefficients[2,4]
      
      print(lm.test.p)
      
      lrr.res <- data.frame(response = resp,
                            descriptor = desc,
                            site = loc,
                            non.param.p = ifelse(lm.test.p<0.05,"signif","ns"))
      lrr.cor.test.data <- rbind(lrr.cor.test.data, lrr.res)
      
    }
  }
}

lrr.cor.test.data -> lctd

cpdata$sign <- ""

sign.dat <- lctd[lctd$non.param.p == "signif",]

for(row in 1:dim(sign.dat)[1]){
  resp <- sign.dat[row, ]$response
  desc <- sign.dat[row, ]$descriptor
  loc <- sign.dat[row, ]$site
  
  c1 <- cpdata$response == resp
  c2 <- cpdata$descriptor == desc
  c3 <- cpdata$site == loc
  
  cpdata[c1 & c2 & c3, ]$sign <- "signif"
}

# tiff(filename='figures/corrplots/lrr_correlation_panel_woody.tif',
#      height=10600,
#      width=8200,
#      units='px',
#      res=800,compression='lzw')
ggplot(cpdata, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.4)+
  stat_smooth(data = cpdata %>%
                filter(sign == "signif"),
              method ="lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  ylab("Effect size of insects")+
  xlab("Effect size")+
  theme_bw()+
  facet_grid(response*descriptor~site)

# dev.off()