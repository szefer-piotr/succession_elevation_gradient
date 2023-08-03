# RDA analysis
source('code/log_ratio_extraction.R')

library(vegan)
library(cowplot)
library(ggplot2)

# 1. Load the data
COMMDATA <- t(COMMDATA)
COMMVARS

# Select only i,c,p,f
COMM.SEL <- COMMDATA[grepl("i|c$|p|f|_h", rownames(COMMDATA)), ]
VARS.SEL <- COMMVARS[grepl("i|c$|p|f|_h", COMMVARS$plotid), ]

treats.to.consider <- c("i","c","h","f","p")

# Dummy-coding of the treatments
COMMVARS <- COMMVARS %>%
  mutate(f = ifelse(treatment == "f", 1, 0),
         h = ifelse(treatment == "h", 1, 0),
         p = ifelse(treatment == "p", 1, 0),
         c = ifelse(treatment == "c", 1, 0),
         i = ifelse(treatment == "i", 1, 0))

# RDA at each elevation

## 3.1 Analyses for each elevation
ord.list <- list()
plot.list <- list()

for(site.id in substr(unique(COMMVARS$site),1,1)){
  
  # print(site.id)
  
  SUB.VARS <- COMMVARS %>%
    filter(grepl(paste('^', site.id, sep = ""), site) & treatment %in% treats.to.consider)
  
  SUB.DATA <- COMMDATA[SUB.VARS$plotid, ]
  SUB.DATA <- decostand(SUB.DATA, method = 'hellinger')
  # Perform RDA and ANOVA and print the results
  rdamod <- rda(as.data.frame(SUB.DATA)~f+h+p+i+Condition(garden), 
                data = SUB.VARS)
  
  ord.list[[site.id]] <- rdamod
  
  h <- how(blocks = SUB.VARS$garden)
  
  # print(anova(rdamod, permutations = h, by = "terms"))
  
  # Goodness
  gdnsof <- goodness(rdamod, model = "CCA", proportional = T)
  gof2ax <- gdnsof[,1] + gdnsof[,2]
  cgof <- gof2ax[gof2ax > 0.2]
  cgof <- sort(cgof, decreasing = T)
  sign.names <- names(cgof[!is.na(cgof) & !is.infinite(cgof)])
  
  colors <- as.numeric(as.factor(SUB.VARS$treatment))
  
  # Alternative way to obtain these
  rda.dat <- list(sites = rdamod$CCA$wa[, c(1,2)],
                  species = rdamod$CCA$v[, c(1,2)],
                  biplot = rdamod$CCA$biplot[, c(1,2)])
  
  sites.dat <- as.data.frame(rda.dat$sites) %>%
    mutate(codes = rownames(rda.dat$sites),
           treats = ifelse(grepl("c", codes),"c",
                           ifelse(grepl("f", codes),"f", 
                                  ifelse(grepl("h", codes), "h", 
                                         ifelse(grepl("i", codes), "i","p")))),
           shapes = as.numeric(as.factor(treats)))
  
  species.dat <- as.data.frame(rda.dat$species) %>%
    mutate(names = rownames(as.data.frame(rda.dat$species)))
  
  arrow.dat <- as.data.frame(rda.dat$biplot) %>%
    mutate(treats = rownames(as.data.frame(rda.dat$biplot)))
  
  if(site.id == "y"){
    arr.col <- c("red","black","black","red")
    arr.lwd <- c(0.5,0.1,0.1,0.5)
    arr.lty <- c(1,2,2,1)
  } else {arr.col <- c("black","black","black","red")
  arr.lwd <- c(0.1,0.1,0.1,0.5)
  arr.lty <- c(2,2,2,1)
  }
  
  p1 <- ggplot(sites.dat,aes(x = RDA1, y = RDA2))+
    geom_point(aes(colour = treats), pch = sites.dat$shapes+14, size = 2) + 
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) + 
    geom_vline(xintercept = 0, lty = 2, alpha = 0.5) + 
    geom_segment(data = arrow.dat, aes(x = 0, y = 0, 
                                       xend = RDA1, yend = RDA2),
                 arrow = arrow(length=unit(0.30,"cm"), 
                               ends="last", 
                               type = "closed"),
                 alpha = 0.8, 
                 col = arr.col,
                 lwd = arr.lwd,
                 lty = arr.lty)+
    geom_text(data = arrow.dat, aes(x = RDA1*1.1, y = RDA2*1.1, 
                                    label = treats),
              cex = 5)+
    theme_minimal()
  
  # scalar = 2.5
  
  p2 <- ggplot(species.dat, aes(x = RDA1*1.1, y = RDA2*1.1)) + 
    geom_text(data = species.dat %>%
                filter(names %in% sign.names), 
              aes(label = names, alpha = 0.2))+
    geom_segment(data = arrow.dat, aes(x = 0, y = 0, 
                                       xend = RDA1, yend = RDA2),
                 arrow = arrow(length=unit(0.30,"cm"), 
                               ends="last", 
                               type = "closed"),
                 alpha = 0.8, 
                 col = arr.col,
                 lwd = arr.lwd,
                 lty = arr.lty)+
    geom_text(data = arrow.dat, aes(x = RDA1*1.1, y = RDA2*1.1, 
                                    label = treats))+
    theme_minimal()
  
  plot.list[[site.id]] <- list(sites.ord = as_grob(p1),
                               species.ord = as_grob(p2))
  
}

