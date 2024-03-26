library(betapart)
library(lsmeans)
library(multcomp)

beta.df <- data.frame()

# siteid <- "^w"
# treat <- "_c\\b"

COMMDATA <- t(COMMDATA)

for(siteid in c("^w","^y","^n")){
  for(treat in c("_c\\b","_f","_h","_i","_p")){
    
    # Subset the data
    nms <- rownames(COMMDATA)
    sel.nms <- nms[grepl(siteid, nms) & grepl(treat, nms)]
    site.treat.comm <- COMMDATA[sel.nms,]
    
    # Calculate beta diversity
    subbeta <- beta.pair.abund(site.treat.comm)
    
    # Add to a data frame
    balanced.df <- data.frame(site = siteid,
                              treat = treat,
                              vals = as.numeric(subbeta$beta.bray.bal),
                              component = "balanced")
    
    gradient.df <- data.frame(site = siteid,
                              treat = treat,
                              vals = as.numeric(subbeta$beta.bray.gra),
                              component = "gradient")
    
    bray.df <- data.frame(site = siteid,
                          treat = treat,
                          vals = as.numeric(subbeta$beta.bray),
                          component = "bray")
    
    sub.df <- rbind(balanced.df, gradient.df, bray.df)
    
    # Merge the data-set
    beta.df <- rbind(beta.df,sub.df)
    
  }
}

# beta.df

# Fix the site order
beta.df$fsite <- factor(beta.df$site, levels = c("^w","^n","^y"),
                        labels = c("w","n","y"))

# Transform 0s to 0.001 and 1s to 0.999
beta.df$trans.vals = ifelse(beta.df$vals == 1, 0.999999,
                            ifelse(beta.df$vals == 0, 0.000001, beta.df$vals))

# Tests for individual facets
cld.dat = data.frame()
prws.comp <- list()

# CLD data for comparisons between elevations
for (comp in unique(beta.df$component)){
  for (trt in unique(beta.df$treat)){
    
    test.df <- beta.df %>%
      filter(treat == trt,
             component == comp)
    
    mod1 <- betareg::betareg(trans.vals ~ fsite, data = test.df)
    
    lsmod1 <- lsmeans(mod1,
                      pairwise ~ fsite,
                      adjust="tukey")
    
    groups <- cld(lsmod1, Letters = c("abcdefg"))
    
    y_loc = test.df %>%
      group_by(fsite) %>%
      summarise(y_loc = mean(trans.vals))
    
    sub.cld <- data.frame(component = comp,
                          treat = trt,
                          fsite = groups$fsite,
                          gr.label = groups$.group,
                          y_loc = y_loc$y_loc[order(groups$fsite)])
    
    prws.comp[[paste(comp,trt)]] <- lsmod1
    
    cld.dat = rbind(cld.dat, sub.cld)
    
  }
}

pairwise.comp.res <- data.frame()
for (comp in unique(beta.df$component)){
  
  for (ste in unique(beta.df$fsite)){
    # print(comp)
    # print(ste)
    
    test.df <- beta.df %>%
      filter(fsite == ste,
             component == comp)
    
    # print(test.df)
    
    mod1 <- betareg::betareg(trans.vals ~ treat, data = test.df)
    
    breg.table <- summary(mod1)$coefficients$mean
    breg.pvals <- breg.table[,4]
    
    y_loc = test.df %>%
      group_by(treat) %>%
      summarise(y_loc = mean(trans.vals))
    
    pcr.row <- data.frame(comp = comp,
                          fsite = ste,
                          component = comp,
                          treat = names(breg.pvals),
                          p.vals = breg.pvals,
                          y_loc = y_loc$y_loc,
                          y_loc_trt = y_loc$treat)
    
    pcr.row <- pcr.row %>%
      mutate(label = ifelse(p.vals <= 0.05 & treat != "(Intercept)", "*",""))
    
    pairwise.comp.res <- rbind(pairwise.comp.res,pcr.row)
    
  }
}

beta.part.plot1 <- ggplot(beta.df, aes(x = fsite, y = vals))+
  geom_jitter(width = 0.1, alpha = 0.2, shape = 16, size = 1.8)+
  stat_summary(fun.y = mean, na.rm = T,
               geom = "point", size = 3, col = "gray30")+
  stat_summary(fun.data = "mean_cl_boot", col = "gray30")+
  stat_summary(fun.y = mean, aes(group = treat),
               geom = "line", alpha = 0.8, col = "gray30", lty = 2)+
  geom_text(data = cld.dat,
            aes(x = fsite, y = y_loc + 0.5,
                label = gr.label),
            size = 3)+
  facet_grid(cols = vars(component), rows = vars(treat)) + 
  theme_bw()

# Plot for the component comparisons
pairwise.comp.res$fsite <- factor(pairwise.comp.res$fsite,
                                  levels = c("w","n","y"))


# 
# ifelse(site == "^w", 
#        "Wanang (200 m a.s.l.)",
#        ifelse(site == "^n", 
#               "Numba (750 m a.s.l.)",
#               "Yawan (1900 m a.s.l.)"))

beta.df.rf <- beta.df %>%
  mutate(fsite = factor(as.character(fsite), 
                        levels = c("w","n","y"),
                        labels = c("Wanang (200 m a.s.l.)",
                                   "Numba (750 m a.s.l.)",
                                   "Yawan (1900 m a.s.l.)")),
         treat = ifelse(treat == '_c\\b', 
                        "Control",
                        ifelse(treat == '_f', 
                               "Fungi",
                               ifelse(treat == '_h', "Herbivores",
                                      ifelse(treat == '_i', "Insecticide",'Predator')))),
         component = ifelse(component == 'balanced', "Species turnover (balanced component)",
                            ifelse(component == "gradient", "Dominance shifts (gradient component)",
                                   "Bray-Curtis dissimilarty")))

pairwise.comp.res.rf <- pairwise.comp.res %>%
  mutate(fsite = factor(as.character(fsite), 
                        levels = c("w","n","y"),
                        labels = c("Wanang (200 m a.s.l.)",
                                   "Numba (750 m a.s.l.)",
                                   "Yawan (1900 m a.s.l.)")),
         y_loc_trt = ifelse(y_loc_trt == '_c\\b', 
                        "Control",
                        ifelse(y_loc_trt == '_f', 
                               "Fungi",
                               ifelse(y_loc_trt == '_h', "Herbivores",
                                      ifelse(y_loc_trt == '_i', "Insecticide",'Predator')))),
         component = ifelse(component == 'balanced', "Species turnover",
                            ifelse(component == "gradient", "Dominance shifts",
                                   "Bray-Curtis Ind.")))

colors <- c("gray30", "gray","gray","red","gray",
            "gray30", "gray","gray","gray","gray",
            "gray30", "gray","red","red","gray",
            "gray30", "gray","gray","red","gray",
            "gray30", "gray","gray","red","red",
            "gray30", "gray","gray","gray","gray",
            "gray30", "gray","gray","gray","gray",
            "gray30", "gray","red","red","red",
            "gray30", "gray","red","red","red")

# Alternative boxplot
beta.part.plot2 <- ggplot(beta.df.rf, aes(x = treat, y = vals))+
  # geom_jitter(width = 0.1, alpha = 0.1, color = "navyblue")+
  stat_summary(fun.y = mean, na.rm = T,
               geom = "point", size = 5, col = colors)+
  stat_summary(fun.data = "mean_cl_boot", col = colors, lwd = 1.1)+
  # geom_text(data = pairwise.comp.res.rf,
  #           aes(x = y_loc_trt, y = y_loc + 0.15,
  #               label = label), cex = 6,  col = "red")+
  facet_grid(component ~ fsite, scales = 'free',
             labeller = label_wrap_gen(25)) + 
  theme_bw()+ theme(axis.text.x = element_text(angle=90, hjust =1 )) + 
  xlab("") + ylab("")

tiff('beta-fig.tiff', width = 4*480, height=4*480, res=300)
beta.part.plot2
dev.off()
