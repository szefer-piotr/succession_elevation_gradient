all.pairs_max_accuracy <- read.csv("data/raup_crick/all.pairs_max_accuracy.csv")
all.pairs_max_accuracy$fsite <- factor(all.pairs_max_accuracy$fsite, 
                                       levels = c("w","n","y"), labels = c("Wanang",
                                                                           "Numba",
                                                                           "Yawan"))

test.df <- data.frame()

all.pairs_max_accuracy$fsite <- factor(all.pairs_max_accuracy$fsite, levels = c("Wanang",
                                                                                "Numba","Yawan"))

for (st in unique(all.pairs_max_accuracy$fsite)){
  for (cp in unique(all.pairs_max_accuracy$comp)){
    
    print(paste(st, cp))
    
    subdat <- all.pairs_max_accuracy %>%
      filter(fsite == st & comp == cp)
    
    mod <- nlme::lme(RC ~ treatment,
                     random = ~1|comp.id,
                     data = subdat)
    
    # print(t.test(RC ~ treatment, data = subdat, paired = TRUE))
    
    test.df <- rbind(test.df, data.frame(fsite = st,
                                         comp = cp,
                                         group1 = tolower(substr(cp,1,1)),
                                         group2 = tolower(substr(cp,6,6)),
                                         test.res = summary(mod)$tTable[2,5]))
    
    # print(summary(mod))
    
  }
}

test.df <- test.df %>%
  mutate(label = ifelse(test.res < 0.05, "*"," "))
test.df$fsite <- factor(test.df$fsite, levels = c("Wanang",
                                                  "Numba",
                                                  "Yawan"))

rcplot <- ggplot(all.pairs_max_accuracy, aes(x = treatment, y = RC))+
  # geom_boxplot()+
  ylim(c(-1.25,1.5))+
  stat_summary(fun.y = mean, na.rm = T,
               geom = "point", size = 4)+
  stat_summary(fun.data = "mean_cl_boot")+
  theme_bw()+
  ggtitle(paste("Accuracy = 0.001"))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_hline(yintercept = 1, lty = 3)+
  geom_hline(yintercept = -1, lty = 3)+
  facet_grid(vars(fsite), vars(comp), scales = "free")+
  ggpubr::stat_pvalue_manual(data = test.df[test.df$label == "*",], label = "label",
                             y.position = 1.25, 
                             label.size = 6)