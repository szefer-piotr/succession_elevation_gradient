# bio, dens, div, rich
panel_data <- data.frame()

dats <- list(biomass = BIOMASS.LR, 
             denssity = DENSITY.LR, 
             diversity = DIVERSITY.LR, 
             richness = RICHNESS.LR)

# Stack the data and rename descriptor names
for (file in names(dats)){
  # print(file)
  dat <- dats[[file]]
  dat$descriptor <- ifelse(grepl("bio", file), "Biomass",
                           ifelse(grepl("dens", file), "Woody plant density",
                                  ifelse(grepl("div", file),"Diversity", "Richness")))
  dat <- dat[, c("site", "garden", "treatment", "lratio","descriptor")]
  panel_data <- rbind(dat, panel_data)
}

# Make a renamed factor variable for the treatments

panel_data$ftreatment <- panel_data$treatment

# Fix treatment names
panel_data$ftreatment[panel_data$ftreatment == "i"] <- "Insects"
panel_data$ftreatment[panel_data$ftreatment == "p"] <- "Birds, bats and ants"
panel_data$ftreatment[panel_data$ftreatment == "f"] <- "Fungi"
panel_data$ftreatment[panel_data$ftreatment == "h"] <- "Increased herbivory"
panel_data$ftreatment <- factor(panel_data$ftreatment,
                                levels = c("Fungi",
                                           "Insects",
                                           "Birds, bats and ants",
                                           "Increased herbivory"
                                ))

# Set the transparency value
aph <- 0.5

# Create a factor variable for sites
panel_data$fsite <- factor(panel_data$site, labels = c("Low",
                                                       "Mid",
                                                       "High"))

# Display panel plot
lrr_panel <- ggplot(panel_data, aes(x = fsite, y = lratio))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 2)+
  
  # Group means
  stat_summary(fun = mean, na.rm = T,
               geom = "point", size = 2,
               alpha = aph)+
  
  stat_summary(fun.data = "mean_cl_boot",
               alpha = aph) + 
  
  # Annotations
  geom_text(data = panel_data %>%
              group_by(ftreatment, fsite) %>%
              summarise(mlr = mean(lratio)),
            aes(x = fsite, y = mlr + 1.5, group = ftreatment,
                label = lrr_panel_labs),
            size = 3)+
  
  theme_bw()+
  theme(strip.text = element_text(size =5.5),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  ylab("Log response ratio")+xlab("")+
  facet_grid(descriptor ~ ftreatment)