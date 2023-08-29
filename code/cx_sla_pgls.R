
subsetVars <- c("c", "i")
source('code/get_data_ready_for_the_sla_analyses.R')
source('code/model_definitions_pgls.R')
i.mod <- q.herb.q.water
i.dD <- deltaData

subsetVars <- c("c", "f")
source('code/get_data_ready_for_the_sla_analyses.R')
source('code/model_definitions_pgls.R')
f.mod <- just.water.linear.noint
f.dD <- deltaData

subsetVars <- c("c", "h")
source('code/get_data_ready_for_the_sla_analyses.R')
source('code/model_definitions_pgls.R')
h.mod <- just.water.linear
h.dD <- deltaData

subsetVars <- c("c", "p")
source('code/get_data_ready_for_the_sla_analyses.R')
source('code/model_definitions_pgls.R')
p.mod <- full.model
p.dD <- deltaData

source("code/predictAndVisualize_function.R")

# Predator --------------------------
pslap <- predictAndVisualize(p.mod, 
                             p.dD,
                             "lsla.d",
                             n = 100,
                             titlestring = "Predator - Specific Leaf Area",
                             ylabstring = "Log-response ratio of total biomass.",
                             xlabstring = "Log(SLA)")

psitep <- predictAndVisualize(p.mod, 
                              p.dD,
                              "fsite",
                              n = 100,
                              titlestring = "Predator - Site",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Sites")

plabup <- predictAndVisualize(p.mod, 
                              p.dD,
                              "labu",
                              n = 100,
                              titlestring = "Predator - Abundance",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Log(stem number)")
prichp <- predictAndVisualize(p.mod, 
                              p.dD,
                              "richness",
                              n = 100,
                              titlestring = "Predator - Richness",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Number od species")
plrrslap <- predictAndVisualize(p.mod, 
                                p.dD,
                                "lrrsla",
                                n = 100,
                                titlestring = "Predator - LRR SLA",
                                ylabstring = "Log-response ratio of total biomass.",
                                xlabstring = "LRR SLA")


# Insecticide-----------------------
ilabup <- predictAndVisualize(i.mod, 
                              i.dD,
                              "labu",
                              n = 100,
                              titlestring = "Insecticide - Abundance",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Log(stem number)")



# Extra herbivory-------------------
hsitep <- predictAndVisualize(h.mod, 
                              h.dD,
                              "fsite",
                              n = 100,
                              titlestring = "Herbivory - Site",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Sites")

hwaterp <- predictAndVisualize(h.mod, 
                               h.dD,
                               "water.cont",
                               n = 100,
                               titlestring = "Herbivory - Water Content",
                               ylabstring = "Log-response ratio of total biomass.",
                               xlabstring = "Water content")


hlabup <- predictAndVisualize(h.mod, 
                              h.dD,
                              "labu",
                              n = 100,
                              titlestring = "Herbivory - Abundance",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Log(stem number)")

hlrrslap <- predictAndVisualize(h.mod, 
                                h.dD,
                                "lrrsla",
                                n = 100,
                                titlestring = "Herbivory - LRR SLA",
                                ylabstring = "Log-response ratio of total biomass.",
                                xlabstring = "LRR SLA")

# Fungicide

fwaterp <- predictAndVisualize(f.mod, 
                               f.dD,
                               "water.cont",
                               n = 100,
                               titlestring = "Fungicide - Water Content",
                               ylabstring = "Log-response ratio of total biomass.",
                               xlabstring = "Water content")

fsitep <- predictAndVisualize(f.mod, 
                              f.dD,
                              "fsite",
                              n = 100,
                              titlestring = "Fungicide - Site",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Site")

frichp <- predictAndVisualize(f.mod, 
                              f.dD,
                              "richness",
                              n = 100,
                              titlestring = "Fungicide - Richness",
                              ylabstring = "Log-response ratio of total biomass.",
                              xlabstring = "Number of species per plot")