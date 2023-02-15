# # My idea is to investigate using simple mechanistic model 
# # under which conditions we would be able to reproduce the results seen 
# # in the study.
# 
# # The most important aspects of the model is its structure and the experiment. 
# # - Parameters will be subjected to simulations to evaluate the parameter space.
# # - Parameters evaluated again after the top-predators are removed to see 
# #   where we are in the parameter space 
# # - Variation in parameters should be included.
# # - Individual-based model could be implemented as well to model behavioral changes.
# 
# # What is going to be the benchmark? 
# # Biomass ratios 
# # and variation in biomass
# 
# 
# 
# # Model for the diamond module is taken from McCann, Hastings and Huxel (1998) Nature.
# 
# library(deSolve)
# 
# # Parameters and variables
# # R - resource density
# # C1 is the density of the first consumer species (herbivores)
# # C2 density of the second consumer (spiders)
# # P density of top predator
# 
# # K - carrying capacity 
# # P0, H0 - half saturation densities of the resource and C1
# # x_i is mass specific metabolic rate of species i measured relative to the
# #     production-to-biomass ratio of the resource population 
# # y_i is ingestion rate per unit metabolic rate of species i
# # O_ij is a fraction indicating the preference of species i for consuming 
# #     resource j
# # r - growth rate of plants (equals to 1)
# 
# # I will model functional responses explicitly
# # Herbivores are only ones eating the plants
# # Herbivores are consumed by both top-predators (B for birds) and spiders (S)
# # Birds eat both herbivores and spiders.
# 
# fullModule <- function(t, state, parameters) {
#   with(as.list(c(state, parameters)),{
#     dP <-  r*P*(1-P/K)-(o_HP*x_H*y_H*P*H)/(P+P0) # C1 is the only consumer of the Resource
#     dH <- -x_H*H*(1-(o_HP*y_H*P)/(P+P0))-(o_SH*x_S*y_S*H*S)/(H+H0)-(o_BH*y_B*x_B*H*B)/(o_BH*H+(1-o_BH)*S+H0)
#     dS <- -x_S*S*(1-(o_SH*y_S*H)/(H+H0))-((1-o_BH)*y_B*x_B*S*B)/(o_BH*H+(1-o_BH)*S+H0)
#     dB <- -x_B*B*(1-(o_BH*y_B*H+(1-o_BH)*y_B*S)/(o_BH*H+(1-o_BH)*S+H0))
#     list(c(dP,dH,dS,dB))
#   })
# }
# 
# params <- c(r = 1         ,
#             K = 1         , # environmental 
#             o_BS = 0.7    , # preference of birds for spiders
#             o_BH = 0.3    , # preference of birds for herbivores
#             o_HP = 1      , # preference of herbivores for plants
#             x_H  = 0.4    ,
#             y_H  = 2.009  ,
#             x_B  = 0.1   ,
#             y_B  = 5      ,
#             P0   = 0.16129,
#             o_SH = 1      , # preference of spiders for herbivores
#             x_S  = 0.7    ,
#             y_S  = 3.5    ,
#             H0   = 0.9
#             ) 
# 
# state <- c(P = 100, H = 10, S = 5, B = 20)
# times <- seq(0,50, by = 0.1)
# 
# out <- ode(y = state, times = times, func = fullModule, parms = params)
# 
# matplot(out[,-1], type = "l")
# 
# # What parameters I should use?
# 
# 
# 
# # Create a search algorithm to scan the parameter space to get plausible results
# 
# 
# 
# 



library(deSolve)
# Define full model
fullModule <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dP <-  r*P*(1-P/K)-(o_HP*x_H*y_H*P*H)/(P+P0)
    dH <- -x_H*H*(1-(o_HP*y_H*P)/(P+P0))-(o_SH*x_S*y_S*H*S)/(H+H0)-(o_BH*y_B*x_B*H*B)/(o_BH*H+(1-o_BH)*S+H0)
    dS <- -x_S*S*(1-(o_SH*y_S*H)/(H+H0))-((1-o_BH)*y_B*x_B*S*B)/(o_BH*H+(1-o_BH)*S+H0)
    dB <- -x_B*B*(1-(o_BH*y_B*H+(1-o_BH)*y_B*S)/(o_BH*H+(1-o_BH)*S+H0))
    list(c(dP,dH,dS,dB))
  })
}

reducedModule <- function(t, state, parameters) {
  # This models the a simple food chain without the top predators
  with(as.list(c(state, parameters)),{
    dP <-  r*P*(1-P/K)-(o_HP*x_H*y_H*P*H)/(P+P0)
    dH <- -x_H*H*(1-(o_HP*y_H*P)/(P+P0))-(o_SH*x_S*y_S*H*S)/(H+H0)
    dS <- -x_S*S*(1-(o_SH*y_S*H)/(H+H0))
    list(c(dP,dH,dS))
  })
}

# mass specific metabolic rate
metabolic_rate_herbivores <- 0.1
metabolic_rate_birds <- 0.08
metabolic_rate_spiders <- 0.3

# ingestion rate per unit metabolic rate of species i
multipl <- 1
ingestion_rate_herbivores <- 1.5 * multipl
ingestion_rate_birds <- 5 * multipl
ingestion_rate_spiders <- 5 * multipl

half_sat_plants <- 0.2
half_sat_herbivores <- 0.5

params_fm <- c(r = 1         ,
            K = 1       , # environmental 
            
            
            o_BS = 0.7    , # preference of birds for spiders
            o_BH = 0.3    , # preference of birds for herbivores
            o_HP = 1      , # preference of herbivores for plants
            o_SH = 1      , # preference of spiders for herbivores
            
            
            x_H  = metabolic_rate_herbivores,
            x_B  = metabolic_rate_birds,
            x_S  = metabolic_rate_spiders,
            
            y_H  = ingestion_rate_herbivores  , 
            y_B  = ingestion_rate_birds  ,
            y_S  = ingestion_rate_spiders    , # ing.rates
            
            P0   = half_sat_plants,
            H0   = half_sat_herbivores) 

params_rm <- c(r = 1         ,
               K = 1       , # environmental 
            
            
               o_HP = 1      , # preference of herbivores for plants
               o_SH = 1      , # preference of spiders for herbivores
            
            
               x_H  = metabolic_rate_herbivores,
               x_S  = metabolic_rate_spiders,
            
               y_H  = ingestion_rate_herbivores  , 
               y_B  = ingestion_rate_birds  ,
               y_S  = ingestion_rate_spiders ,
               
               P0   = half_sat_plants,
               H0   = half_sat_herbivores) 


stateFM <- c(P = 0.5, H = 0.5, S = 0.5, B = 0.5)
stateRM <- c(P = 0.5, H = 0.5, S = 0.5)
times <- seq(0,900, by = 0.1)

outFM <- ode(y = stateFM, times = times, func = fullModule,    parms = params_fm)
outRM <- ode(y = stateRM, times = times, func = reducedModule, parms = params_rm)

library(reshape2)
df.fm <- data.frame(time = 1:nrow(outFM), outFM[,-1])
df.fm.melt <- melt(df.fm, id = "time")
df.fm.melt$model <- "Full Model"

df.rm <- data.frame(time = 1:nrow(outRM), outRM[,-1])
df.rm.melt <- melt(df.rm, id = "time")
df.rm.melt$model <- "Reduced Model"

df.bm <- rbind(df.fm.melt, df.rm.melt)

library(ggplot2)
ggplot(df.bm, aes(x = time, y = value, color = variable))+
  geom_line()+
  facet_wrap(~model, scales = "free")



# Build the model-up starting with just plants add herbivores, spiders, and birds

# Step 1 plants

# mass specific metabolic rate
metabolic_rate_herbivores <- 1
metabolic_rate_birds <- 1
metabolic_rate_spiders <- 1

# ingestion rate per unit metabolic rate of species i
multipl <- 1
ingestion_rate_herbivores <- 1.7 * multipl
ingestion_rate_birds <- 5 * multipl
ingestion_rate_spiders <- 3 * multipl

half_sat_plants <- 0.51
half_sat_herbivores <- 0.5

params <- c(r = 0.1, K = 1, 
            o_BS = 0.7, o_BH = 0.3, o_HP = 1, o_SH = 1,
               x_H  = metabolic_rate_herbivores,
               x_B  = metabolic_rate_birds,
               x_S  = metabolic_rate_spiders,
               y_H  = ingestion_rate_herbivores, 
               y_B  = ingestion_rate_birds,
               y_S  = ingestion_rate_spiders,
               P0   = half_sat_plants,
               H0   = half_sat_herbivores) 

stateS1 <- c(P = 0.001, H = 0, S = 0, B = 0)
times <- seq(0,1000, by = 0.1)
outs1 <- ode(y = stateS1, times = times, func = fullModule, parms = params)

# That is a pretty good simulation of the successional growth
matplot(outs1[,-1], type = "l")

# Qualitative characteristics must be in place: i.e. herbivore biomass cannot exceed that of plants

# STEP 2 add. herbivores

#apply.equation for herb vs plants

stateS2 <- c(P = 0.1, H = 0.1, S = 0, B = 0)
outs2 <- ode(y = stateS2, times = times, func = fullModule, parms = params)
matplot(outs2[,-1], type = "l")

# Bifurcation function ----
# To get bifurcation I need to simulate by starting with the last initial point in teh 

# for a given trajectory scan the whole 
outs2[,2]

# Think about parameters ----
# What are average body masses of insects spiders and insectivorous birds and plants?



