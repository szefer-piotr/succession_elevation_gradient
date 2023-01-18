# My idea is to investigate using simple mechanistic model 
# under which conditions we would be able to reproduce the results seen 
# in the study.

# The most important aspects of the model is its structure and the experiment. 
# - Parameters will be subjected to simulations to evaluate the parameter space.
# - Parameters evaluated again after the top-predators are removed to see 
#   where we are in the parameter space 
# - Variation in parameters should be included.
# - Individual-based model could be implemented as well to model behavioral changes.

# What is going to be the benchmark? 
# Biomass ratios 
# and variation in biomass



# Model for the diamond module is taken from McCann, Hastings and Huxel (1998) Nature.

library(deSolve)

# Parameters and variables
# R - resource density
# C1 is the density of the first consumer species (herbivores)
# C2 density of the second consumer (spiders)
# P density of top predator

# K - carrying capacity 
# P0, H0 - half saturation densities of the resource and C1
# x_i is mass specific metabolic rate of species i measured relative to the
#     production-to-biomass ratio of the resource population 
# y_i is ingestion rate per unit metabolic rate of species i
# O_ij is a fraction indicating the preference of species i for consuming 
#     resource j
# r - growth rate of plants (equals to 1)

# I will model functional responses explicitly
# Herbivores are only ones eating the plants
# Herbivores are consumed by both top-predators (B for birds) and spiders (S)
# Birds eat both herbivores and spiders.

fullModule <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dP <-  r*P*(1-P/K)-(o_HP*x_H*y_H*P*H)/(P+P0) # C1 is the only consumer of the Resource
    dH <- -x_H*H*(1-(o_HP*y_H*P)/(P+P0))-(o_SH*x_S*y_S*H*S)/(H+H0)-(o_BH*y_B*x_B*H*B)/(o_BH*H+(1-o_BH)*S+H0)
    dS <- -x_S*S*(1-(o_SH*y_S*H)/(H+H0))-((1-o_BH)*y_B*x_B*S*B)/(o_BH*H+(1-o_BH)*S+H0)
    dB <- -x_B*B*(1-(o_BH*y_B*H+(1-o_BH)*y_B*S)/(o_BH*H+(1-o_BH)*S+H0))
    list(c(dP,dH,dS,dB))
  })
}

params <- c(r = 1         ,
            K = 1         , # environmental 
            o_BS = 0.7    , # preference of birds for spiders
            o_BH = 0.3    , # preference of birds for herbivores
            o_HP = 1      , # preference of herbivores for plants
            x_H  = 0.4    ,
            y_H  = 2.009  ,
            x_B  = 0.1   ,
            y_B  = 5      ,
            P0   = 0.16129,
            o_SH = 1      , # preference of spiders for herbivores
            x_S  = 0.7    ,
            y_S  = 3.5    ,
            H0   = 0.9
            ) 

state <- c(P = 100, H = 10, S = 5, B = 20)
times <- seq(0,50, by = 0.1)

out <- ode(y = state, times = times, func = fullModule, parms = params)

matplot(out[,-1], type = "l")

# What parameters I should use?



# Create a search algorithm to scan the parameter space to get plausible results




