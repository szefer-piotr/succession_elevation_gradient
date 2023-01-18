# G:/My Drive/PROJECTS/food_webs/dynamical_model.R

library(deSolve)

ConstResource <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dB <- -d1*B*B    + a2*I*B + a1*H*B
    dI <- -a2*B*I + -d2*I*I  +  a3*H*I
    dH <- -a1*B*H - a3*I*H - d3*H*H + a4*P*H
    dP <- 0
    list(c(dB,dI,dH,dP))
  })
}

ConstResourceNoP <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    # dB <- -d1*B*B    + a2*I*B + a1*H*B
    dI <- -d2*I*I  +  a3*H*I
    dH <- -a3*I*H - d3*H*H + a4*P*H
    dP <- 0
    list(c(dI,dH,dP))
  })
}

# LVGardenNoDet <- function(t, state, parameters){
#   with(as.list(c(state, parameters)),{
#     dB <- -d1*B*B    + a2*I*B + a1*H*B
#     dI <- -a2*B*I + -d2*I*I  +  a3*H*I
#     dH <- -a1*B*H - a3*I*H - d3*H*H + a4*P*H
#     dP <- g*P                 -a4*H*P - d4*P*P
#     list(c(dB,dI,dH,dP))
#   })
# }
# 
# LVGardenNoDetNoPred <- function(t, state, parameters){
#   with(as.list(c(state, parameters)),{
#     # dB <- -d1*B*B    + a2*I*B + a1*H*B
#     dI <-  + -d2*I*I  +  a3*H*I
#     dH <-  - a3*I*H - d3*H*H + a4*P*H
#     dP <- g*P        -a4*H*P - d4*P*P
#     list(c(dI,dH,dP))
#   })
# }

unival <- runif(8, min = 0.01, max = 0.5)

parameters <- c(a1=unival[1],a2=unival[2], # assimilation * feeding rates
                a3=unival[3],              # Ip assimilation * feeding rate
                a4=unival[4],              # Herbivore feeding rrate
                d1=unival[5]*0.5,          # Bird intraguild competition
                d2=unival[6]*0.5,          # Intermediate pred. intrag. comp
                d3=unival[7]*0.5,          # Herbivore igc
                d4=unival[8]*0.5,          # Plant itraguild competition
                g = 2)                     # Plant growth rate

parameters_np <- c( # assimilation * feeding rates
  a3=unival[3]*0.1,              # Ip assimilation * feeding rate
  a4=unival[4],              # Herbivore feeding rrate
  # Bird intraguild competition
  d2=unival[6]*0.5,          # Intermediate pred. intrag. comp
  d3=unival[7]*0.5,          # Herbivore igc
  d4=unival[8]*0.5,          # Plant itraguild competition 
  g = 2)       

# state <- c(B = 1,I = 1,H = 1,P = 10)
# state_np <- c(I = 1,H = 1,P = 10)
state_const <- c(B=1, I = 1, H = 1, P = 10)
state_const_nop <- c(I = 1, H = 1, P = 10)

times <- seq(0,10,by=0.1)

# out <- ode(y = state, 
#            times = times, 
#            func = LVGardenNoDet, 
#            parms = parameters)
# 
# out_np <- ode(y = state_np, 
#            times = times, 
#            func = LVGardenNoDetNoPred, 
#            parms = parameters_np)

out_const <- ode(y = state_const, 
                 times = times, 
                 func = ConstResource, 
                 parms = parameters)

out_const_nop <- ode(y = state_const_nop, 
                     times = times, 
                     func = ConstResourceNoP, 
                     parms = parameters_np)

# M1control <- out[dim(out)[1], ]
# M1exclosure <- out_np[dim(out_np)[1], ]
M1const <- out_const[dim(out_const)[1], ]
M1constnop <- out_const_nop[dim(out_const_nop)[1], ]

# matplot(out[,-1], type = "l")
# legend("topright", inset = 0.02, legend = c("B", "I","H","P"),
#        col = c(1,2,3,4), lty = c(1,2,3,4))

# matplot(out_np[,-1], type = "l")
# legend("topright", inset = 0.02, legend = c("I","H","P"),
#        col = c(1,2,3), lty = c(1,2,3))

matplot(out_const[,-1], type = "l")
legend("topright", inset = 0.02, legend = c("B", "I","H","P"),
       col = c(1,2,3,4), lty = c(1,2,3,4))
matplot(out_const_nop[,-1], type = "l")
legend("topright", inset = 0.02, legend = c("I","H","P"),
       col = c(1,2,3), lty = c(1,2,3))

# Omnivory
# https://www.jstor.org/stable/3450749?seq=1#metadata_info_tab_contents
# https://www.tandfonline.com/doi/full/10.1080/17513750801956313
# https://www.researchgate.net/publication/303379740_A_general_parameterized_mathematical_food_web_model_that_predicts_a_stable_green_world_in_the_terrestrial_ecosystem

# What would be an expected difference in parameter values between spiders and birds?

# Eigenvalues
J <- matrix(c(-1, 1, 1, 0,
              -1,-1, 1, 0,
              -1,-1,-1, 1,
              0, 0,-1,-1), byrow = T, nrow=4, ncol=4)

# All eigenvalues are complex and negative. Stable dynamics, cycles.
eigen(J)$values


# Discrete model with noise
# Journal of Biological Dynamics
# Vol. 2, No. 2, April 2008, 89–101
# Dynamics of a plant–herbivore model

# Stochastic plant-herbivore interaction model with Alle effect.
for (i in 1:times){
  P(i+1) = P(i) + (r*P(i) - a*P(i)*H(i)*dt + alpha1*P(i)*rnorm(1,0,1)*sqrt(dt))
  H(i+1) = H(i) + (r*P(i) - (c*a*P(i)*H(i) - mu*H(i))*dt + alpha2*H(i)*rnorm(1,0,1)*sqrt(dt))
}

updateStep<- function(P,H){
  uP = P + (r*P - a*P*H)*dt + alpha1*P*rnorm(1,0,1)*sqrt(dt)
  uH = H + (c*a*P*H - mu*H)*dt + alpha2*H*rnorm(1,0,1)*sqrt(dt)
  return(c(uP,uH))
}

parameters <- c(r = 0.25, 
                a = 0.025, 
                c = 0.25, 
                mu = 0.3,
                alpha1 = 0.05,
                alpha2 = 0.05,
                P = 40,
                H = 15,
                dt = 0.1)
END_TIME <- 150
results <- data.frame()

new_init_pop <- c(50,50)

attach(as.list(parameters))
for(t in seq(from=0, to=END_TIME, by=dt)){
  new_init_pop <- updateStep(P,H)
  P <- new_init_pop[1]
  H <- new_init_pop[2]
  results <- rbind(results, 
                   data.frame(time = t,
                              plant = new_init_pop[1],
                              herbi = new_init_pop[2]))
}

matplot(results[,-1], type = "l")
