library(deSolve)
library(spatialEco)


# Three species RM model
reducedRMM <- function(t, state, parameters) {
  # This models the a simple food chain without the top predators
  with(as.list(c(state, parameters)),{
    dP <-  r * P * (1 - P/K) - (a_H * P * H)/(P + P0) # Plant
    dH <- (e_H * a_H * P * H)/(P + P0) - (m_H * H) - (a_S * S * H)/(H + H0)
    dS <- (e_S * a_S * S * H)/(H + H0) - (m_S * S)
    list(c(dP,dH,dS))
  })
}


# Parameters
params <- c(r = 1,
            K = 2,
            P0 = 0.5,
            
            a_H = 1.8,
            m_H = 0.5,
            e_H = 0.5,
            H0 = 0.3,
            
            a_S = 1.15,
            m_S = 0.3,
            e_S = 0.5 
            )

times <- seq(0,1000, by = 0.01)

states <- c(P = 0.5,H = 0.5, S = 0.5)

out <- ode(y = states, 
           times = times, 
           func = reducedRMM, 
           parms = params)

matplot(log(out[,-1]), type = "l")

# Analyse the model
getPoints <- function(out, col = "P"){
  localmm <- local.min.max(out[(dim(out)[1]/2):(dim(out)[1]-dim(out)[1]*0.1), col], plot = F)[c(1,2)]
  lapply(localmm, function(x){x[10:length(x)]})
}

# For the a_S plot herbivore bifurcations
plotBifurcations <- function (aS.range = seq(0.5,3, by = 0.01)) {
  plot(x=aS.range, y = seq(0,1, length = length(aS.range)),  type = "n")
  point.list <- list()
  for(aS in aS.range){
    params["a_S"] <<- aS
    print(paste("Now taking a_S = ", aS))
    curr.ode <- ode(y = states, 
               times = times, 
               func = reducedRMM, 
               parms = params)
    pointsToPlot <- unlist(getPoints(curr.ode, col = "P"))
    points(x = rep(aS, length(pointsToPlot)), y = pointsToPlot, pch = 19)
    point.list[[paste(aS)]] <- pointsToPlot
  }
  return(point.list)
}

# Bifurcations for plants
aS_red_mod <- plotBifurcations()

# Define full Rozenzweig-Macarthur model
fullRMM <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dP <-  r * P * (1 - P/K) - (a_H * P * H)/(P + P0) # Plant
    dH <- (e_H * a_H * P * H)/(P + P0) - (m_H * H) - (a_S * S * H)/(H + H0) - (a_B * o_BH * H * B)/(o_BH * H + (1 - o_BH) * S + H0)
    dS <- (e_S * a_S * S * H)/(H + H0) - (m_S * S) - (a_B * (1 - o_BH) * S * B)/(o_BH * H + (1 - o_BH) * S + H0)
    dB <- -m_B*B + e_B * B * e_B * a_B * ((o_BH * a_B * H) + ((1 - o_BH) * S))/(o_BH * H + (1 - o_BH) * S + H0)
    list(c(dP,dH,dS,dB))
  })
}

params_full <- c()