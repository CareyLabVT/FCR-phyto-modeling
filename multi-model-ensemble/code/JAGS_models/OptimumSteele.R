model{
  
  ##Data Model
  for(i in 1:length(chla)){
    
    chla[i] ~ dnorm(mu[i], tau_obs)T(0,)
    
  } #end data model
  
  ##Process Model
  
  for(i in 2:length(chla)){
    
    #Process model
    mu[i] = mu[i-1] + (mu[i-1] * R_growth * (((wtemp[i] - 1) / (Topt - 1)) *((40 - wtemp[i]) / (40 - Topt)) ^((40 - Topt) / (Topt - 1))) * ((swr[i]/I_S) * exp(1 - (swr[i]/I_S)))) - (mu[i-1] * R_resp * (1.08^(wtemp[i] - 20))) 
  
  } #end process model
  
  ##Set Initial Condition
  mu[1] ~ dnorm(mu1, 1)
  
  ##Priors
  tau_obs ~ dgamma(0.001, 0.001)
  muopt ~ dnorm(1, 1)T(0, )
  Topt ~ dnorm(10, 1)T(0,)
  I_S ~ dnorm(100, 10)T(0, )
  R_growth ~ dnorm(1, 1)T(0,)
  R_resp ~ dnorm(1, 1)T(0,)
  
}