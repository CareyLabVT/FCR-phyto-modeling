model{
  
  ##Data Model
  for(i in 1:length(chla)){
    
    chla[i] ~ dnorm(mu[i], tau_obs)T(0,)
    
  } #end data model
  
  ##Process Model
  
  for(i in 2:length(chla)){
    
    #Process model
    mu[i] = mu[i-1] + (mu[i-1] * R_growth * (((wtemp[i] - 0) / (Topt - 0)) *((100 - wtemp[i]) / (100 - Topt)) ^((100 - Topt) / (Topt - 0))) * ((swr[i]/I_K) / (1 + (swr[i]/I_K))) * (DIN[i]/(DIN[i]+ksdin)) * (SRP[i]/(SRP[i]+kssrp))) - (mu[i-1] * R_resp * (1.08^(wtemp[i] - 20))) 
  
  } #end process model
  
  ##Set Initial Condition
  mu[1] ~ dnorm(mu1, 1)
  
  ##Priors
  tau_obs ~ dgamma(0.001, 0.001)
  Topt ~ dnorm(10, 1)T(0,)
  I_K ~ dnorm(100, 1/10^2)T(0, )
  ksdin ~ dnorm(1, 1)T(0,)
  kssrp ~ dnorm(1, 1)T(0,)
  R_growth ~ dnorm(1, 1)T(0,)
  R_resp ~ dnorm(1, 1)T(0,)
  
}