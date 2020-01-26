

check_stationarity = function(data, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  plot(data, type = "l", main = title, xlab = xlab, ylab = ylab)
  len = length(data)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  acf(data[1:len], main = "Full Dataset")
  acf(data[1:len_by_2], main = "First Half ACF")
  acf(data[seg_2_start:len], main = "Second Half ACF")
}


calculate_ts_gamma0 = function(x){
  n = length(x)
  gamma0 = var(x)*(n-1)/n
  return(gamma0)
}

calculate_ts_mean = function(x){
  return(mean(x))
}

calculate_ts_var_of_mean = function(x){
  n=length(x) 
  nlag=n-1 
  m=mean(x)
  v=var(x,na.rm = TRUE)
  gamma0=calculate_ts_gamma0(x)
  aut=acf(x,lag.max=nlag) 
  sum=0
  for (k in 1:nlag) {
    sum=sum+(1-k/n)*aut$acf[k+1]*gamma0
  }
  
  vxbar=2*sum/n + gamma0/n 
  return(vxbar)  
}

calculate_ts_mean_confidence_interval = function(x, alpha = 0.05){
  xbar = calculate_ts_mean(x)
  vxbar = calculate_ts_var_of_mean(x)
  multiplier = qnorm(1 - alpha/2)
  ci = c(xbar - multiplier * sqrt(vxbar), xbar +  multiplier * sqrt(vxbar))
  return(ci)
}

calculate_ar1_varx = function(phi, vara=1){
  # Computes SigmaX^2 = vara/(1-phi^2)
  # actually, we can use calculate_arp_varx for this directly since rho1 = phi1^k where k = 1
  return (vara/(1 - phi^2))
} 

calculate_arp_varx = function(phi, p, vara = 1){
  # Computes SigmaX^2 = vara/(1-phi1*rho1 - phi2*rho2 - ...)
  sum = 0
  for (i in 1:length(phi)){
    sum = sum + phi[i] * p$aut1[i+1]
  }
  
  return (vara / (1 - sum))
}