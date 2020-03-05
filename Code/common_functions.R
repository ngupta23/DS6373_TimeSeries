

check_stationarity = function(data, title = "Time Series Plot", xlab = "Time", ylab = "Time Series Realization" ){
  plot(data, type = "l", main = title, xlab = xlab, ylab = ylab)
  plotts.wge(x)
  len = length(data)
  len_by_2 = round(len/2)
  seg_2_start = len_by_2+1
  acf(data[1:len], main = "Full Dataset")
  acf(data[1:len_by_2], main = "First Half ACF")
  acf(data[seg_2_start:len], main = "Second Half ACF")
}

calculate_ts_gamma = function(x, k = 0){
  lhs = lag(x, k) - mean(x)
  rhs = x - mean(x)
  gamma_k = sum(lhs * rhs, na.rm = TRUE)/length(x)
  return(gamma_k)
}

calculate_ts_rho = function(x, k = 0){
  gamma_k = calculate_ts_gamma(x, k)
  gamma_0 = calculate_ts_gamma(x, 0)
  rho_k = gamma_k/gamma_0
  return(rho_k)
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

calculate_arp_varx = function(phi, pt, vara = 1){
  # Computes SigmaX^2 = vara/(1-phi1*rho1 - phi2*rho2 - ...)
  sum = 0
  for (i in 1:length(phi)){
    sum = sum + phi[i] * pt$aut1[i+1]
  }
  
  return (vara / (1 - sum))
}

### Add to tswgewrapped
factor.wge.season = function(s){
  phi = c(rep(0,s-1), 1)
  cat("--------------------------------------------------\n")
  cat(paste0("Printing Factors for Seasonality 's' = ", s, "\n"))
  cat("--------------------------------------------------\n")
  factor.wge(phi = phi)
}



compute_a = function(x, phi, theta, index){
  
  ## Limit 1
  get_a_i_lesseq_p = function(index){
    all_a = rep(0,index)  # Return all 0s till the index
    return(all_a)
  }
  
  ## Limit 2
  get_a_i_lesseq_lenX = function(x, p, q, phi, theta, index){
    all_a = get_a_i_lesseq_p(p)  ## get all 0's till p
    
    ## Then compute the values one by one till needed
    limit = min(index, length(x))
    for (i in (p+1):limit){
      a = x[i] - sum(phi * x[(i-1):(i-p)]) + sum(theta * all_a[(i-1):(i-q)]) - (1 - sum(phi)) * mean(x)
      all_a = c(all_a, a)
    }
    
    return(all_a)
  }
  
  ## Limit 3
  get_a_i_more_lenX = function(x, p, q, phi, theta, index){
    n = length(x)
    all_a = get_a_i_lesseq_lenX(x, p, q, phi, theta, n)  ## Get all values till len(x)
    all_a = c(all_a, rep(0, (index-n)))  ## Then append 0s after that
    return(all_a)
  }
  
  
  p = length(phi)
  q = length(theta)
  
  all_a = c()
  
  if (index <= p){
    all_a = get_a_i_lesseq_p(index)
  }
  else if (index <= length(x)){
    all_a = get_a_i_lesseq_lenX(x, p, q, phi, theta, index)
  }
  else{
    all_a = get_a_i_more_lenX(x, p, q, phi, theta, index)
  }
  
  return(all_a)
}

compute_vara = function(all_a){
  subset = all_a[all_a != 0]
  len_subset = length(subset)
  vara = sum(subset^2)/length(subset)
  return(vara)
}

compute_stda = function(all_a){
  return(sqrt(compute_vara(all_a)))
}

return_all_a_calc = function(x, phi, theta, index){
  all_a = compute_a(x = x, phi = phi, theta = theta, index = index)
  vara = compute_vara(all_a)
  stda = compute_stda(all_a)
  return(list(all_a = all_a, vara = vara, stda = stda))
}

sliding_ase = function(x,
                       phi = 0, theta = 0, d = 0, s = 0,    # ARUMA arguments
                       linear = NA, freq = NA,              # Signal + Noise arguments
                       n.ahead = NA, batch_size = NA,       # Forecasting specific arguments
                       ...)                                 # max.p for signal + noise, lambda for ARUMA      
  {
  # Sliding CV ... batches are mutually exclusive
  
  n = length(x)
  
  if (is.na(batch_size)){
    warning("Batch Size has not been specified. Will assume a single batch")
    cat("\n")
    batch_size = n
  }
  
  if (is.na(n.ahead)){
    stop("Number of points to be used for forecasting has not been specified. Please specify n.ahead")
  }
  
  if (all(phi == 0) & all(theta == 0) & d == 0 & s == 0){
    if (is.na(linear) & is.na(freq)){
      stop("You have specified the arguments for neither an ARMA/ARUMA model or a Signal + Noise Model. Please specify at least one of these to continue")
    }
  }
  
  aruma = FALSE
  if (!(all(phi == 0) & all(theta == 0) & d == 0 & s == 0)){
    aruma = TRUE
  }
  else{
    # Signal + Noise model
  }
  
  start = 1
  num_batches = n-batch_size+1
  ASEs = numeric(num_batches)
  
  for (i in 0:(num_batches-1))
  {
    # Define the batch
    subset = x[start:(batch_size+i)]
    # Take last n.ahead observations from the batch and use that to compare with the forecast
    test_data = x[(batch_size+i-n.ahead+1):(batch_size+i)]
    
    # print(paste("i: ", i, "Start: ", start, " Stop: ", batch_size+i))
    # print(paste(" Test Start: ", (batch_size+i-n.ahead+1),  "Test End: ", (batch_size+i)))
    
    if (aruma){
      forecasts = fore.aruma.wge(x = subset, phi = phi, theta = theta, d = d, s = s,
                                 n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    else{
      forecasts = fore.sigplusnoise.wge(x = subset, linear = linear, freq = freq,
                                        n.ahead = n.ahead, lastn = TRUE, plot = FALSE, ...)
    }
    
    ASEs[i+1] = mean((test_data - forecasts$f)^2)
    start = start+1
  }
  
  return(ASEs)
}
