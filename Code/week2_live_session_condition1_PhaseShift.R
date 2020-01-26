#dev.off()
set.seed(107)
num_realizations = 10
xs = seq(0,(2*pi),length = 100)

realization_holder = matrix(nrow = num_realizations,ncol = 100) # this holds all 100 time periods for the realizations

# Generate Realizations
for( i in 1 : num_realizations)
{
  #phase_shift = 0 # Time series mean changes with time since each realization has same outcome (no phase shift)
  phase_shift = runif(1,0,(2*pi))  # Time Series mean does not change with time
  ys = sin(xs + phase_shift)
  realization_holder[i,] = ys
}

par(mfrow = c(2,1))


plot(realization_holder[1,])

for(k in 2:num_realizations)
{
  points(realization_holder[k,], col = "blue")
}

# Compute means at each time point (across realization) and plot
means_RH = colMeans(realization_holder)
plot(means_RH,ylim = c(-1,1))


