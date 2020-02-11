n = 48
seed = 101
vara = 0.0

# ## ARUMA with Seasonality only + ARMA noise
# aruma_seasonal_only = gen.aruma.wge(n = n, s = 12, sn = seed, vara = vara, plot = FALSE)
# arma_noise = gen.arma.wge(n = n, phi = 0.9, sn = seed, vara = 1, plot = FALSE)
# aruma_plus_noise = aruma_seasonal_only + arma_noise
# plotts.wge(aruma_plus_noise)

## ARUMA with ARMA Components and White noise
aruma_plus_arma_plus_noise = gen.aruma.wge(n = n, phi = 0.9, s = 12, vara = vara, sn = seed)
