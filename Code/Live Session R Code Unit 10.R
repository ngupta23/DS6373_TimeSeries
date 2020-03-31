# Time Series Unit 10 Live Session

# Setup
library(tswge)
data(patemp)

# EDA
plotts.wge(patemp)
parzen.wge(patemp)

# Signal + Noise Model
fore.patemp.spn = fore.sigplusnoise.wge(patemp,freq = .083, max.p = 4, n.ahead = 24, linear = FALSE, lastn = TRUE)
ASE.spn = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.spn$f)^2)
ASE.spn

# ARUMA with s = 12 only
fore.patemp.B12 = fore.aruma.wge(patemp,s = 12, n.ahead = 24, lastn = TRUE)
ASE.B12 = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.B12$f)^2)
ASE.B12

# ARUMA with s = 12 and ARMA noise
patemp_B12 = artrans.wge(patemp, phi.tr = c(rep(0,11),1))
aic5.wge(patemp_B12)
patemp.est.AR2 = est.arma.wge(patemp,p = 2)
fore.patemp.B12.AR2 = fore.aruma.wge(patemp,s = 12, phi = patemp.est.AR2$phi, n.ahead = 24, lastn = TRUE)
ASE.B12.AR2 = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.B12.AR2$f)^2)
ASE.B12.AR2

# ARUMA with 2 Lambda terms and ARMA noise
y.tr=artrans.wge(patemp,phi.tr=c(1.732,-1))
patemp.est.lamda.AR3 = est.ar.wge(y.tr,p=3)
fore.patemp.L2.AR3 = fore.aruma.wge(patemp,lambda = c(1.732, -1), phi = patemp.est.lamda.AR3$phi, n.ahead = 24, lastn = TRUE)
ASE.L2.AR3 = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.L2.AR3$f)^2)
ASE.L2.AR3



