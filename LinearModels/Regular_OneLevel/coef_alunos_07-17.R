# Efeitos por regressões múltiplas (alunos)

# Title
# Author: Victor Gabriel Alcantara
# Date: 
# Github:
# LinkedIn:

# 0. Packages and setup -----------------------------------------------------

library(tidyverse)
library(ggplot2)

wd <- "C:/Users/VictorGabriel/Dropbox/mestrado PPGSA_VictorAlcantara/resultados/regs"
setwd(wd)

# 1. Import databases --------------------------------------------------------
ano = seq(2007,2017,2)

regs_LP <- list("07"=NA,"09"=NA,"11"=NA,"13"=NA,"15"=NA,"17"=NA,"19"=NA)

regs_MT <- list("07"=NA,"09"=NA,"11"=NA,"13"=NA,"15"=NA,"17"=NA,"19"=NA)

for(i in 1:length(ano)){
  print(i)
  load(paste0("lm_LP_",ano[i],".RDS"))
  
  regs_LP[[i]] <- lm_LP_8
  
  load(paste0("lm_MT_",ano[i],".RDS"))
  
  regs_MT[[i]] <- lm_MT_8
  rm(lm_LP_8,lm_MT_8)
  gc()
}

# 2. Data management -------------------------------------------------------

coef_LP <- data.frame("Ano"=NA,"Intercepto"=NA,"Educ. Mae"=NA,
                      "CE"=NA,
                      "Pardos"=NA,"Pretos"=NA,"Amarelos"=NA,
                      "Indígenas"=NA,"Meninas"=NA,"Trab. Fora"=NA,
                      "Rural"=NA,"NE"=NA,"NO"=NA,
                      "SE"=NA,"SU"=NA,"Fluxo regular"=NA,"R2"=NA
                      )

R2_LP <- c(0.130,0.133,0.104,0.145,0.133,0.150)

for(i in 1:length(ano)){
coef_LP[i,2:16] <- regs_LP[[i]]$coefficients
coef_LP[i,1]    <- ano[i]
coef_LP[i,17]   <- R2_LP[i]
}

# MT

coef_MT <- data.frame("Ano"=NA,"Intercepto"=NA,"Educ. Mae"=NA,
                      "CE"=NA,
                      "Pardos"=NA,"Pretos"=NA,"Amarelos"=NA,
                      "Indígenas"=NA,"Meninas"=NA,"Trab. Fora"=NA,
                      "Rural"=NA,"NE"=NA,"NO"=NA,
                      "SE"=NA,"SU"=NA,"Fluxo regular"=NA,"R2"=NA
)

R2_MT <- c(0.152,0.156,0.080,0.126,0.122,0.154)


for(i in 1:length(ano)){
  coef_MT[i,2:16] <- regs_MT[[i]]$coefficients
  coef_MT[i,1]    <- ano[i]
  coef_MT[i,17]   <- R2_MT[i]
  
}

coef_LP[,2:16] = round(coef_LP[,2:16],2)
coef_MT[,2:16] = round(coef_MT[,2:16],2)

write.csv(coef_LP,file="coef_LP_07-17.csv")
write.csv(coef_MT,file="coef_MT_07-17.csv")

# LP 
n = c(26918,31949,31599,32634,29907,32520)
sigma = c(1716.46,1879.79,1855.25,1974.27,1991.19,1939.58)
tau   = c(253.49,343.26,350.62,408.69,349.09,464.92)
ICC   = c(0.13,0.15,0.16,0.17,0.15,0.19)

coef_mlm_LP <- data.frame("ano"=ano,"n"=n,"sigma"=sigma,"tau"=tau,"ICC"=ICC)

# Modelos neutros

# MT
n = c(26918,31950,31599,32634,29907,32520)
sigma = c(1609.55,1695.20,1796.41,1833.74,1650.31,1866.11)
tau   = c(325.49,386.53,448.70,487.33,326.26,552.84)
ICC   = c(0.17,0.19,0.20,0.21,0.17,0.23)

coef_mlm_MT <- data.frame("ano"=ano,"n"=n,"sigma"=sigma,"tau"=tau,"ICC"=ICC)

coef_mlm_LP[,3:5] <- round(coef_mlm_LP[,3:5],2)
coef_mlm_MT[,3:5] <- round(coef_mlm_MT[,3:5],2)

write.csv(coef_mlm_LP,file="coef_mlm_LP_07-17.csv")
write.csv(coef_mlm_MT,file="coef_mlm_MT_07-17.csv")

# Modelos completos

# LP
intercept <- c(210.03,220.47,219.79,213.52,222.28,226.68)
CE <- c(1.30,1.23,2.44,0.77,1.25,2.31)
educMae <- c(0.94,0.88,1.51,1.13,1.06,1.11)
meninas <- c(10.68,12.29,16.98,13.10,11.23,8.44)
pardos <- c(-4.69,-4.85,-7.07,-4.94,-5.04,-4.93)
pretos <- c(-8.04,-9.09,-12.28,-9.47,-9.05,-9.98)
amarelos <- c(-2.38,-2.68,-4.18,-2.93,-2.48,-4.31)
indigenas <- c(-4.95,-6.11,-9.11,-6.10,-4.30,-6.29)
trabFora <- c(-6.66,-7.44,-6.90,-8.26,-9.11,-9.20)
fluxoIrregular <- c(-17.82,-17.98,4.24,-21.36,-20.37,-21.97)
sigma <- c(1571.09,1722.77,1765.50,1751.01,1740.93,1694.59)
tauEscola <- c(1659.92,1291.56,1102.35,797.85,922.37,1738.54)
tauEE <- c(23.66,61.48,56.58,48.53,51.59,53.85)
tauDaE <- c(2158.48,1375.99,2731.93,1199.73,1306.18,1018.40)
tauDaM <- c(2214.83,1593.94,1939.88,1359.16,1315.26,1860.44)
ICC <- c(0.51,0.43,0.38,0.31,0.35,0.51)
nEsc <- c(24624,22400,27971,28764,27680,31302)
nAlu <- c(1214854,1009843,659845,1114267,982541,845066)

coef_mlm1_LP <- data.frame("ano"=ano,"intercept"=intercept,"CE"=CE,"educMae"=educMae,
                           "meninas"=meninas,"pardos"=pardos,
                           "pretos"=pretos,"amarelos"=amarelos,
                           "indigenas"=indigenas,"trabFora"=trabFora,
                           "fluxoIrregular"=fluxoIrregular,
                           "sigma"=sigma,"tauEscola"=tauEscola,
                           "tauEE"=tauEE,"tauDaE"=tauDaE,
                           "tauDaM"=tauDaM,"ICC"=ICC,"nEsc"=nEsc,
                           "nAlu"=nAlu)

# MT
intercept <- c(231.02,234.21,237.39,226.71,236.09,233.13)
CE <- c(2.76,2.54,3.46,2.17,3.03,4.04)
educMae <- c(0.79,0.77,1.35,1.00,0.87,1.00)
meninas <- c(-10.36,-11.04,-4.43,-7.34,-11.26,-12.80)
pardos <- c(-4.76,-5.00,-6.35,-4.07,-4.30,-3.94)
pretos <- c(-9.34,-9.87,-12.77,-9.20,-8.87,-10.06)
amarelos <- c(-1.78,-2.38,-3.43,-2.25,-1.84,-3.67)
indigenas <- c(-6.11,-7.21,-9.18,-6.63,-5.36,-6.99)
trabFora <- c(-4.52,-4.04,-8.06,-4.34,-3.92,-4.81)
fluxoIrregular <- c(-19.05,-19.01,-2.93,-21.90,-18.77,-22.60)
sigma <- c(1489.69,1613.80,1709.00,1667.52,1515.11,1653.24)
tauEscola <- c(3251.98,3029.51,1824.45,1835.70,1929.25,2685.84)
tauEE <- c(28.78,68.97,86.99,70.27,56.82,73.80)
tauDaE <- c(5587.14,3479.59,3117.12,3043.24,2599.33,2398.15)
tauDaM <- c(3806.06,3291.53,3468.47,2875.30,2463.22,2281.08)
ICC <- c(0.69,0.65,0.52,0.52,0.56,0.62)
nEsc <- c(24624,22402,27971,28764,27680,31302)
nAlu <- c(1214854,1009787,659845,1114267,982541,845066)

coef_mlm1_MT <- data.frame("ano"=ano,"intercept"=intercept,"CE"=CE,"educMae"=educMae,
                           "meninas"=meninas,"pardos"=pardos,
                           "pretos"=pretos,"amarelos"=amarelos,
                           "indigenas"=indigenas,"trabFora"=trabFora,
                           "fluxoIrregular"=fluxoIrregular,
                           "sigma"=sigma,"tauEscola"=tauEscola,
                           "tauEE"=tauEE,"tauDaE"=tauDaE,
                           "tauDaM"=tauDaM,"ICC"=ICC,"nEsc"=nEsc,
                           "nAlu"=nAlu)

coef_mlm1_LP[,2:16] <- round(coef_mlm1_LP[,2:16],2)
coef_mlm1_MT[,2:16] <- round(coef_mlm1_MT[,2:16],2)

write.csv(coef_mlm1_LP,file="coef_mlm1_LP_07-17.csv")
write.csv(coef_mlm1_MT,file="coef_mlm1_MT_07-17.csv")
