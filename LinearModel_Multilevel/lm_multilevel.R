# Estudo de regressão multinível
# Author: Victor G Alcantara
# Date: 03.12.21

# 0. Packages and Setup ======================================================

library(tidyverse)
library(readxl)
library(lme4) # for multilevel models

# 1. Openning data ===========================================================
# Diretório com as bases de dados que iremos trabalhar guardado no objeto 'wd'
wd <- "C:/Users/VictorGabriel/documents/00_dados/EDUCACAO/"
wd <- "E:/VGA/dados/"
setwd(wd)

load("mysamples_n10.RDS")

# 2. Data management ---------------------------------------------------------

# Calculate p : measure of proportion of variation in outcome variable that
# occurs between groups versus within groups

# variables
my_data <- mydata %>% group_by(ID_ESCOLA) %>% summarise(
 S_LP = var(PROFICIENCIA_LP_SAEB, na.rm = T),
 S_MT = var(PROFICIENCIA_MT_SAEB, na.rm = T),
 n = n(),
 me_LP = mean(PROFICIENCIA_LP_SAEB,na.rm=T),
 me_MT = mean(PROFICIENCIA_MT_SAEB,na.rm=T)
 )

# variables
C = nrow(my_data)   # n de clusters
N = nrow(mydata)    # n total de indivíduos
n = my_data$n       # n de indivíduos por cluster/tamanho do cluster
S_LP = my_data$S_LP # variância por cluster
me_LP_total = mean(mydata$PROFICIENCIA_LP_SAEB,na.rm=T)
me_MT_total = mean(mydata$PROFICIENCIA_MT_SAEB,na.rm=T)
me_LP_cluster = my_data$me_LP
me_MT_cluster = my_data$me_MT

# Variance within schools (clusters)
o_hat = sum( ( (n - 1)*S_LP) ) / (N - C)

# variables
n_tiu = (1/C) * (N - (sum(n^2)/N))
S_b = sum ( n * ( (me_LP_cluster - me_LP_total )^2) ) / (n_tiu*(C-1)) 

# variation between schools (clusters)
t_hat = S_b - (o_hat/n_tiu)

# measure of p
p = t_hat / (t_hat + o_hat)

# O resultado indica que há uma correlação alta entre a proficiência dos alunos
# entre escolas. Podemos interpretar este valor como a proporção da variação
# das proficiências que é explicada pelas escolas.

model1.0 <- lmer(data=data,PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA))
summary(model1.0)
t_hat_0 = 365.6   # variance between clusters
o_hat_0 = 1942.2  # variance within clusters

p_hat = t_hat_0 / (t_hat_0 + o_hat_0)

# We interpret this value to mean that the correlation of reading test scores
# among students within the same schools is approximately 0.16.

model1.1 <- lmer(data=data,PROFICIENCIA_LP_SAEB ~ CE + educMae + educPai + sexo + raca + trabFora + regiao + ID_LOCALIZACAO + fluxo + (1|ID_ESCOLA))
summary(model1.1)

t_hat_1 = 191.7   # variance between school
o_hat_1 = 1706.7  # variance within school

R2_lv1 = 1 - (o_hat_1 + t_hat_1) / (o_hat_0 + t_hat_0)

B = mean(n)

R2_lv2 = 1 - ( o_hat_1 / (B + t_hat_1) ) / (o_hat_0 / (B + t_hat_0))

# Finally, it is possible to estimate the proportion of variance in the outcome
# variable that is accounted for at each level of the model. In Chapter 1, we
# saw that with single-level OLS regression models, the proportion of response
# variable variance accounted for by the model is expressed as R2. In the context
# of multilevel modeling, R2 values can be estimated for each level of the
# model (Snijders and Bosker, 1999). For level 1, we can calculate: