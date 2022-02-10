# Title: Prova Brasil 2009
# Author: Victor Gabriel Alcantara
# Last update: 09.02.2022
# Github: https://github.com/victorgalcantar
# LinkedIn: https://www.linkedin.com/in/victorgalcantara/ 

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
install.packages("sjPlot")
library(pacman)
p_load("rio","tidyverse","stargazer","lme4","sjPlot")

# 1. Data imput ---------------------------------------------------------------
load("data_09.RDS")
load("estruturaEscola_2009.RDS")

data_09 <- merge(data_09,escolas_09)

# Modelos

# Modelo linear
lm_LP_1 <- lm(PROFICIENCIA_LP_SAEB ~ educMae,data=data_09)
lm_LP_2 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE,data=data_09)
lm_LP_3 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca,data=data_09)
lm_LP_4 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo,data=data_09)
lm_LP_5 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + localizacao,data=data_09)
lm_LP_6 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao,data=data_09)
lm_LP_7 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo,data=data_09)
lm_LP_8 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo + depAdmin,data=data_09)
lm_LP_9 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_09)

stargazer(lm_LP_1,lm_LP_2,lm_LP_3,lm_LP_4,lm_LP_5,lm_LP_6,lm_LP_7,
          lm_LP_8,lm_LP_9,
          type="html",out=paste0("lm_LP_parciais_09.html"))

lm_MT_1 <- lm(PROFICIENCIA_MT_SAEB ~ educMae,data=data_09)
lm_MT_2 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE,data=data_09)
lm_MT_3 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca,data=data_09)
lm_MT_4 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo,data=data_09)
lm_MT_5 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + localizacao,data=data_09)
lm_MT_6 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao,data=data_09)
lm_MT_7 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo,data=data_09)
lm_MT_8 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo + depAdmin,data=data_09)
lm_MT_9 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_09)

stargazer(lm_MT_1,lm_MT_2,lm_MT_3,lm_MT_4,lm_MT_5,lm_MT_6,lm_MT_7,
          lm_MT_8,lm_MT_9,
          type="html",out=paste0("lm_MT_parciais_09.html"))

# Modelo nulo apenas com o cluster nível 2 (escola)
model_LP_1.0 <- lmer(data=data_09,PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA))
tab_model(model_LP_1.0,file="mlm0_LP_09.html")

# Modelo com os níveis e sem variáveis entre os níveis acima do indivíduo
model_LP_1.1 <- lmer(data=data_09,
                     PROFICIENCIA_LP_SAEB ~ CE + educMae + sexo + raca + trabFora + fluxo + (1|ID_ESCOLA/ID_MUNICIPIO),
)
tab_model(model_LP_1.1,file="mlm0_LP_09.html")