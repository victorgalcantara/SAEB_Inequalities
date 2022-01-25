# Modelos lineares e multiníveis
## Author: Victor Alcantara (PPGSA/UFRJ) --- Date: 15.05.21
### Github: 
### Lattes:
### LinkedIn: 

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
library(pacman)
p_load("rio","tidyverse","stargazer","lme4","sjPlot")

# 1. Data imput ---------------------------------------------------------------

# Modelos

# Modelo linear
lm_LP_1 <- lm(PROFICIENCIA_LP_SAEB ~ educMae,data=data_07)
lm_LP_2 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE,data=data_07)
lm_LP_3 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca,data=data_07)
lm_LP_4 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo,data=data_07)
lm_LP_5 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora,data=data_07)
lm_LP_6 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao,data=data_07)
lm_LP_7 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao,data=data_07)
lm_LP_8 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo,data=data_07)
lm_LP_9 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin,data=data_07)
lm_LP_10 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_07)

stargazer(lm_LP_1,lm_LP_2,lm_LP_3,lm_LP_4,lm_LP_5,lm_LP_6,lm_LP_7,
          lm_LP_8,lm_LP_9,lm_LP_10,
          type="html",out=paste0("lm_LP_parciais_07.html"))

lm_MT_1 <- lm(PROFICIENCIA_MT_SAEB ~ educMae,data=data_07)
lm_MT_2 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE,data=data_07)
lm_MT_3 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca,data=data_07)
lm_MT_4 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo,data=data_07)
lm_MT_5 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora,data=data_07)
lm_MT_6 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao,data=data_07)
lm_MT_7 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao,data=data_07)
lm_MT_8 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo,data=data_07)
lm_MT_9 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin,data=data_07)
lm_MT_10 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_07)

stargazer(lm_MT_1,lm_MT_2,lm_MT_3,lm_MT_4,lm_MT_5,lm_MT_6,lm_MT_7,
          lm_MT_8,lm_MT_9,lm_MT_10,
          type="html",out=paste0("lm_MT_parciais_07.html"))

# ----- LP
# Modelo nulo apenas com o cluster nível 2 (escola)
model_LP_1.0 <- lmer(data=data_07,PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA))
tab_model(model_LP_1.0,file="mlm0_LP_07.html")

# Modelo com os níveis e sem variáveis entre os níveis acima do indivíduo
model_LP_1.1 <- lmer(data=data_07,
                     PROFICIENCIA_LP_SAEB ~ CE + educMae + sexo + raca + trabFora + fluxo + (1 + estruturaEscola + depAdmin |ID_ESCOLA),
)
tab_model(model_LP_1.1,file="mlm1_LP_07.html")

# ----- MT
# Modelo nulo apenas com o cluster nível 2 (escola)
model_MT_1.0 <- lmer(data=data_07,PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA))
tab_model(model_MT_1.0,file="mlm0_MT_07.html")

# Modelo com os níveis e sem variáveis entre os níveis acima do indivíduo
model_MT_1.1 <- lmer(data=data_07,
                     PROFICIENCIA_MT_SAEB ~ CE + educMae + sexo + raca + trabFora + fluxo + (1 + estruturaEscola + depAdmin |ID_ESCOLA),
)
tab_model(model_MT_1.1,file="mlm1_MT_07.html")

