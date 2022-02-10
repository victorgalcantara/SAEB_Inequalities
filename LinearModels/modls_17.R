# Title: Prova Brasil 2011
# Author: Victor Gabriel Alcantara
# Last update: 09.02.2022
# Github: https://github.com/victorgalcantar
# LinkedIn: https://www.linkedin.com/in/victorgalcantara/

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
install.packages("sjPlot")
library(pacman)
p_load("rio","tidyverse","stargazer","lme4","sjPlot","lme4","corrplot")

# 1. Data imput ---------------------------------------------------------------
load("data_17.RDS")

# Modelos

# Modelo linear
lm_LP_1 <- lm(PROFICIENCIA_LP_SAEB ~ educMae,data=data_17)
lm_LP_2 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE,data=data_17)
lm_LP_3 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca,data=data_17)
lm_LP_4 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo,data=data_17)
lm_LP_5 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora,data=data_17)
lm_LP_6 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao,data=data_17)
lm_LP_7 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao,data=data_17)
lm_LP_8 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo,data=data_17)
lm_LP_9 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin,data=data_17)
lm_LP_10 <- lm(PROFICIENCIA_LP_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_17)

stargazer(lm_LP_1,lm_LP_2,lm_LP_3,lm_LP_4,lm_LP_5,lm_LP_6,lm_LP_7,
          lm_LP_8,lm_LP_9,lm_LP_10,
          type="html",out=paste0("lm_LP_parciais_17.html"))

lm_MT_1 <- lm(PROFICIENCIA_MT_SAEB ~ educMae,data=data_17)
lm_MT_2 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE,data=data_17)
lm_MT_3 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca,data=data_17)
lm_MT_4 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo,data=data_17)
lm_MT_5 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora,data=data_17)
lm_MT_6 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao,data=data_17)
lm_MT_7 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao,data=data_17)
lm_MT_8 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo,data=data_17)
lm_MT_9 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin,data=data_17)
lm_MT_10 <- lm(PROFICIENCIA_MT_SAEB ~ educMae + CE + raca + sexo + trabFora + localizacao + regiao + fluxo + depAdmin + estruturaEscola,data=data_17)

stargazer(lm_MT_1,lm_MT_2,lm_MT_3,lm_MT_4,lm_MT_5,lm_MT_6,lm_MT_7,
          lm_MT_8,lm_MT_9,lm_MT_10,
          type="html",out=paste0("lm_MT_parciais_17.html"))

# ----- LP
# Modelo nulo apenas com o cluster nível 2 (escola)
model_LP_1.0 <- lmer(data=data_17,PROFICIENCIA_LP_SAEB ~ 1 + (1|ID_ESCOLA))
tab_model(model_LP_1.0,file="mlm0_LP_17.html")

# Modelo com os níveis e sem variáveis entre os níveis acima do indivíduo
model_LP_1.1 <- lmer(data=data_17,
                     PROFICIENCIA_LP_SAEB ~ CE + educMae + sexo + raca + trabFora + fluxo + (1 + estruturaEscola + depAdmin |ID_ESCOLA),
)
tab_model(model_LP_1.1,file="mlm1_LP_17.html")

# ----- MT
# Modelo nulo apenas com o cluster nível 2 (escola)
model_MT_1.0 <- lmer(data=data_17,PROFICIENCIA_MT_SAEB ~ 1 + (1|ID_ESCOLA))
tab_model(model_MT_1.0,file="mlm0_MT_17.html")

# Modelo com os níveis e sem variáveis entre os níveis acima do indivíduo
model_MT_1.1 <- lmer(data=data_17,
                     PROFICIENCIA_MT_SAEB ~ CE + educMae + sexo + raca + trabFora + fluxo + (1 + estruturaEscola + depAdmin |ID_ESCOLA),
)
tab_model(model_MT_1.1,file="mlm1_MT_17.html")

# ----- Matriz de correlações

idhm <- read_xlsx("ATLAS_2013.xlsx",sheet = 2) %>% 
  select(ANO,IDHM,Codmun7) %>% filter(ANO == 2010) %>% rename(ID_MUNICIPIO = Codmun7)

data_muni <- data_17 %>% group_by(ID_MUNICIPIO) %>% summarise(.,
                                                           me_ce_mun = mean(CE,na.rm=T),
                                                           me_LP_mun = mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                                                           me_MT_mun = mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                                                           me_ee_mun = mean(estruturaEscola,na.rm=T)
)

data_17 <- merge(data_17,data_muni)
data_17 <- merge(data_17,idhm)

my_matrix <- data_17 %>% rename(.,
                             "Hab.\n Aluno \n LP"          = PROFICIENCIA_LP_SAEB,
                             "Hab.\n Aluno \n MT"          = PROFICIENCIA_MT_SAEB,
                             "Média \n Escola \n LP"       = MEDIA_9EF_LP,
                             "Média \n Escola \n MT"       = MEDIA_9EF_MT,
                             "Docentes \n com ES \n %"     = PC_FORMACAO_DOCENTE_FINAL,
                             "Estrutura \n escola"         = estruturaEscola,
                             "Capital \n Econômico"        = CE,
                             "Educ. \n Pai"                = educPai,
                             "Educ. \n Mãe"                = educMae,
                             "Média \n CE \n muni"         = me_ce_mun,
                             "Média \n LP \n muni"         = me_LP_mun,
                             "Média \n MT \n muni"         = me_MT_mun,
) %>% 
  select("Hab.\n Aluno \n LP" ,"Hab.\n Aluno \n MT","Média \n CE \n muni",
         "Média \n Escola \n LP","Média \n Escola \n MT",
         "Docentes \n com ES \n %","Estrutura \n escola",
         "Capital \n Econômico",
         "Educ. \n Pai","Educ. \n Mãe",IDHM) %>% 
  na.exclude() %>% 
  as.matrix()

correlation <- cor(my_matrix)

png(file="corr_saeb17.png", res=300, width=4500, height=4500)
corrplot.mixed(correlation,
               order="hclust",
               upper = "ellipse",
               lower.col = "black",number.cex=0.7,tl.cex=0.8,tl.col="black",
               outline=T)
dev.off()

save(correlation,file="corr_17.RDS")
