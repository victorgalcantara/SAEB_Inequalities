# Title: Prova Brasil 2011
# Author: Victor Gabriel Alcantara
# Last update: 09.02.2022
# Github: https://github.com/victorgalcantar
# LinkedIn: https://www.linkedin.com/in/victorgalcantara/

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
library(pacman)
p_load("rio","tidyverse")

# 1. Import data --------------------------------------------------------------

# ----- Proficiência Prova Brasil/SAEB
TS_ALUNO <- import("TS_RESULTADO_ALUNO.csv")

TS_ALUNO_9EF <- TS_ALUNO %>% filter(ID_SERIE == 9)

# ----- Questionários socioeconômicos
TS_QUEST_ALUNO <- import("TS_QUEST_ALUNO.csv")
TS_QUEST_ALUNO_9EF <- TS_QUEST_ALUNO %>% filter(ID_SERIE == 9)

# ----- Unindo Resultados+Questionários
data_11 <- merge(TS_ALUNO_9EF,TS_QUEST_ALUNO_9EF)

# ----- Padrão resp do quest
names(data_11)[26:87] <- paste0("Q",1:62)

# ----- Construção de indicadores

# Parâmetros para discriminação dos itens compositores do Capital Econômico
load("discr_itens.RDS")

# Indicador de Capital Econômico (CE)
data <- data_11 %>% mutate(
  PROFICIENCIA_LP_SAEB = as.numeric(gsub(",",".",.$PROFICIENCIA_LP_SAEB)),
  PROFICIENCIA_MT_SAEB = as.numeric(gsub(",",".",.$PROFICIENCIA_MT_SAEB))
)
  
data <- data %>% 
  mutate(.,
         fluxo = ifelse(Q38 == "A" & Q39 == "A","Regular","Irregular"),
         
         sexo =
           case_when(
             Q1 == "A" ~ "A-Masculino",
             Q1 == "B" ~ "B-Feminino"
           ),
         
         raca = case_when(
           Q2 == "A" ~ "A - branca",
           Q2 == "B" ~ "B - parda",
           Q2 == "C" ~ "C - preta",
           Q2 == "D" ~ "D - amarela",
           Q2 == "E" ~ "E - indigena"
         ),
         raca2 = case_when(
           Q2 == "A" ~ "Brancos",
           Q2 == "B" ~ "Não brancos",
           Q2 == "C" ~ "Não brancos",
           Q2 == "D" ~ "Brancos",
           Q2 == "E" ~ "Não brancos"
         ),
         
         trabFora =
           case_when(
             Q40 == "A" ~ "B-Sim",
             Q40 == "B" ~ "A-Não"
           ),
         
         educMae = 
           case_when(
             Q19 == "A" ~ 2,
             Q19 == "B" ~ 4,
             Q19 == "C" ~ 8,
             Q19 == "D" ~ 12,
             Q19 == "E" ~ 16,
             Q19 == "F" ~ 99
           ),
         
         educPai = 
           case_when(
             Q23 == "A" ~ 2,
             Q23 == "B" ~ 4,
             Q23 == "C" ~ 8,
             Q23 == "D" ~ 12,
             Q23 == "E" ~ 16,
             Q23 == "F" ~ 99
           ),
         tvco = case_when(
           
           Q5 == "A" ~ 1,
           Q5 == "B" ~ 2,
           Q5 == "C" ~ 3,
           Q5 == "D" ~ 0,
         ),
         carr = case_when(
           
           Q12 == "A" ~ 1,
           Q12 == "B" ~ 2,
           Q12 == "C" ~ 3,
           Q12 == "D" ~ 0
         ),
         comp = case_when(
           
           Q13 == "A" ~ 2,
           Q13 == "B" ~ 1,
           Q13 == "C" ~ 0
         ),
         banh = case_when(
           
           Q14 == "A" ~ 1,
           Q14 == "B" ~ 2,
           Q14 == "C" ~ 3,
           Q14 == "D" ~ 3,
           Q14 == "E" ~ 0
         ),
         quar = case_when(
           
           Q16 == "A" ~ 1,
           Q16 == "B" ~ 2,
           Q16 == "C" ~ 3,
           Q16 == "D" ~ 3,
           Q16 == "E" ~ 0
         ),
         empd = case_when(
           
           Q15 == "A" ~ 1,
           Q15 == "B" ~ 2,
           Q15 == "C" ~ 3,
           Q15 == "D" ~ 0
         ) 
  ) %>% mutate(.,
               educMae = ifelse(educMae == 99,NA,educMae),
               educPai = ifelse(educPai == 99,NA,educPai),
               CE =
                 (   
                     tvco  * discr.itens[1,"discr"]  +
                     carr  * discr.itens[2,"discr"]  +
                     banh  * discr.itens[4,"discr"]  +
                     quar  * discr.itens[5,"discr"] ) / sum(discr.itens[c(1,2,4,5),1]
                     ),
               educPais = (educPai+educMae) / 2
  )

data <- data %>% mutate(.,
                        regiao   = case_when(
                          ID_REGIAO == 1 ~ "NO",
                          ID_REGIAO == 2 ~ "NE",
                          ID_REGIAO == 3 ~ "SE",
                          ID_REGIAO == 4 ~ "SU",
                          ID_REGIAO == 5 ~ "CO"
                        ),
                        localizacao = case_when(
                          ID_LOCALIZACAO == 2 ~ "B - rural",
                          ID_LOCALIZACAO == 1 ~ "A - urbana"),
                        depAdmin = case_when(
                          ID_DEPENDENCIA_ADM == 1  ~ "A - Federal",
                          ID_DEPENDENCIA_ADM == 2  ~ "B - Estadual",
                          ID_DEPENDENCIA_ADM == 3  ~ "C - Municipal",
                          ID_DEPENDENCIA_ADM == 4  ~ "D - Privada"
                          )
)

data_11 <- data %>% select(ID_ALUNO,ID_TURMA,ID_ESCOLA,ID_TURNO,
                           ID_UF,ID_MUNICIPIO,ID_UF,
                           IN_PREENCHIMENTO,
                           regiao,depAdmin,localizacao,
                           fluxo,sexo,raca,raca2,trabFora,educMae,tvco,carr,
                           comp,banh,quar,CE,educPais,
                           PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT_SAEB)

# 2. Export data ---------------------------------------------------------------

save(data_11, file="data_11.RDS") # only students infos

load("estruturaEscola_2011.RDS")
data_11 <- merge(data_11,escolas_11) 

save(data_11,file="data_11_compl.RDS") # students and schools infos
