# Title: Prova Brasil 2015
# Author: Victor Gabriel Alcantara
# Last update: 09.02.2022
# Github: https://github.com/victorgalcantar
# LinkedIn: https://www.linkedin.com/in/victorgalcantara/ 

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
library(pacman)
p_load("rio","tidyverse")

# 1. Import data --------------------------------------------------------------

# ----- Proficiência Prova Brasil/SAEB + Questionário socioeconômico
TS_ALUNO_9EF <- import("TS_ALUNO_9EF.csv")

# ----- Padrão resp do quest
names(TS_ALUNO_9EF)[36:92] <- paste0("Q",1:57)

# ----- Construção de indicadores

# Parâmetros para discriminação dos itens compositores do Capital Econômico
load("discr_itens.RDS")

# Indicador de Capital Econômico (CE)
data <- TS_ALUNO_9EF %>% 
  mutate(.,
         fluxo = ifelse(Q48 == "A" & Q49 == "A","Regular","Irregular"),
         
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
             Q45 == "A" ~ "B-Sim",
             Q45 == "B" ~ "A-Não"
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
           
           Q5 == "A" ~ 0,
           Q5 == "B" ~ 1,
           Q5 == "C" ~ 2,
           Q5 == "D" ~ 3,
           Q5 == "E" ~ 3
         ),
         carr = case_when(
           
           Q12 == "A" ~ 0,
           Q12 == "B" ~ 1,
           Q12 == "C" ~ 2,
           Q12 == "D" ~ 3,
           Q12 == "E" ~ 3
         ),
         comp = case_when(
           
           Q15 == "A" ~ 2,
           Q15 == "B" ~ 1,
           Q15 == "C" ~ 0
         ),
         banh = case_when(
           
           Q14 == "A" ~ 0,
           Q14 == "B" ~ 1,
           Q14 == "C" ~ 2,
           Q14 == "D" ~ 3,
           Q14 == "E" ~ 3
         ),
         quar = case_when(
           
           Q15 == "A" ~ 0,
           Q15 == "B" ~ 1,
           Q15 == "C" ~ 2,
           Q15 == "D" ~ 3,
           Q15 == "E" ~ 3
         ),
         empd = case_when(
           
           Q17 == "A" ~ 0,
           Q17 == "B" ~ 1,
           Q17 == "C" ~ 2,
           Q17 == "D" ~ 3,
           Q17 == "E" ~ 3
         ),
         pretensao = case_when(
           Q57 == "A" ~ "Só estudar",
           Q57 == "B" ~ "Só trabalhar",
           Q57 == "C" ~ "Estudar e trabalhar",
           Q57 == "D" ~ "NS",
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

# ----- Merge com dados das escolas
load("estruturaEscola_2015.RDS")

data_15 <- merge(data,escolas_9EF_15)

# Select variáveis
data_15 <- data_15 %>% select(ID_ALUNO,ID_TURMA,ID_ESCOLA,ID_TURNO,
                              ID_UF,ID_MUNICIPIO,ID_UF,
                              regiao,depAdmin,localizacao,
                              fluxo,sexo,raca,raca2,trabFora,educMae,tvco,carr,
                              comp,banh,quar,CE,educPais,
                              PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT_SAEB,
                              estruturaEscola,PC_FORMACAO_DOCENTE_FINAL,
                              MEDIA_9EF_MT,MEDIA_9EF_LP,INSE,INSE2)

# 2. Export data ---------------------------------------------------------------

save(data_15, file="data_11.RDS") # only students infos

load("estruturaEscola_2011.RDS")
data_15 <- merge(data_15,escolas_15) 

save(data_15,file="data_15_compl.RDS") # students and schools infos
