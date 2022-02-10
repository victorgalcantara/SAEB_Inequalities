# Title: Prova Brasil 2019
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
names(TS_ALUNO_9EF)[82:130] <- paste0("Q",1:49)

# ----- Construção de indicadores

# Parâmetros para discriminação dos itens compositores do Capital Econômico
load("discr_itens.RDS")

# Indicador de Capital Econômico (CE)
data <- TS_ALUNO_9EF %>% 
  mutate(.,
         fluxo = ifelse(Q39 == "A" & Q40 == "A","Regular","Irregular"),
         
         # sexo =
         #   case_when(
         #     Q1 == "A" ~ "A-Masculino",
         #     Q1 == "B" ~ "B-Feminino"
         #   ),
         
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
         
         # trabFora =
         #   case_when(
         #     Q45 == "A" ~ "B-Sim",
         #     Q45 == "B" ~ "A-Não"
         #   ),
         
         educMae = 
           case_when(
             Q9 == "A" ~ 2,
             Q9 == "B" ~ 4,
             Q9 == "C" ~ 8,
             Q9 == "D" ~ 12,
             Q9 == "E" ~ 16,
             Q9 == "F" ~ 99
           ),
         
         educPai = 
           case_when(
             Q8 == "A" ~ 2,
             Q8 == "B" ~ 4,
             Q8 == "C" ~ 8,
             Q8 == "D" ~ 12,
             Q8 == "E" ~ 16,
             Q8 == "F" ~ 99
           ),
         tvco = case_when(
           
           Q23 == "A" ~ 0,
           Q23 == "B" ~ 1,
           Q23 == "C" ~ 2,
           Q23 == "D" ~ 3,
         ),
         carr = case_when(
           
           Q25 == "A" ~ 0,
           Q25 == "B" ~ 1,
           Q25 == "C" ~ 2,
           Q25 == "D" ~ 3,
         ),
         comp = case_when(
           
           Q19 == "A" ~ 2,
           Q19 == "B" ~ 1,
           Q19 == "C" ~ 0
         ),
         banh = case_when(
           
           Q24 == "A" ~ 0,
           Q24 == "B" ~ 1,
           Q24 == "C" ~ 2,
           Q24 == "D" ~ 3,
         ),
         quar = case_when(
           
           Q22 == "A" ~ 0,
           Q22 == "B" ~ 1,
           Q22 == "C" ~ 2,
           Q22 == "D" ~ 3,
         ),
         # empd = case_when(
         #   
         #   Q19 == "A" ~ 0,
         #   Q19 == "B" ~ 1,
         #   Q19 == "C" ~ 2,
         #   Q19 == "D" ~ 3,
         #   Q19 == "E" ~ 3
         # )
         pretensao = case_when(
           Q49 == "A" ~ "Só estudar",
           Q49 == "B" ~ "Só trabalhar",
           Q49 == "C" ~ "Estudar e trabalhar",
           Q49 == "D" ~ "Não sei",
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

# Select variáveis
data_19_2 <- data %>% select(ID_ALUNO,ID_TURMA,ID_ESCOLA,ID_TURNO,
                              ID_UF,ID_MUNICIPIO,ID_UF,
                              regiao,depAdmin,localizacao,
                              fluxo,raca,raca2,educMae,tvco,carr,pretensao,
                              comp,banh,quar,CE,educPais,educPai,
                              PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT_SAEB,
                              PROFICIENCIA_CN_SAEB,PROFICIENCIA_CH_SAEB
                              )


# 2. Export data ---------------------------------------------------------------

save(data_19_2, file="data_19_2.RDS") # only students infos

load("estruturaEscola_2019.RDS")
data_19 <- merge(data,escolas_9EF_19)

save(data_19,file="data_19_compl.RDS") # students and schools infos
