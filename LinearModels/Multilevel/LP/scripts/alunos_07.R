# Dados alunos 2007
## Author: Victor Alcantara (PPGSA/UFRJ) --- Date: 15.05.21
### Github: 
### Lattes:
### LinkedIn: 

# 0. Packages and Setup -------------------------------------------------------
install.packages("pacman")
library(pacman)
p_load("rio","tidyverse","gdata")

# 1. Import data --------------------------------------------------------------

# ----- Proficiência Prova Brasil/SAEB
TamanhosVar <- c(8, 7, 5, 5, 4, 1, 8, 1, 1, 2, 2, 
                 50, 7, 1, 1, 8, 8, 8, 8, 8, 8, 8, 8)
FilePath <- "TS_ALUNO.TXT"
ListaVariaveis <- c("ID_ALUNO", "ID_TURMA", "TX_HORARIO_INICIO", "TX_HORARIO_FINAL", "NU_QTD_ALUNO", "ID_SERIE",
                    "PK_COD_ENTIDADE", "ID_DEPENDENCIA_ADM", "ID_LOCALIZACAO", "SIGLA_UF", "COD_UF", "NO_MUNICIPIO", "COD_MUNICIPIO",
                    "ST_LINGUA_PORTUGUESA", "ST_MATEMATICA", "NU_THETA_L", "NU_SETHETA_L", "NU_THETAT_L", "NU_SETHETAT_L", "NU_SETHETA_M",
                    "NU_THETAT_M", "NU_SETHETAT_M", "NU_THETA_M")
TS_ALUNO <- read.fwf(FilePath, TamanhosVar, skip = 0, col.names = ListaVariaveis)


names(TS_ALUNO) <- c("ID_ALUNO", "ID_TURMA", "TX_HORARIO_INICIO", "TX_HORARIO_FINAL",
                     "NU_QTD_ALUNO","ID_SERIE",
                     "ID_ESCOLA", "ID_DEPENDENCIA_ADM", "ID_LOCALIZACAO", 
                     "SIGLA_UF", "ID_UF", "NO_MUNICIPIO", "ID_MUNICIPIO",
                     "PREENCHIMENTO_LP", "PREENCHIMENTO_MT", 
                     "NU_THETA_L", "NU_SETHETA_L", 
                     "PROFICIENCIA_LP_SAEB", "DP_LP_SAEB", 
                     "NU_SETHETA_M",
                     "PROFICIENCIA_MT_SAEB", "DP_MT_SAEB",
                     "NU_THETA_M"
)

TS_ALUNO_9EF <- TS_ALUNO %>% filter(ID_SERIE == 8)

# ----- Questionários socioeconômicos
TamanhosVar <- c(8, 1, 1, 1, 2, 2, 50, 7, rep(1,47))
FilePath <- "TS_QUEST_ALUNO.TXT"
ListaVariaveis <- c("ID_ALUNO", "ID_SERIE", "ID_DEPENDENCIA_ADM", 
                    "ID_LOCALIZACAO", "SIGLA_UF", "ID_UF", "NO_MUNICIPIO", "ID_MUNICIPIO",
                    paste0("Q",1:47))

TS_QUEST_ALUNO <- read.fwf(FilePath, TamanhosVar, skip = 0, col.names = ListaVariaveis)

TS_QUEST_ALUNO_9EF <- TS_QUEST_ALUNO %>% filter(ID_SERIE == 8)

# ----- Unindo Resultados+Questionários
data_07 <- merge(TS_ALUNO_9EF,TS_QUEST_ALUNO_9EF)

# ----- Construção de indicadores

# Parâmetros para discriminação dos itens compositores do Capital Econômico
load("discr_itens.RDS")

# Indicador de Capital Econômico (CE)
data_07 <- data_07 %>% 
  replace(.,. == "." | . == "*",NA)

data <- data_07 %>% 
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
             Q35 == "A" ~ "B-Sim",
             Q35 == "B" ~ "A-Não"
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
         ),
         pretensao = case_when(
           Q47 == "A" ~ "Só estudar",
           Q47 == "B" ~ "Só trabalhar",
           Q47 == "C" ~ "Estudar e trabalhar",
           Q47 == "D" ~ "Não sabe",
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
                          ID_UF == 11 ~ "NO",
                          ID_UF == 12 ~ "NO",
                          ID_UF == 13 ~ "NO",
                          ID_UF == 14 ~ "NO",
                          ID_UF == 15 ~ "NO",
                          ID_UF == 16 ~ "NO",
                          ID_UF == 17 ~ "NO",
                          ID_UF == 21 ~ "NE",
                          ID_UF == 22 ~ "NE",
                          ID_UF == 23 ~ "NE",
                          ID_UF == 24 ~ "NE",
                          ID_UF == 25 ~ "NE",
                          ID_UF == 26 ~ "NE",
                          ID_UF == 27 ~ "NE",
                          ID_UF == 28 ~ "NE",
                          ID_UF == 29 ~ "NE",
                          ID_UF == 31 ~ "SE",
                          ID_UF == 32 ~ "SE",
                          ID_UF == 33 ~ "SE",
                          ID_UF == 34 ~ "SE",
                          ID_UF == 41 ~ "SU",
                          ID_UF == 42 ~ "SU",
                          ID_UF == 43 ~ "SU",
                          ID_UF == 50 ~ "CO",
                          ID_UF == 51 ~ "CO",
                          ID_UF == 52 ~ "CO",
                          ID_UF == 53 ~ "CO"
                        ),
                        localizacao = case_when(
                          ID_LOCALIZACAO == 2 ~ "B - rural",
                          ID_LOCALIZACAO == 1 ~ "A - urbana"),
                        depAdmin = case_when(
                          ID_DEPENDENCIA_ADM == 1  ~ "A - Federal",
                          ID_DEPENDENCIA_ADM == 2  ~ "B - Estadual",
                          ID_DEPENDENCIA_ADM == 3  ~ "C - Municipal")
)

data_07 <- data %>% select(ID_ALUNO,ID_TURMA,ID_ESCOLA,
                           TX_HORARIO_INICIO,TX_HORARIO_FINAL,
                           NU_QTD_ALUNO,
                           ID_UF,ID_MUNICIPIO,SIGLA_UF,NO_MUNICIPIO,
                           PREENCHIMENTO_LP,PREENCHIMENTO_MT,
                           regiao,depAdmin,localizacao,
                           fluxo,sexo,raca,raca2,trabFora,educMae,tvco,carr,
                           comp,banh,quar,pretensao,CE,educPais,
                           PROFICIENCIA_LP_SAEB,PROFICIENCIA_MT_SAEB)

save(data_07, file="data_07.RDS")

load("estruturaEscola_2007.RDS")

data_07 <- merge(data_07,escolas_07)
save(data_07,file="data_07_compl.RDS")
